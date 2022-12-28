#  Initialize the fixedHuffmanDecoder only once upon first use.
#  max length of Huffman code
const maxCodeLen = 16
#  The next three numbers come from the RFC section 3.2.7, with the
#  additional proviso in section 3.2.5 which implies that distance codes
#  30 and 31 should never occur in compressed data.
const maxNumLit = 286
const maxNumDist = 30
#  number of codes in Huffman meta-code
const numCodes = 19

# The number of extra bits needed by length code X - LENGTH_CODES_START.
# The largest offset code.
const offsetCodeCount = 30
# The special code used to mark the end of a block.
const endBlockMarker = 256
# The first length code.
const lengthCodesStart = 257
# The number of codegen codes.
const codegenCodeCount = 19
const badCode = 255
# bufferFlushSize indicates the buffer size
# after which bytes are flushed to the writer.
# Should preferably be a multiple of 6, since
# we accumulate 6 bytes between writes to the buffer.
const bufferFlushSize = 240
# bufferSize is the actual output byte buffer size.
# It must have additional headroom for a flush
# which can contain up to 8 bytes.
const bufferSize = bufferFlushSize + 8
# The length indicated by length code X - LENGTH_CODES_START.
const lengthExtraBits = Go.Slice(Int8[
    0, 0, 0,
    0, 0, 0, 0, 0, 1, 1, 1, 1, 2,
    2, 2, 2, 3, 3, 3, 3, 4, 4, 4,
    4, 5, 5, 5, 5, 0,
])

# offset code word extra bits.
const lengthBase = Go.Slice(UInt32[
    0, 1, 2, 3, 4, 5, 6, 7, 8, 10,
    12, 14, 16, 20, 24, 28, 32, 40, 48, 56,
    64, 80, 96, 112, 128, 160, 192, 224, 255,
])

const offsetExtraBits = Go.Slice(Int8[
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3,
    4, 4, 5, 5, 6, 6, 7, 7, 8, 8,
    9, 9, 10, 10, 11, 11, 12, 12, 13, 13,
])

const offsetBase = Go.Slice(UInt32[
    0x000000, 0x000001, 0x000002, 0x000003, 0x000004,
    0x000006, 0x000008, 0x00000c, 0x000010, 0x000018,
    0x000020, 0x000030, 0x000040, 0x000060, 0x000080,
    0x0000c0, 0x000100, 0x000180, 0x000200, 0x000300,
    0x000400, 0x000600, 0x000800, 0x000c00, 0x001000,
    0x001800, 0x002000, 0x003000, 0x004000, 0x006000,
])

const codegenOrder = Go.Slice(UInt32[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15])

const NoCompression = 0
const BestSpeed = 1
const BestCompression = 9
const DefaultCompression = -1

# HuffmanOnly disables Lempel-Ziv match searching and only performs Huffman
# entropy encoding. This mode is useful in compressing data that has
# already been compressed with an LZ style algorithm (e.g. Snappy or LZ4)
# that lacks an entropy encoder. Compression gains are achieved when
# certain bytes in the input stream occur more frequently than others.
# 
# Note that HuffmanOnly produces a compressed output that is
# RFC 1951 compliant. That is, any valid DEFLATE Decompressor will
# continue to be able to decompress this output.
const HuffmanOnly = -2

const logWindowSize = 15
const windowSize = 1 << logWindowSize
const windowMask = windowSize - 1

# The LZ77 step produces a sequence of literal tokens and <length, offset>
# pair tokens. The offset is also known as distance. The underlying wire
# format limits the range of lengths and offsets. For example, there are
# 256 legitimate lengths: those in the range [3, 258]. This package's
# Compressor uses a higher minimum match length, enabling optimizations
# such as finding matches via 32-bit loads and compares.
# The smallest match length per the RFC section 3.2.5
const baseMatchLength = 3
# The smallest match length that the Compressor actually emits
const minMatchLength = 4
# The largest match length
const maxMatchLength = 258
# The smallest match offset
const baseMatchOffset = 1
# The largest match offset
const maxMatchOffset = 1 << 15

# The maximum number of tokens we put into a single flate block, just to
# stop things from getting too large.
const maxFlateBlockTokens = 1 << 14
const maxStoreBlockSize = 65535
# After 17 performance degrades
const hashBits = 17
const hashSize = 1 << hashBits
const hashMask = (1 << hashBits) - 1
const maxHashOffset = 1 << 24

const skipNever = typemax(Int32)

#  For codes smaller than the table width, there are multiple entries
#  (each combination of trailing bits has the same value). For codes
#  larger than the table width, the table contains a link to an overflow
#  table. The width of each entry in the link table is the maximum code
#  size minus the chunk width.
# 
#  Note that you can do a lookup in the table even without all bits
#  filled. Since the extra bits are zero, and the DEFLATE Huffman codes
#  have the property that shorter codes come before longer ones, the
#  bit length estimate in the result is a lower bound on the actual
#  number of bits.
# 
#  See the following:
# 	https://github.com/madler/zlib/raw/master/doc/algorithm.txt
#  chunk & 15 is number of bits
#  chunk >> 4 is value, including table link
const huffmanChunkBits = 9
const huffmanNumChunks = 1 << huffmanChunkBits
const huffmanCountMask = 15
const huffmanValueShift = 4

#  based on Snappy's LZ77-style encoder: github.com/golang/snappy
#  Bits used in the table.
const tableBits = 14
#  Size of the table.
const tableSize = 1 << tableBits
#  Mask for table indices. Redundant, but can eliminate bounds checks.
const tableMask = tableSize - 1
#  Right-shift to get the tableBits most significant bits of a uint32.
const tableShift = 32 - tableBits
#  Reset the buffer offset when reaching this.
#  Offsets are stored between blocks as int32 values.
#  Since the offset we are checking against is at the beginning
#  of the buffer, we need to subtract the current and input
#  buffer to not risk overflowing the int32.
const bufferReset = typemax(Int32) - maxStoreBlockSize * 2

function load32(b::Go.Slice{UInt8}, i::Integer)# ::UInt32
    #  Help the compiler eliminate bounds checks on the next line.
    return UInt32(b[i+1]) | UInt32(b[i+2]) << 8 | UInt32(b[i+3]) << 16 | UInt32(b[i+4]) << 24
end

function load64(b::Go.Slice{UInt8}, i::Integer)# ::UInt64
    #  Help the compiler eliminate bounds checks on the next line.
    return UInt64(b[i + 1]) |
           UInt64(b[i + 2]) << 8 |
           UInt64(b[i + 3]) << 16 |
           UInt64(b[i + 4]) << 24 |
           UInt64(b[i + 5]) << 32 |
           UInt64(b[i + 6]) << 40 |
           UInt64(b[i + 7]) << 48 |
           UInt64(b[i + 8]) << 56
end

function hash(u::UInt32)# ::UInt32
    return (u * 0x1e35a7bd) >> tableShift
end

#  These constants are defined by the Snappy implementation so that its
#  assembly implementation can fast-path some 16-bytes-at-a-time copies. They
#  aren't necessary in the pure Go implementation, as we don't use those same
#  optimizations, but using the same thresholds doesn't really hurt.
const inputMargin = 16 - 1
const minNonLiteralBlockSize = 1 + 1 + inputMargin
