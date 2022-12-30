#  A CorruptInputError reports the presence of corrupt input at a given offset.
struct CorruptInputError
    int64::Int64
end

#  An InternalError reports an error in the flate code itself.
struct InternalError
    String::String
end

#  Initialize Huffman decoding tables from array of code lengths.
#  Following this function, h is guaranteed to be initialized into a complete
mutable struct HuffmanDecoder
    min::Int
    chunks::Go.Array{UInt32}
    links::Go.Slice{Go.Slice{UInt32}}
    linkMask::UInt32
end

HuffmanDecoder() = HuffmanDecoder(0, Go.Array(UInt32, huffmanNumChunks), Go.Slice(Go.Slice{UInt32}, 0), 0)
function Base.empty!(h::HuffmanDecoder)
    h.min = 0
    h.chunks = Go.Array(UInt32, huffmanNumChunks)
    h.links = Go.Slice(Go.Slice{UInt32}, 0)
    h.linkMask = 0
    return
end

#  tree (i.e., neither over-subscribed nor under-subscribed). The exception is a
#  degenerate case where the tree has only a single symbol with length 1. Empty
#  trees are permitted.
function init(h::HuffmanDecoder, lengths::Go.Slice{Int})# ::Bool
    #  Sanity enables additional runtime tests during Huffman
    #  table construction. It's intended to be used during
    #  development to supplement the currently ad-hoc unit tests.
    sanity = false
    if h.min != 0
        empty!(h)
    end

    #  Count number of codes of each length,
    #  compute min and max length.
    count = Go.Array(Int, maxCodeLen)
    min::Int = 0
    max::Int = 0
    for n in lengths
        if n == 0
            continue
        end
        if min == 0 || n < min
            min = n
        end
        if n > max
            max = n
        end
        count[n] += 1
    end

    #  Empty tree. The Decompressor.huffSym function will fail later if the tree
    #  is used. Technically, an empty tree is only valid for the HDIST tree and
    #  not the HCLEN and HLIT tree. However, a stream with an empty HCLEN tree
    #  is guaranteed to fail since it will attempt to use the tree to decode the
    #  codes for the HLIT and HDIST trees. Similarly, an empty HLIT tree is
    #  guaranteed to fail later since the compressed data section must be
    #  composed of at least one symbol (the end-of-block marker).
    if max == 0
        return true
    end

    code = 0
    nextcode = Go.Array(Int, maxCodeLen)
    for i = min:max
        code <<= 1
        nextcode[i] = code
        code += count[i]
    end

    #  Check that the coding is complete (i.e., that we've
    #  assigned all 2-to-the-max possible bit sequences).
    #  Exception: To be compatible with zlib, we also need to
    #  accept degenerate single-code codings. See also
    #  TestDegenerateHuffmanCoding.
    if code != 1 << UInt(max) && !(code == 1 && max == 1)
        return false
    end

    h.min = min
    if max > huffmanChunkBits
        numLinks = 1 << (UInt(max) - huffmanChunkBits)
        h.linkMask = UInt32(numLinks - 1)
        #  create link tables
        link = nextcode[huffmanChunkBits+1] >> 1
        h.links = Go.Slice(Go.Slice{UInt32}, huffmanNumChunks - link)
        for j = UInt(link):(huffmanNumChunks - 1)
            reverse = Int(bitreverse(UInt16(j)))
            reverse >>= UInt(16 - huffmanChunkBits)
            off = j - UInt(link)
            if sanity && h.chunks[reverse] != 0
                error("impossible: overwriting existing chunk")
            end
            h.chunks[reverse] = UInt32(off << huffmanValueShift | (huffmanChunkBits + 1))
            h.links[off] = Go.Slice(UInt32, numLinks)
        end
    end

    for (i, n) in range(lengths)
        if n == 0
            continue
        end
        code = nextcode[n]
        nextcode[n] += 1
        chunk = UInt32(i << huffmanValueShift | n)
        reverse = Int(bitreverse(UInt16(code)))
        reverse >>= UInt(16 - n)
        if n <= huffmanChunkBits
            off = reverse
            while off < length(h.chunks)
                #  We should never need to overwrite
                #  an existing chunk. Also, 0 is
                #  never a valid chunk, because the
                #  lower 4 "count" bits should be
                #  between 1 and 15.
                if sanity && h.chunks[off] != 0
                    error("impossible: overwriting existing chunk")
                end

                h.chunks[off] = chunk
                off += 1 << UInt(n)
            end
        else
            j = reverse & (huffmanNumChunks - 1)
            if sanity && h.chunks[j] & huffmanCountMask != huffmanChunkBits + 1
                #  Longer codes should have been
                #  associated with a link table above.
                error("impossible: not an indirect chunk")
            end
            value = h.chunks[j] >> huffmanValueShift
            linktab = h.links[value]
            reverse >>= huffmanChunkBits
            off = reverse
            while off < length(linktab)
                if sanity && linktab[off] != 0
                    error("impossible: overwriting existing chunk")
                end
                linktab[off] = chunk
                off += 1 << UInt(n - huffmanChunkBits)
            end
        end
    end

    if sanity
        for (i, chunk) in range(h.chunks)
            if chunk == 0
                #  As an exception, in the degenerate
                #  single-code case, we allow odd
                #  chunks to be missing.
                if code == 1 && i % 2 == 1
                    continue
                end
                error("impossible: missing chunk")
            end
        end
        for linktab in h.links
            for chunk in linktab
                if chunk == 0
                    error("impossible: missing chunk")
                end
            end
        end
    end
    return true
end

#  Decompress state.
mutable struct Decompressor <: IO
    r::IO
    roffset::Int64
    b::UInt32
    nb::UInt
    h1::HuffmanDecoder
    h2::HuffmanDecoder
    bits::Go.Array{Int}
    codebits::Go.Array{Int}
    dict::DictDecoder
    buf::Go.Array{UInt8}
    step::Function #=(Decompressor)=#
    stepState::Int
    final::Bool
    toRead::Go.Slice{UInt8}
    hl::HuffmanDecoder
    hd::Union{Nothing, HuffmanDecoder}
    copyLen::Int
    copyDist::Int
end

Decompressor(r::IO, bits::Go.Array{Int}, codebits::Go.Array{Int}, dict::DictDecoder, step::Function) =
    Decompressor(
        r, 0, 0, 0, HuffmanDecoder(), HuffmanDecoder(), bits, codebits, dict, Go.Array(UInt8, 4), step, 0, false, Go.Slice(UInt8, 0), HuffmanDecoder(), nothing, 0, 0
    )

Base.eof(f::Decompressor) = length(f.toRead) == 0 && eof(f.r)

function nextBlock(f::Decompressor)
    while f.nb < 1 + 2
        moreBits(f)
    end

    f.final = f.b & 1 == 1
    f.b >>= 1
    typ = f.b & 3
    f.b >>= 2
    f.nb -= 1 + 2
    if typ == 0
        dataBlock(f)
    elseif typ == 1
        #  compressed, fixed Huffman tables
        f.hl = fixedHuffmanDecoder[]
        f.hd = nothing
        huffmanBlock(f)
    elseif typ == 2
        #  compressed, dynamic Huffman tables
        readHuffman(f)
        f.hl = f.h1
        f.hd = f.h2
        huffmanBlock(f)
    else
        #  3 is reserved.
        throw(CorruptInputError(f.roffset))
    end
end

function Base.readavailable(f::Decompressor)
    if length(f.toRead) > 0
        bytes = copy(f.toRead)
        f.toRead = f.toRead[0:0]
        return bytes
    end
    f.step(f)
    if length(f.toRead) == 0
        #  Flush what's left in case of error
        f.toRead = readFlush(f.dict)
    end
    return length(f.toRead) == 0 ? UInt8[] : readavailable(f)
end

function Base.read(f::Decompressor, b::Go.Slice{UInt8})# ::Tuple{Int, error}
    while true
        if length(f.toRead) > 0
            n = copy(b, f.toRead)
            f.toRead = f.toRead[n, :]
            if length(f.toRead) == 0
                return n
            end
            return n
        end
        f.step(f)
        if length(f.toRead) == 0
            #  Flush what's left in case of error
            f.toRead = readFlush(f.dict)
        end
    end
end

function Base.close(f::Decompressor)# ::error
    return
end

#  RFC 1951 section 3.2.7.
#  Compression with dynamic Huffman codes
const codeOrder = Go.Slice([16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15])

function readHuffman(f::Decompressor)# ::error
    while f.nb < 5 + 5 + 4
        moreBits(f)
    end
    nlit = Int(f.b & 0x1F) + 257
    if nlit > maxNumLit
        throw(CorruptInputError(f.roffset))
    end
    f.b >>= 5
    ndist = Int(f.b & 0x1F) + 1
    if ndist > maxNumDist
        throw(CorruptInputError(f.roffset))
    end
    f.b >>= 5
    nclen = Int(f.b & 0x0F) + 4
    #  numCodes is 19, so nclen is always valid.
    f.b >>= 4
    f.nb -= 5 + 5 + 4
    for i = 0:(nclen - 1)
        while f.nb < 3
            moreBits(f)
        end
        f.codebits[codeOrder[i]] = Int(f.b & 0x07)
        f.b >>= 3
        f.nb -= 3
    end
    for i = nclen:(length(codeOrder) - 1)
        f.codebits[codeOrder[i]] = 0
    end
    if !init(f.h1, f.codebits[:])
        throw(CorruptInputError(f.roffset))
    end
    i, n = 0, nlit + ndist
    while i < n
        x = huffSym(f, f.h1)
        if x < 16
            #  Actual length.
            f.bits[i] = x
            i += 1
            continue
        end
        #  Repeat previous length or zero.
        rep::Int = 0
        nb::UInt = 0
        b::Int = 0
        if x == 16
            rep = 3
            nb = 2
            if i == 0
                throw(CorruptInputError(f.roffset))
            end
            b = f.bits[i-1]
        elseif x == 17
            rep = 3
            nb = 3
            b = 0
        elseif x == 18
            rep = 11
            nb = 7
            b = 0
        else
            throw(InternalError("unexpected length code"))
        end
        while f.nb < nb
            moreBits(f)
        end
        rep += Int(f.b & UInt32(1 << nb - 1))
        f.b >>= nb
        f.nb -= nb
        if i + rep > n
            throw(CorruptInputError(f.roffset))
        end
        for j = 0:(rep - 1)
            f.bits[i] = b
            i += 1
        end
    end
    if !init(f.h1, f.bits[0:nlit]) || !init(f.h2, f.bits[nlit:nlit+ndist])
        throw(CorruptInputError(f.roffset))
    end

    #  As an optimization, we can initialize the min bits to read at a time
    #  for the HLIT tree to the length of the EOB marker since we know that
    #  every block must terminate with one. This preserves the property that
    #  we never read any extra bytes after the end of the DEFLATE stream.
    if f.h1.min < f.bits[endBlockMarker]
        f.h1.min = f.bits[endBlockMarker]
    end
    return
end

#  Decode a single Huffman block from f.
#  hl and hd are the Huffman states for the lit/length values
#  and the distance values, respectively. If hd == nil, using the
#  fixed distance encoding associated with fixed Huffman blocks.
function huffmanBlock(f::Decompressor)
    #  Zero value must be stateInit
    stateInit = 0
    stateDict = 1
    if f.stepState == stateInit
        @goto readLiteral
    elseif f.stepState == stateDict
        @goto copyHistory
    end

@label readLiteral
    #  Read literal and/or (length, distance) according to RFC section 3.2.3.
    v = huffSym(f, f.hl)
    #  number of bits extra
    n::UInt = 0
    length::Int = 0
    if v < 256
        writeByte(f.dict, v % UInt8)
        if availWrite(f.dict) == 0
            f.toRead = readFlush(f.dict)
            f.step = huffmanBlock
            f.stepState = stateInit
            return
        end
        @goto readLiteral
    elseif v == 256
        finishBlock(f)
        return
    elseif v < 265
        length = v - (257 - 3)
        n = 0
    elseif v < 269
        length = v * 2 - (265 * 2 - 11)
        n = 1
    elseif v < 273
        length = v * 4 - (269 * 4 - 19)
        n = 2
    elseif v < 277
        length = v * 8 - (273 * 8 - 35)
        n = 3
    elseif v < 281
        length = v * 16 - (277 * 16 - 67)
        n = 4
    elseif v < 285
        length = v * 32 - (281 * 32 - 131)
        n = 5
    elseif v < maxNumLit
        length = 258
        n = 0
    else
        throw(CorruptInputError(f.roffset))
    end
    if n > 0
        while f.nb < n
            moreBits(f)
        end
        length += Int(f.b & UInt32(1 << n - 1))
        f.b >>= n
        f.nb -= n
    end
    dist::Int = 0
    if f.hd === nothing
        while f.nb < 5
            moreBits(f)
        end
        dist = Int(bitreverse(UInt8((f.b & 0x1F) << 3)))
        f.b >>= 5
        f.nb -= 5
    else
        dist = huffSym(f, f.hd)
    end
    if dist < 4
        dist += 1
    elseif dist < maxNumDist
        nb = UInt(dist - 2) >> 1
        #  have 1 bit in bottom of dist, need nb more.
        extra = (dist & 1) << nb
        while f.nb < nb
            moreBits(f)
        end
        extra |= Int(f.b & UInt32(1 << nb - 1))
        f.b >>= nb
        f.nb -= nb
        dist = 1 << (nb + 1) + 1 + extra
    else
        throw(CorruptInputError(f.roffset))
    end

    #  No check on length; encoding can be prescient.
    if dist > histSize(f.dict)
        throw(CorruptInputError(f.roffset))
    end

    f.copyLen, f.copyDist = length, dist
    @goto copyHistory

@label copyHistory
    #  Perform a backwards copy according to RFC section 3.2.3.
    cnt = tryWriteCopy(f.dict, f.copyDist, f.copyLen)
    if cnt == 0
        cnt = writeCopy(f.dict, f.copyDist, f.copyLen)
    end

    f.copyLen -= cnt
    if availWrite(f.dict) == 0 || f.copyLen > 0
        f.toRead = readFlush(f.dict)
        #  We need to continue this work
        f.step = huffmanBlock
        f.stepState = stateDict
        return
    end
    @goto readLiteral
end

#  Copy a single uncompressed data block from input to output.
function dataBlock(f::Decompressor)
    #  Uncompressed.
    #  Discard current half-byte.
    f.nb = 0
    f.b = 0
    #  Length then ones-complement of length.
    nr = readbytes!(f.r, f.buf.data)
    f.roffset += Int64(nr)
    if nr != 4
        throw(CorruptInputError(f.roffset))
    end
    n = Int(f.buf[0]) | Int(f.buf[1]) << 8
    nn = Int(f.buf[2]) | Int(f.buf[3]) << 8
    if (nn % UInt16) != (~n % UInt16)
        throw(CorruptInputError(f.roffset))
    end
    if n == 0
        f.toRead = readFlush(f.dict)
        finishBlock(f)
        return
    end
    f.copyLen = n
    copyData(f)
end

#  copyData copies f.copyLen bytes from the underlying reader into f.hist.
#  It pauses for reads when f.hist is full.
function copyData(f::Decompressor)
    buf = writeSlice(f.dict)
    if length(buf) > f.copyLen
        buf = buf[begin:f.copyLen]
    end
    cnt = readbytes!(f.r, copy(buf))
    f.roffset += Int64(cnt)
    f.copyLen -= cnt
    writeMark(f.dict, cnt)
    if availWrite(f.dict) == 0 || f.copyLen > 0
        f.toRead = readFlush(f.dict)
        f.step = copyData
        return
    end
    finishBlock(f)
end

function finishBlock(f::Decompressor)
    if f.final
        if availRead(f.dict) > 0
            f.toRead = readFlush(f.dict)
        end
    end
    f.step = nextBlock
    return
end

function moreBits(f::Decompressor)# ::error
    c = read(f.r, UInt8)
    f.roffset += 1
    f.b |= UInt32(c) << f.nb
    f.nb += 8
    return
end

#  Read the next Huffman-encoded symbol from f according to h.
function huffSym(f::Decompressor, h::HuffmanDecoder)# ::Tuple{Int, error}
    #  Since a HuffmanDecoder can be empty or be composed of a degenerate tree
    #  with single element, huffSym must error on these two edge cases. In both
    #  cases, the chunks slice will be 0 for the invalid sequence, leading it
    #  satisfy the n == 0 check below.
    n = UInt(h.min)
    #  Optimization. Compiler isn't smart enough to keep f.b,f.nb in registers,
    #  but is smart enough to keep local variables in registers, so use nb and b,
    #  inline call to moreBits and reassign b,nb back to f on return.
    nb, b = f.nb, f.b
    while true
        while nb < n
            c = read(f.r, UInt8)
            f.roffset += 1
            b |= UInt32(c) << (nb & 31)
            nb += 8
        end
        if nb == 16
        end
        chunk = h.chunks[b & (huffmanNumChunks-1)]
        if nb == 16
        end
        n = UInt(chunk & huffmanCountMask)
        if n > huffmanChunkBits
            chunk = h.links[chunk >> huffmanValueShift][(b >> huffmanChunkBits) & h.linkMask]
            n = UInt(chunk & huffmanCountMask)
        end
        if n <= nb
            if nb == 16
            end
            if n == 0
                f.b = b
                f.nb = nb
                throw(CorruptInputError(f.roffset))
            end
            f.b = b >> (n & 31)
            f.nb = nb - n
            return Int(chunk >> huffmanValueShift)
        end
    end
end

function fixedHuffmanDecoderInit()
    #  These come from the RFC section 3.2.6.
    bits = Go.Array(Int, 288)
    for i = 0:(144 - 1)
        bits[i] = 8
    end
    for i = 144:(256 - 1)
        bits[i] = 9
    end
    for i = 256:(280 - 1)
        bits[i] = 7
    end
    for i = 280:(288 - 1)
        bits[i] = 8
    end
    f = HuffmanDecoder()
    init(f, bits[:])
    fixedHuffmanDecoder[] = f
    return
end

function Base.reset(f::Decompressor, r::IO, dict::Go.Slice{UInt8})# ::error
    f = Decompressor(r, f.bits, f.codebits, f.dict, nextBlock)
    init(f.dict, maxMatchOffset, dict)
    return f
end

#  NewReader returns a new ReadCloser that can be used
#  to read the uncompressed version of r.
#  If r does not also implement io.ByteReader,
#  the Decompressor may read more data than necessary from r.
#  It is the caller's responsibility to call Close on the ReadCloser
#  when finished reading.
# 
#  The ReadCloser returned by NewReader also implements Resetter.
function NewReader(r::IO)# ::io.ReadCloser
    f = Decompressor(
        r, Go.Array(Int, maxNumLit + maxNumDist),
        Go.Array(Int, numCodes),
        DictDecoder(),
        nextBlock,
    )
    init(f.dict, maxMatchOffset, Go.Slice(UInt8, 0))
    return f
end

#  NewReaderDict is like NewReader but initializes the reader
#  with a preset dictionary. The returned Reader behaves as if
#  the uncompressed data stream started with the given dictionary,
#  which has already been read. NewReaderDict is typically used
#  to read data compressed by NewWriterDict.
# 
#  The ReadCloser returned by NewReader also implements Resetter.
function NewReaderDict(r::IO, dict::Go.Slice{UInt8})# ::io.ReadCloser
    f = Decompressor(
        r, Go.Array(Int, maxNumLit + maxNumDist),
        Go.Array(Int, numCodes),
        DictDecoder(),
        nextBlock,
    )
    init(f.dict, maxMatchOffset, dict)
    return f
end

const fixedHuffmanDecoder = Ref{HuffmanDecoder}()
