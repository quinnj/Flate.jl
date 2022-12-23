#  A CorruptInputError reports the presence of corrupt input at a given offset.
struct CorruptInputError
    int64::Int64
end

function Error(e::CorruptInputError)# ::String
    return "flate: corrupt input before offset: $e"
end

#  An InternalError reports an error in the flate code itself.
struct InternalError
    String::String
end

function Error(e::InternalError)# ::String
    return "flate: internal error: $e"
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

#  tree (i.e., neither over-subscribed nor under-subscribed). The exception is a
#  degenerate case where the tree has only a single symbol with length 1. Empty
#  trees are permitted.
function init(h::HuffmanDecoder, lengths::Go.Slice{Int})# ::Bool
    #  Sanity enables additional runtime tests during Huffman
    #  table construction. It's intended to be used during
    #  development to supplement the currently ad-hoc unit tests.
    sanity = false
    if h.min != 0
        h = HuffmanDecoder()
    end

    #  Count number of codes of each length,
    #  compute min and max length.
    count = Go.Array(Int, maxCodeLen)
    local min::Int, max::Int
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
    if code != 1 << UInt(max) && (!code == 1 && max == 1)
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
            h.links[off] = make(Go.Slice{UInt32}, numLinks)
        end
    end

    for (i, n) in Go.each(lengths)
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
            while off < Go.len(h.chunks)
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
            while off < Go.len(linktab)
                if sanity && linktab[off] != 0
                    error("impossible: overwriting existing chunk")
                end
                linktab[off] = chunk
                off += 1 << UInt(n - huffmanChunkBits)
            end
        end
    end

    if sanity
        for (i, chunk) in Go.each(h.chunks)
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
mutable struct Decompressor
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
    err::Exception
    toRead::Go.Slice{UInt8}
    hl::HuffmanDecoder
    hd::Union{Nothing, HuffmanDecoder}
    copyLen::Int
    copyDist::Int
end

function nextBlock(f::Decompressor)
    while f.nb < 1 + 2
        if (f.err = moreBits(f); f.err !== nothing)
            return
        end
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
        f.hl = fixedHuffmanDecoder
        f.hd = nothing
        huffmanBlock(f)
    elseif typ == 2
        #  compressed, dynamic Huffman tables
        if (f.err = readHuffman(f); f.err !== nothing)
            # pass
        else
            f.hl = f.h1
            f.hd = f.h2
            huffmanBlock(f)
        end
    else
        #  3 is reserved.
        f.err = CorruptInputError(f.roffset)
    end
end

function Read(f::Decompressor, b::Go.Slice{UInt8})# ::Tuple{Int, error}
    while true
        if Go.len(f.toRead) > 0
            n = copy(b, f.toRead)
            f.toRead = f.toRead[n, :]
            if Go.len(f.toRead) == 0
                return n, f.err
            end
            return n, nothing
        end
        if f.err !== nothing
            return 0, f.err
        end
        f.step(f)
        if f.err !== nothing && Go.len(f.toRead) == 0
            #  Flush what's left in case of error
            f.toRead = readFlush(f.dict)
        end
    end
end

function Close(f::Decompressor)# ::error
    if f.err == EOFError
        return nothing
    end
    return f.err
end

#  RFC 1951 section 3.2.7.
#  Compression with dynamic Huffman codes
const codeOrder = Go.Slice([16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15])

function readHuffman(f::Decompressor)# ::error
    while f.nb < 5 + 5 + 4
        if (err = moreBits(f); err !== nothing)
            return err
        end
    end

    nlit = Int(f.b & 0x1F) + 257
    if nlit > maxNumLit
        return CorruptInputError(f.roffset)
    end

    f.b >>= 5
    ndist = Int(f.b & 0x1F) + 1
    if ndist > maxNumDist
        return CorruptInputError(f.roffset)
    end
    f.b >>= 5
    nclen = Int(f.b & 0x0F) + 4
    #  numCodes is 19, so nclen is always valid.
    f.b >>= 4
    f.nb -= 5 + 5 + 4
    for i = 0:(nclen - 1)
        while f.nb < 3
            if (err = moreBits(f); err !== nothing)
                return err
            end
        end
        f.codebits[codeOrder[i]] = Int(f.b & 0x07)
        f.b >>= 3
        f.nb -= 3
    end
    for i = nclen:(Go.len(codeOrder) - 1)
        f.codebits[codeOrder[i]] = 0
    end
    if !init(f.h1, f.codebits[:])
        return CorruptInputError(f.roffset)
    end
    i, n = 0, nlit + ndist
    while i < n
        x, err = huffSym(f, f.h1)
        if err !== nothing
            return err
        end
        if x < 16
            #  Actual length.
            f.bits[i] = x
            i += 1
            continue
        end
        #  Repeat previous length or zero.
        local rep::Int
        local nb::UInt
        local b::Int
        if x == 16
            rep = 3
            nb = 2
            if i == 0
                return CorruptInputError(f.roffset)
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
            return InternalError("unexpected length code")
        end
        while f.nb < nb
            if (err = moreBits(f); err !== nothing)
                return err
            end
        end
        rep += Int(f.b & UInt32(1 << nb - 1))
        f.b >>= nb
        f.nb -= nb
        if i + rep > n
            return CorruptInputError(f.roffset)
        end
        for j = 0:(rep - 1)
            f.bits[i] = b
            i += 1
        end
    end

    if !init(f.h1, f.bits[0:nlit]) || !init(f.h2, f.bits[nlit:nlit+ndist])
        return CorruptInputError(f.roffset)
    end

    #  As an optimization, we can initialize the min bits to read at a time
    #  for the HLIT tree to the length of the EOB marker since we know that
    #  every block must terminate with one. This preserves the property that
    #  we never read any extra bytes after the end of the DEFLATE stream.
    if f.h1.min < f.bits[endBlockMarker]
        f.h1.min = f.bits[endBlockMarker]
    end

    return nothing
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
    v, err = huffSym(f, f.hl)
    if err !== nothing
        f.err = err
        return
    end
    #  number of bits extra
    local n::UInt
    local length::Int
    if v < 256
        writeByte(f.dict, UInt8(v))
        if availWrite(f.dict) == 0
            f.toRead = readFlush(f.dict)
            #FIXME
            f.step = Decompressor.huffmanBlock
            f.stepState = stateInit
            return
        end
        @goto readLiteral
    elseif v == 256
        finishBlock(f)
        return
    elseif v < 265
        length = v - 257 - 3
        n = 0
    elseif v < 269
        length = v * 2 - 265 * 2 - 11
        n = 1
    elseif v < 273
        length = v * 4 - 269 * 4 - 19
        n = 2
    elseif v < 277
        length = v * 8 - 273 * 8 - 35
        n = 3
    elseif v < 281
        length = v * 16 - 277 * 16 - 67
        n = 4
    elseif v < 285
        length = v * 32 - 281 * 32 - 131
        n = 5
    elseif v < maxNumLit
        length = 258
        n = 0
    else
        f.err = CorruptInputError(f.roffset)
        return
    end

    if n > 0
        while f.nb < n
            if (err = moreBits(f); err !== nothing)
                f.err = err
                return
            end
        end
        length += Int(f.b & UInt32(1 << n - 1))
        f.b >>= n
        f.nb -= n
    end
    local dist::Int
    if f.hd === nothing
        while f.nb < 5
            if (err = moreBits(f); err !== nothing)
                f.err = err
                return
            end
        end
        dist = Int(bitreverse(UInt8(f.b & 0x1F << 3)))
        f.b >>= 5
        f.nb -= 5
    else
        dist, err = huffSym(f, f.hd)
        if err !== nothing
            f.err = err
            return
        end
    end

    if dist < 4
        dist += 1
    elseif dist < maxNumDist
        nb = UInt(dist - 2) >> 1
        #  have 1 bit in bottom of dist, need nb more.
        extra = dist & 1 << nb
        while f.nb < nb
            if (err = moreBits(f); err !== nothing)
                f.err = err
                return
            end
        end
        extra |= Int(f.b & UInt32(1 << nb - 1))
        f.b >>= nb
        f.nb -= nb
        dist = 1 << (nb + 1) + 1 + extra
    else
        f.err = CorruptInputError(f.roffset)
        return
    end

    #  No check on length; encoding can be prescient.
    if dist > histSize(f.dict)
        f.err = CorruptInputError(f.roffset)
        return
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
        # FIXME
        f.step = Decompressor.huffmanBlock
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
    # FIXME
    nr, err = io.ReadFull(f.r, f.buf[0:4])
    f.roffset += Int64(nr)
    if err !== nothing
        f.err = noEOF(err)
        return
    end

    n = Int(f.buf[0]) | Int(f.buf[1]) << 8
    nn = Int(f.buf[2]) | Int(f.buf[3]) << 8
    if UInt16(nn) != UInt16(~(n))
        f.err = CorruptInputError(f.roffset)
        return
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
    if Go.len(buf) > f.copyLen
        buf = buf[begin:f.copyLen]
    end

    # FIXME
    cnt, err = io.ReadFull(f.r, buf)
    f.roffset += Int64(cnt)
    f.copyLen -= cnt
    writeMark(f.dict, cnt)
    if err !== nothing
        f.err = noEOF(err)
        return
    end

    if availWrite(f.dict) == 0 || f.copyLen > 0
        f.toRead = readFlush(f.dict)
        # FIXME
        f.step = Decompressor.copyData
        return
    end

    finishBlock(f)
end

function finishBlock(f::Decompressor)
    if f.final
        if availRead(f.dict) > 0
            f.toRead = readFlush(f.dict)
        end
        f.err = io.EOF
    end
    # FIXME
    f.step = Decompressor.nextBlock
end

#  noEOF returns err, unless err == io.EOF, in which case it returns io.ErrUnexpectedEOF.
function noEOF(e::Exception)# ::error
    if e == EOFError
        return io.ErrUnexpectedEOF
    end
    return e
end

function moreBits(f::Decompressor)# ::error
    c, err = ReadByte(f.r)
    if err !== nothing
        return noEOF(err)
    end
    f.roffset += 1
    f.b |= UInt32(c) << f.nb
    f.nb += 8
    return nothing
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
            c, err = ReadByte(f.r)
            if err !== nothing
                f.b = b
                f.nb = nb
                return 0, noEOF(err)
            end
            f.roffset += 1
            b |= UInt32(c) << nb & 31
            nb += 8
        end

        chunk = h.chunks[b&(huffmanNumChunks-1)]
        n = UInt(chunk & huffmanCountMask)
        if n > huffmanChunkBits
            chunk = h.links[chunk>>huffmanValueShift][(b>>huffmanChunkBits)&h.linkMask]
            n = UInt(chunk & huffmanCountMask)
        end
        if n <= nb
            if n == 0
                f.b = b
                f.nb = nb
                f.err = CorruptInputError(f.roffset)
                return 0, f.err
            end
            f.b = b >> n & 31
            f.nb = nb - n
            return Int(chunk >> huffmanValueShift), nothing
        end
    end
end

#FIXME
function makeReader(r::IO)# ::Reader
    rr, ok = (r::Reader)
    if ok
        return rr
    end
    return bufio.NewReader(r)
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
    init(fixedHuffmanDecoder[], bits[:])
    return
end

function Reset(f::Decompressor, r::IO, dict::Go.Slice{UInt8})# ::error
    #FIXME
    f = Decompressor(
        r = makeReader(r),
        bits = f.bits,
        codebits = f.codebits,
        dict = f.dict,
        step = Decompressor.nextBlock,
    )

    init(f.dict, maxMatchOffset, dict)
    return nothing
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
    fixedHuffmanDecoderInit()
    local f::Decompressor

    f.r = makeReader(r)
    f.bits = new(Go.Slice{Int})
    f.codebits = new(Go.Slice{Int})
    f.step = Decompressor.nextBlock
    f.dict.init(maxMatchOffset, nothing)
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
    fixedHuffmanDecoderInit()
    local f::Decompressor

    f.r = makeReader(r)
    f.bits = new(Go.Slice{Int})
    f.codebits = new(Go.Slice{Int})
    f.step = Decompressor.nextBlock
    f.dict.init(maxMatchOffset, dict)
    return f
end

const fixedHuffmanDecoder = Ref{HuffmanDecoder}()
