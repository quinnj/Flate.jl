mutable struct HuffmanBitWriter{I <: IO}
    writer::I
    bits::UInt64
    nbits::UInt
    bytes::Go.Array{UInt8}
    codegenFreq::Go.Array{Int32}
    nbytes::Int
    literalFreq::Go.Slice{Int32}
    offsetFreq::Go.Slice{Int32}
    codegen::Go.Slice{UInt8}
    literalEncoding::HuffmanEncoder
    offsetEncoding::HuffmanEncoder
    codegenEncoding::HuffmanEncoder
    err::Any
end

function newHuffmanBitWriter(w::IO)# ::HuffmanBitWriter
    return HuffmanBitWriter(
        w, UInt64(0), UInt(0), Go.Array(UInt8, bufferSize), Go.Array(Int32, codegenCodeCount),
        0, Go.Slice(Int32, maxNumLit), Go.Slice(Int32, offsetCodeCount),
        Go.Slice(UInt8, maxNumLit + offsetCodeCount + 1),
        newHuffmanEncoder(maxNumLit),
        newHuffmanEncoder(offsetCodeCount),
        newHuffmanEncoder(codegenCodeCount),
        nothing
    )
end

function reset(w::HuffmanBitWriter, writer::IO)
    w.writer = writer
    w.bits, w.nbits, w.nbytes, w.err = 0, 0, 0, nothing
end

function flush(w::HuffmanBitWriter)
    if w.err !== nothing
        w.nbits = 0
        return
    end

    n = w.nbytes
    while w.nbits != 0
        w.bytes[n] = (w.bits) % UInt8
        w.bits >>= 8
        if w.nbits > 8
            # Avoid underflow
            w.nbits -= 8
        else
            w.nbits = 0
        end
        n += 1
    end
    w.bits = 0
    write(w, w.bytes[begin:n])
    w.nbytes = 0
end

function Base.write(w::HuffmanBitWriter, b::Go.Slice{UInt8})
    if w.err !== nothing
        return
    end
    write(w.writer, b)
end

function writeBits(w::HuffmanBitWriter, b::Integer, nb::Integer)
    if w.err !== nothing
        return
    end

    w.bits |= (UInt64(b) << w.nbits)
    w.nbits += nb
    if w.nbits >= 48
        bits = w.bits
        w.bits >>= 48
        w.nbits -= 48
        n = w.nbytes
        bytes = w.bytes[n:n+6]
        bytes[0] = (bits) % UInt8
        bytes[1] = (bits >> 8) % UInt8
        bytes[2] = (bits >> 16) % UInt8
        bytes[3] = (bits >> 24) % UInt8
        bytes[4] = (bits >> 32) % UInt8
        bytes[5] = (bits >> 40) % UInt8
        n += 6
        if n >= bufferFlushSize
            write(w, w.bytes[begin:n])
            n = 0
        end
        w.nbytes = n
    end
end

function writeBytes(w::HuffmanBitWriter, bytes::Go.Slice{UInt8})
    if w.err !== nothing
        return
    end
    n = w.nbytes
    if w.nbits & 7 != 0
        w.err = InternalError("writeBytes with unfinished bits")
        return
    end
    while w.nbits != 0
        w.bytes[n] = (w.bits) % UInt8
        w.bits >>= 8
        w.nbits -= 8
        n += 1
    end
    if n != 0
        write(w, w.bytes[begin:n])
    end
    w.nbytes = 0
    write(w, bytes)
end

# RFC 1951 3.2.7 specifies a special run-length encoding for specifying
# the literal and offset lengths arrays (which are concatenated into a single
# array).  This method generates that run-length encoding.
# 
# The result is written into the codegen array, and the frequencies
# of each code is written into the codegenFreq array.
# Codes 0-15 are single byte codes. Codes 16-18 are followed by additional
# information. Code badCode is an end marker
# 
# 	numLiterals      The number of literals in literalEncoding
# 	numOffsets       The number of offsets in offsetEncoding
# 	litenc, offenc   The literal and offset encoder to use
function generateCodegen(
    w::HuffmanBitWriter,
    numLiterals::Int,
    numOffsets::Int,
    litEnc::HuffmanEncoder,
    offEnc::HuffmanEncoder,
)
    for i in eachindex(w.codegenFreq)
        w.codegenFreq[i] = 0
    end
    # Note that we are using codegen both as a temporary variable for holding
    # a copy of the frequencies, and as the place where we put the result.
    # This is fine because the output is always shorter than the input used
    # so far.
    # cache
    codegen = w.codegen
    # Copy the concatenated code sizes to codegen. Put a marker at the end.
    cgnl = codegen[begin:numLiterals]
    for i in eachindex(cgnl)
        cgnl[i] = (litEnc.codes[i].len) % UInt8
    end
    cgnl = codegen[numLiterals:numLiterals+numOffsets]
    for i in eachindex(cgnl)
        cgnl[i] = (offEnc.codes[i].len) % UInt8
    end
    codegen[numLiterals+numOffsets] = badCode
    size = codegen[0]
    count = 1
    outIndex = 0
    inIndex = 1
    while size != badCode
        # INVARIANT: We have seen "count" copies of size that have not yet
        # had output generated for them.
        nextSize = codegen[inIndex]
        if nextSize == size
            count += 1
            inIndex += 1
            continue
        end

        # We need to generate codegen indicating "count" of size.
        if size != 0
            codegen[outIndex] = size
            outIndex += 1
            w.codegenFreq[size] += 1
            count -= 1
            while count >= 3
                n = 6
                if n > count
                    n = count
                end
                codegen[outIndex] = 16
                outIndex += 1
                codegen[outIndex] = (n - 3) % UInt8
                outIndex += 1
                w.codegenFreq[16] += 1
                count -= n
            end
        else
            while count >= 11
                n = 138
                if n > count
                    n = count
                end
                codegen[outIndex] = 18
                outIndex += 1
                codegen[outIndex] = (n - 11) % UInt8
                outIndex += 1
                w.codegenFreq[18] += 1
                count -= n
            end
            if count >= 3
                # count >= 3 && count <= 10
                codegen[outIndex] = 17
                outIndex += 1
                codegen[outIndex] = (count - 3) % UInt8
                outIndex += 1
                w.codegenFreq[17] += 1
                count = 0
            end
        end
        count -= 1
        while count >= 0
            codegen[outIndex] = size
            outIndex += 1
            w.codegenFreq[size] += 1
            count -= 1
        end
        # Set up invariant for next time through the loop.
        size = nextSize
        count = 1
        inIndex += 1
    end
    # Marker indicating the end of the codegen.
    codegen[outIndex] = badCode
    return
end

# dynamicSize returns the size of dynamically encoded data in bits.
function dynamicSize(w::HuffmanBitWriter, litEnc::HuffmanEncoder, offEnc::HuffmanEncoder, extraBits::Integer)# ::Tuple{size::Int, numCodegens::Int}
    numCodegens = Go.len(w.codegenFreq)
    while numCodegens > 4 && w.codegenFreq[codegenOrder[numCodegens-1]] == 0
        numCodegens -= 1
    end

    header = 3 + 5 + 5 + 4 + (3 * numCodegens) +
        bitLength(w.codegenEncoding, w.codegenFreq[:]) +
        Int(w.codegenFreq[16]) * 2 +
        Int(w.codegenFreq[17]) * 3 +
        Int(w.codegenFreq[18]) * 7
    size = header +
        bitLength(litEnc, w.literalFreq) +
        bitLength(offEnc, w.offsetFreq) +
        extraBits
    return size, numCodegens
end

# fixedSize returns the size of dynamically encoded data in bits.
function fixedSize(w::HuffmanBitWriter, extraBits::Int)# ::Int
    return 3 +
           bitLength(fixedLiteralEncoding, w.literalFreq) +
           bitLength(fixedOffsetEncoding, w.offsetFreq) +
           extraBits
end

# storedSize calculates the stored size, including header.
# The function returns the size in bits and whether the block
# fits inside a single block.
function storedSize(w::HuffmanBitWriter, in::Go.Slice{UInt8})# ::Tuple{Int, Bool}
    if Go.len(in) == 0
        return 0, false
    end
    if Go.len(in) <= maxStoreBlockSize
        return (Go.len(in) + 5) * 8, true
    end
    return 0, false
end

function writeCode(w::HuffmanBitWriter, c::HCode)
    if w.err !== nothing
        return
    end
    w.bits |= UInt64(c.code) << w.nbits
    w.nbits += UInt(c.len)
    if w.nbits >= 48
        bits = w.bits
        w.bits >>= 48
        w.nbits -= 48
        n = w.nbytes
        bytes = w.bytes[n:n+6]
        bytes[0] = (bits) % UInt8
        bytes[1] = (bits >> 8) % UInt8
        bytes[2] = (bits >> 16) % UInt8
        bytes[3] = (bits >> 24) % UInt8
        bytes[4] = (bits >> 32) % UInt8
        bytes[5] = (bits >> 40) % UInt8
        n += 6
        if n >= bufferFlushSize
            write(w, w.bytes[begin:n])
            n = 0
        end
        w.nbytes = n
    end
end

# Write the header of a dynamic Huffman block to the output stream.
# 
# 	numLiterals  The number of literals specified in codegen
# 	numOffsets   The number of offsets specified in codegen
# 	numCodegens  The number of codegens used in codegen
function writeDynamicHeader(
    w::HuffmanBitWriter,
    numLiterals::Int,
    numOffsets::Int,
    numCodegens::Int,
    isEof::Bool,
)
    if w.err !== nothing
        return
    end
    firstBits::Int32 = 4
    if isEof
        firstBits = 5
    end
    writeBits(w, firstBits, 3)
    writeBits(w, Int32(numLiterals - 257), 5)
    writeBits(w, Int32(numOffsets - 1), 5)
    writeBits(w, Int32(numCodegens - 4), 4)
    for i = 0:(numCodegens-1)
        value = UInt(w.codegenEncoding.codes[codegenOrder[i]].len)
        writeBits(w, Int32(value), 3)
    end
    
    i = 0
    while true
        codeWord::Int = Int(w.codegen[i])
        i += 1
        if codeWord == badCode
            break
        end
        writeCode(w, w.codegenEncoding.codes[codeWord])
        if codeWord == 16
            writeBits(w, Int32(w.codegen[i]), 2)
            i += 1
        elseif codeWord == 17
            writeBits(w, Int32(w.codegen[i]), 3)
            i += 1
        elseif codeWord == 18
            writeBits(w, Int32(w.codegen[i]), 7)
            i += 1
        end
    end
    return
end

function writeStoredHeader(w::HuffmanBitWriter, length::Int, isEof::Bool)
    if w.err !== nothing
        return
    end
    flag::Int32 = 0
    if isEof
        flag = 1
    end
    writeBits(w, flag, 3)
    flush(w)
    writeBits(w, Int32(length), 16)
    writeBits(w, Int32(~(UInt16(length))), 16)
end

function writeFixedHeader(w::HuffmanBitWriter, isEof::Bool)
    if w.err !== nothing
        return
    end
    # Indicate that we are a fixed Huffman block
    value::Int32 = 2
    if isEof
        value = 3
    end
    writeBits(w, value, 3)
end

# writeBlock will write a block of tokens with the smallest encoding.
# The original input can be supplied, and if the huffman encoded data
# is larger than the original bytes, the data will be written as a
# stored block.
# If the input is nil, the tokens will always be Huffman encoded.
function writeBlock(
    w::HuffmanBitWriter,
    tokens::Go.Slice{Token},
    eof::Bool,
    input::Go.Slice{UInt8},
)
    if w.err !== nothing
        return
    end
    tokens = Go.append(tokens, Token(endBlockMarker))
    numLiterals, numOffsets = indexTokens(w, tokens)
    extraBits::Int = 0
    ss, storable = storedSize(w, input)
    if storable
        for lengthCode = (lengthCodesStart + 8):(numLiterals - 1)
            # First eight length codes have extra size = 0.
            extraBits +=
                Int(w.literalFreq[lengthCode]) *
                Int(lengthExtraBits[lengthCode-lengthCodesStart])
        end
        for offsetCode = 4:(numOffsets - 1)
            # First four offset codes have extra size = 0.
            extraBits += Int(w.offsetFreq[offsetCode]) * Int(offsetExtraBits[offsetCode])
        end
    end
    # Figure out smallest code.
    # Fixed Huffman baseline.
    literalEncoding = fixedLiteralEncoding
    offsetEncoding = fixedOffsetEncoding
    size = fixedSize(w, extraBits)

    # Generate codegen and codegenFrequencies, which indicates how to encode
    # the literalEncoding and the offsetEncoding.
    numCodegens::Int = 0

    generateCodegen(w, numLiterals, numOffsets, w.literalEncoding, w.offsetEncoding)
    generate(w.codegenEncoding, w.codegenFreq[:], 7)
    ds, numCodegens = dynamicSize(w, w.literalEncoding, w.offsetEncoding, extraBits)
    if ds < size
        size = ds
        literalEncoding = w.literalEncoding
        offsetEncoding = w.offsetEncoding
    end
    # Stored bytes?
    if storable && ss < size
        writeStoredHeader(w, len(input), eof)
        writeBytes(w, input)
        return
    end

    # Huffman.
    if literalEncoding == fixedLiteralEncoding
        writeFixedHeader(w, eof)
    else
        writeDynamicHeader(w, numLiterals, numOffsets, numCodegens, eof)
    end

    # Write the tokens.
    writeTokens(w, tokens, literalEncoding.codes, offsetEncoding.codes)
end

# writeBlockDynamic encodes a block using a dynamic Huffman table.
# This should be used if the symbols used have a disproportionate
# histogram distribution.
# If input is supplied and the compression savings are below 1/16th of the
# input size the block is stored.
function writeBlockDynamic(
    w::HuffmanBitWriter,
    tokens::Go.Slice{Token},
    eof::Bool,
    input::Go.Slice{UInt8},
)
    if w.err !== nothing
        return
    end
    tokens = Go.append(tokens, endBlockMarker)
    numLiterals, numOffsets = indexTokens(w, tokens)
    # Generate codegen and codegenFrequencies, which indicates how to encode
    # the literalEncoding and the offsetEncoding.
    generateCodegen(w, numLiterals, numOffsets, w.literalEncoding, w.offsetEncoding)
    generate(w.codegenEncoding, w.codegenFreq[:], 7)
    size, numCodegens = dynamicSize(w, w.literalEncoding, w.offsetEncoding, 0)
    # Store bytes, if we don't get a reasonable improvement.
    ssize, storable = storedSize(w, input)
    if (storable && ssize < size + size >> 4)
        writeStoredHeader(w, len(input), eof)
        writeBytes(w, input)
        return
    end

    # Write Huffman table.
    writeDynamicHeader(w, numLiterals, numOffsets, numCodegens, eof)
    # Write the tokens.
    writeTokens(w, tokens, w.literalEncoding.codes, w.offsetEncoding.codes)
end

# indexTokens indexes a slice of tokens, and updates
# literalFreq and offsetFreq, and generates literalEncoding
# and offsetEncoding.
# The number of literal and offset tokens is returned.
function indexTokens(w::HuffmanBitWriter, tokens::Go.Slice{Token})# ::Tuple{numLiterals::Int, numOffsets::Int}
    for i in eachindex(w.literalFreq)
        w.literalFreq[i] = 0
    end
    for i in eachindex(w.offsetFreq)
        w.offsetFreq[i] = 0
    end

    for t in tokens
        if UInt32(t) < matchType
            w.literalFreq[literal(t)] += 1
            continue
        end

        length = len(t)
        off = offset(t)
        w.literalFreq[lengthCodesStart+lengthCode(length)] += 1
        w.offsetFreq[offsetCode(off)] += 1
    end

    # get the number of literals
    numLiterals = Go.len(w.literalFreq)
    while w.literalFreq[numLiterals-1] == 0
        numLiterals -= 1
    end

    # get the number of offsets
    numOffsets = Go.len(w.offsetFreq)
    while numOffsets > 0 && w.offsetFreq[numOffsets-1] == 0
        numOffsets -= 1
    end

    if numOffsets == 0
        # We haven't found a single match. If we want to go with the dynamic encoding,
        # we should count at least one offset to be sure that the offset huffman tree could be encoded.
        w.offsetFreq[0] = 1
        numOffsets = 1
    end

    generate(w.literalEncoding, w.literalFreq, 15)
    generate(w.offsetEncoding, w.offsetFreq, 15)
    return numLiterals, numOffsets
end

# writeTokens writes a slice of tokens to the output.
# codes for literal and offset encoding must be supplied.
function writeTokens(
    w::HuffmanBitWriter,
    tokens::Go.Slice{Token},
    leCodes::Go.Slice{HCode},
    oeCodes::Go.Slice{HCode},
)
    if w.err !== nothing
        return
    end
    for t in tokens
        if UInt32(t) < matchType
            writeCode(w, leCodes[literal(t)])
            continue
        end
        # Write the length
        _len = len(t)
        lc = lengthCode(_len)
        writeCode(w, leCodes[lc+lengthCodesStart])
        extraLengthBits = UInt(lengthExtraBits[lc])
        if extraLengthBits > 0
            extraLength = Int32(_len - lengthBase[lc])
            writeBits(w, extraLength, extraLengthBits)
        end

        # Write the offset
        off = offset(t)
        oc = offsetCode(off)
        writeCode(w, oeCodes[oc])
        extraOffsetBits = UInt(offsetExtraBits[oc])
        if extraOffsetBits > 0
            extraOffset = Int32(off - offsetBase[oc])
            writeBits(w, extraOffset, extraOffsetBits)
        end
    end
end

# writeBlockHuff encodes a block of bytes as either
# Huffman encoded literals or uncompressed bytes if the
# results only gains very little from compression.
function writeBlockHuff(w::HuffmanBitWriter, eof::Bool, input::Go.Slice{UInt8})
    if w.err !== nothing
        return
    end
    for i in eachindex(w.literalFreq)
        w.literalFreq[i] = 0
    end
    # Add everything as literals
    histogram(input, w.literalFreq)
    w.literalFreq[endBlockMarker] = 1
    numLiterals = endBlockMarker + 1

    w.offsetFreq[0] = 1
    numOffsets = 1

    generate(w.literalEncoding, w.literalFreq, 15)
    # Generate codegen and codegenFrequencies, which indicates how to encode
    # Figure out smallest code.
    # Always use dynamic Huffman or Store
    # the literalEncoding and the offsetEncoding.
    numCodegens::Int = 0

    generateCodegen(w, numLiterals, numOffsets, w.literalEncoding, huffOffset[])
    generate(w.codegenEncoding, w.codegenFreq[:], 7)
    size, numCodegens = dynamicSize(w, w.literalEncoding, huffOffset[], 0)
    # Store bytes, if we don't get a reasonable improvement.
    ssize, storable = storedSize(w, input)
    if (storable && ssize < (size + size >> 4))
        writeStoredHeader(w, Go.len(input), eof)
        writeBytes(w, input)
        return
    end

    # Huffman.
    writeDynamicHeader(w, numLiterals, numOffsets, numCodegens, eof)
    encoding = w.literalEncoding.codes[begin:257]
    n = w.nbytes
    for t in input
        # Bitwriting inlined, ~30% speedup
        c = encoding[t]
        w.bits |= UInt64(c.code) << w.nbits
        w.nbits += UInt(c.len)
        if w.nbits < 48
            continue
        end
        # Store 6 bytes
        bits = w.bits
        w.bits >>= 48
        w.nbits -= 48
        bytes = w.bytes[n:n+6]
        bytes[0] = (bits) % UInt8
        bytes[1] = (bits >> 8) % UInt8
        bytes[2] = (bits >> 16) % UInt8
        bytes[3] = (bits >> 24) % UInt8
        bytes[4] = (bits >> 32) % UInt8
        bytes[5] = (bits >> 40) % UInt8
        n += 6
        if n < bufferFlushSize
            continue
        end
        write(w, w.bytes[begin:n])
        if w.err !== nothing
            # Return early in the event of write failures
            return
        end
        n = 0
    end

    w.nbytes = n
    writeCode(w, encoding[endBlockMarker])
end

# histogram accumulates a histogram of b in h.
# 
# length(h) must be >= 256, and h's elements must be all zeroes.
function histogram(b::Go.Slice{UInt8}, h::Go.Slice{Int32})
    h = h[begin:256]
    for t in b
        h[t] += 1
    end
end
