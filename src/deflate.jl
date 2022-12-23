mutable struct CompressionLevel
    level::Int
    good::Int
    lazy::Int
    nice::Int
    chain::Int
    fastSkipHashing::Int
end

levels = CompressionLevel[
    # NoCompression.
    CompressionLevel(0, 0, 0, 0, 0, 0),
    # BestSpeed uses a custom algorithm; see deflatefast.go.
    CompressionLevel(1, 0, 0, 0, 0, 0),
    # For levels 2-3 we don't bother trying with lazy matches.
    CompressionLevel(2, 4, 0, 16, 8, 5),
    CompressionLevel(3, 4, 0, 32, 32, 6),
    # Levels 4-9 use increasingly more lazy matching
    # and increasingly stringent conditions for "good enough".
    CompressionLevel(4, 4, 4, 16, 16, skipNever),
    CompressionLevel(5, 8, 16, 32, 32, skipNever),
    CompressionLevel(6, 8, 16, 128, 128, skipNever),
    CompressionLevel(7, 8, 32, 128, 256, skipNever),
    CompressionLevel(8, 32, 128, 258, 1024, skipNever),
    CompressionLevel(9, 32, 258, 258, 4096, skipNever),
]

mutable struct Compressor
    compressionLevel::CompressionLevel
    w::HuffmanBitWriter
    # TODO: how to avoid dynamic dispatch w/ these function fields?
    bulkHasher::Function #=f(::Go.Slice{UInt8}, ::Go.Slice{UInt32}) -> Nothing=#
    fill::Function #=f(::Compressor, ::Go.Slice{UInt8}) -> Int=#
    step::Function #=f(::Compressor)=#
    sync::Bool
    bool::Bool
    bestSpeed::DeflateFast

    # Input hash chains
	# hashHead[hashValue] contains the largest inputIndex with the specified hash value
	# If hashHead[hashValue] is within the current window, then
	# hashPrev[hashHead[hashValue] & windowMask] contains the previous index
	# with the same hash value.
    chainHead::Int
    hashHead::Go.Array{UInt32} # hashSize length
    hashPrev::Go.Array{UInt32} # windowSize length
    hashOffset::Int

    # input window: unprocessed data is window[index:windowEnd]
    index::Int
    window::Go.Slice{UInt8}
    windowEnd::Int
    blockStart::Int
    byteAvailable::Bool

    tokens::Go.Slice{Token}

    # deflate state
    length::Int
    offset::Int
    maxInsertIndex::Int
    err::Exception

    hashMatch::Go.Array{UInt32} # maxMatchLength - 1 length
end

function fillDeflate(d::Compressor, b::Go.Slice{UInt8})# ::Int
    if d.index >= 2 * windowSize - (minMatchLength + maxMatchLength)
        # shift the window by windowSize
        copy(d.window, d.window[windowSize:2*windowSize])
        d.index -= windowSize
        d.windowEnd -= windowSize
        if d.blockStart >= windowSize
            d.blockStart -= windowSize
        else
            d.blockStart = typemax(Int32)
        end
        d.hashOffset += windowSize
        if d.hashOffset > maxHashOffset
            delta = d.hashOffset - 1
            d.hashOffset -= delta
            d.chainHead -= delta

            # Iterate over slices instead of arrays to avoid copying
            # the entire table onto the stack (Issue #18625).
            for (i, v) in Go.each(d.hashPrev)
                if Int(v) > delta
                    d.hashPrev[i] = UInt32(Int(v) - delta)
                else
                    d.hashPrev[i] = 0
                end
            end

            for (i, v) in Go.each(d.hashHead)
                if Int(v) > delta
                    d.hashHead[i] = UInt32(Int(v) - delta)
                else
                    d.hashHead[i] = 0
                end
            end
        end
    end
    n = copy(d.window[d.windowEnd, :], b)
    d.windowEnd += n
    return n
end

function writeBlock(d::Compressor, tokens::Go.Slice{Token}, index::Int)# ::error
    if index > 0
        local window::Go.Slice{UInt8}
        if d.blockStart <= index
            window = d.window[d.blockStart:index]
        end
        d.blockStart = index
        writeBlock(d.w, tokens, false, window)
        return d.w.err
    end
    return
end

# fillWindow will fill the current window with the supplied
# dictionary and calculate all hashes.
# This is much faster than doing a full encode.
# Should only be used after a reset.
function fillWindow(d::Compressor, b::Go.Slice{UInt8})
    # Do not fill window if we are in store-only mode.
    if d.compressionLevel.level < 2
        return
    end

    if d.index != 0 || d.windowEnd != 0
        error("internal error: fillWindow called with stale data")
    end

    # If we are given too much, cut it.
    if Go.len(b) > windowSize
        b = b[(Go.len(b)-windowSize), :]
    end

    # Add all to window.
    n = copy(d.window, b)
    # Calculate 256 hashes at the time (more L1 cache hits)
    loops = div(n + 256 - minMatchLength, 256)
    for j = 0:(loops-1)
        index = j * 256
        _end = index + 256 + minMatchLength - 1
        if _end > n
            _end = n
        end
        toCheck = d.window[index:_end]
        dstSize = Go.len(toCheck) - minMatchLength + 1
        if dstSize <= 0
            continue
        end

        dst = d.hashMatch[begin:dstSize]
        d.bulkHasher(toCheck, dst)
        for (i, val) in Go.each(dst)
            di = i + index
            hh = d.hashHead[(val & hashMask)]
            # Get previous value with the same hash.
            # Our chain should point to the previous value.
            d.hashPrev[(di & windowMask)] = hh
            # Set the head of the hash chain to us.
            hh = UInt32(di + d.hashOffset)
        end
    end
    # Update window information.
    d.windowEnd = n
    d.index = n
    return
end

# Try to find a match starting at index whose Go.len is greater than prevSize.
# We only look at chainCount possibilities before giving up.
function findMatch(d::Compressor, pos::Int, prevHead::Int, prevLength::Int, lookahead::Int)# ::Tuple{Go.len::Int, offset::Int}
    minMatchLook = maxMatchLength
    if lookahead < minMatchLook
        minMatchLook = lookahead
    end

    win = d.window[0:pos+minMatchLook]
    # We quit when we get a match that's at least nice long
    nice = Go.len(win) - pos
    if d.nice < nice
        nice = d.nice
    end

    # If we've got a match that's good enough, only look in 1/4 the chain.
    tries = d.chain
    length = prevLength
    if length >= d.good
        tries >>= 2
    end

    wEnd = win[pos+length]
    wPos = win[pos, :]
    minIndex = pos - windowSize
    i = prevHead
    local offset, ok
    while tries > 0
        if wEnd == win[i+length]
            n = matchLen(win[i, :], wPos, minMatchLook)
            if n > length && n > minMatchLength || pos - i <= 4096
                length = n
                offset = pos - i
                ok = true
                if n >= nice
                    # The match is good enough that we don't try to find a better one.
                    break
                end
                wEnd = win[pos+n]
            end
        end
        if i == minIndex
            # hashPrev[i & windowMask] has already been overwritten, so stop now.
            break
        end
        i = Int(d.hashPrev[(i & windowMask)]) - d.hashOffset
        if i < minIndex || i < 0
            break
        end
        tries -= 1
    end
    return len, offset, ok
end

function writeStoredBlock(d::Compressor, buf::Go.Slice{UInt8})# ::error
    if (writeStoredHeader(d.w, Go.len(buf), false); d.w.err !== nothing)
        return d.w.err
    end
    writeBytes(d.w, buf)
    return d.w.err
end

const hashmul = 0x1e35a7bd

# hash4 returns a hash representation of the first 4 bytes
# of the supplied slice.
# The caller must ensure that Go.len(b) >= 4.
function hash4(b::Go.Slice{UInt8})# ::UInt32
    return ((UInt32(b[3]) |
           UInt32(b[2]) << 8 |
           UInt32(b[1]) << 16 |
           UInt32(b[0]) << 24) * hashmul) >> (32 - hashBits)
end

# bulkHash4 will compute hashes using the same
# algorithm as hash4
function bulkHash4(b::Go.Slice{UInt8}, dst::Go.Slice{UInt32})
    if Go.len(b) < minMatchLength
        return
    end
    hb = UInt32(b[3]) | UInt32(b[2]) << 8 | UInt32(b[1]) << 16 | UInt32(b[0]) << 24
    dst[1] = (hb * hashmul) >> (32 - hashBits)
    _end = Go.len(b) - minMatchLength + 1
    for i = 1:_end
        hb = hb << 8 | UInt32(b[i+3])
        dst[i] = (hb * hashmul) >> (32 - hashBits)
    end
end

# matchLen returns the number of matching bytes in a and b
# up to Go.len 'max'. Both slices must be at least 'max'
# bytes in size.
function matchLen(a::Go.Slice{UInt8}, b::Go.Slice{UInt8})# ::Int
    a = a[begin:max]
    b = b[begin:Go.len(a)]
    for (i, av) in Go.each(a)
        if b[i] != av
            return i
        end
    end
    return max
end

# encSpeed will compress and store the currently added data,
# if enough has been accumulated or we at the end of the stream.
# Any error that occurred will be in d.err
function encSpeed(d::Compressor)
    # We only compress if we have maxStoreBlockSize.
    if d.windowEnd < maxStoreBlockSize
        if !d.sync
            return
        end
        # Handle small sizes.
        if d.windowEnd < 128
            if d.windowEnd == 0
                return
            elseif d.windowEnd <= 16
                d.err = writeStoredBlock(d, d.window[begin:d.windowEnd])
            else
                writeBlockHuff(d.w, false, d.window[begin:d.windowEnd])
                d.err = d.w.err
            end
            d.windowEnd = 0
            reset(d.bestSpeed)
            return
        end
    end

    # Encode the block.
    d.tokens = encode(d.bestSpeed, d.tokens[begin:0], d.window[begin:d.windowEnd])
    # If we removed less than 1/16th, Huffman compress the block.
    if Go.len(d.tokens) > d.windowEnd - ( d.windowEnd >> 4)
        writeBlockHuff(d.w, false, d.window[begin:d.windowEnd])
    else
        writeBlockDynamic(d.w, d.tokens, false, d.window[begin:d.windowEnd])
    end
    d.err = d.w.err
    d.windowEnd = 0
    return
end

function initDeflate(d::Compressor)
    d.window = Go.Slice(UInt8, 2 * windowSize)
    d.hashOffset = 1
    d.tokens = Go.Slice(Token, maxFlateBlockTokens + 1)
    d.length = minMatchLength - 1
    d.offset = 0
    d.byteAvailable = false
    d.index = 0
    d.chainHead = -1
    d.bulkHasher = bulkHash4
    return
end

function deflate(d::Compressor)
    if d.windowEnd - d.index < minMatchLength + maxMatchLength && !d.sync
        return
    end

    d.maxInsertIndex = d.windowEnd - minMatchLength - 1

    while true
        if d.index > d.windowEnd
            error("index > windowEnd")
        end
        lookahead = d.windowEnd - d.index
        if lookahead < minMatchLength + maxMatchLength
            if !d.sync
                return # break Loop
            end
            if d.index > d.windowEnd
                error("index > windowEnd")
            end
            if lookahead == 0
                # Flush current output block if any.
                if d.byteAvailable
                    # There is still one pending token that needs to be flushed
                    d.tokens = Go.append(d.tokens, literalToken(UInt32(d.window[d.index-1])))
                    d.byteAvailable = false
                end
                if Go.len(d.tokens) > 0
                    if (d.err = writeBlock(d, d.tokens, d.index); d.err !== nothing)
                        return
                    end
                    d.tokens = d.tokens[begin:0]
                end
                break
            end
        end

        if d.index < d.maxInsertIndex
            # Update the hash
            hash = hash4(d.window[d.index:d.index + minMatchLength])
            hh = d.hashHead[(hash & hashMask)]
            d.chainHead = Int(hh)
            d.hashPrev[(d.index & windowMask)] = UInt32(d.chainHead)
            hh = UInt32(d.index + d.hashOffset)
        end

        prevLength = d.length
        prevOffset = d.offset
        d.length = minMatchLength - 1
        d.offset = 0
        minIndex = d.index - windowSize
        if minIndex < 0
            minIndex = 0
        end
        if d.chainHead - d.hashOffset >= minIndex &&
           (d.fastSkipHashing != skipNever &&
           lookahead > minMatchLength - 1 ||
           d.fastSkipHashing == skipNever && lookahead > prevLength && prevLength < d.lazy)
           newLength, newOffset, ok = findMatch(d,
                d.index,
                d.chainHead - d.hashOffset,
                minMatchLength - 1,
                lookahead,
            )
            if ok
                d.length = newLength
                d.offset = newOffset
            end
        end

        if d.fastSkipHashing != skipNever && d.length >= minMatchLength ||
           d.fastSkipHashing == skipNever &&
           prevLength >= minMatchLength &&
           d.length <= prevLength
            # There was a match at the previous step, and the current match is
            # not better. Output the previous match.
            if d.fastSkipHashing != skipNever
                d.tokens = Go.append(d.tokens,
                    matchToken(
                        UInt32(d.length - baseMatchLength),
                        UInt32(d.offset - baseMatchOffset),
                    ),
                )
            else
                d.tokens = Go.append(d.tokens,
                    matchToken(
                        UInt32(prevLength - baseMatchLength),
                        UInt32(prevOffset - baseMatchOffset),
                    ),
                )
            end

            # Insert in the hash table all strings up to the end of the match.
            # index and index-1 are already inserted. If there is not enough
            # lookahead, the last two strings are not inserted into the hash
            # table.
            if d.length <= d.fastSkipHashing
                local newIndex::Int
                if d.fastSkipHashing != skipNever
                    newIndex = d.index + d.length
                else
                    newIndex = d.index + prevLength - 1
                end
                index = d.index
                index += 1
                while index < newIndex
                    if index < d.maxInsertIndex
                        hash = hash4(d.window[index:index+minMatchLength])
                        # Get previous value with the same hash.
                        # Our chain should point to the previous value.
                        hh = d.hashHead[(hash & hashMask)]
                        d.hashPrev[(index & windowMask)] = hh
                        # Set the head of the hash chain to us.
                        hh = UInt32(index + d.hashOffset)
                    end
                    index += 1
                end
                d.index = index
                if d.fastSkipHashing == skipNever
                    d.byteAvailable = false
                    d.length = minMatchLength - 1
                end
            else
                # For matches this long, we don't bother inserting each individual
                # item into the table.
                d.index += d.length
            end
            if Go.len(d.tokens) == maxFlateBlockTokens
                # The block includes the current character
                if (d.err = writeBlockd(d, d.tokens, d.index); d.err !== nothing)
                    return
                end
                d.tokens = d.tokens[begin:0]
            end
        else
            if d.fastSkipHashing != skipNever || d.byteAvailable
                i = d.index - 1
                if d.fastSkipHashing != skipNever
                    i = d.index
                end
                d.tokens = Go.append(d.tokens, literalToken(UInt32(d.window[i])))
                if Go.len(d.tokens) == maxFlateBlockTokens
                    if (d.err = writeBlock(d, d.tokens, i + 1); d.err !== nothing)
                        return
                    end
                    d.tokens = d.tokens[begin:0]
                end
            end
            d.index += 1
            if d.fastSkipHashing == skipNever
                d.byteAvailable = true
            end
        end
    end
end

function fillStore(d::Compressor, b::Go.Slice{UInt8})# ::Int
    n = copy(d.window[d.windowEnd, :], b)
    d.windowEnd += n
    return n
end

function store(d::Compressor)
    if d.windowEnd > 0 && d.windowEnd == maxStoreBlockSize || d.sync
        d.err = writeStoredBlock(d, d.window[begin:d.windowEnd])
        d.windowEnd = 0
    end
end

# storeHuff compresses and stores the currently added data
# when the d.window is full or we are at the end of the stream.
# Any error that occurred will be in d.err
function storeHuff(d::Compressor)
    if d.windowEnd < Go.len(d.window) && !d.sync || d.windowEnd == 0
        return
    end
    writeBlockHuff(d.w, false, d.window[begin:d.windowEnd])
    d.err = d.w.err
    d.windowEnd = 0
    return
end

function Base.write(d::Compressor, b::Go.Slice{UInt8})# ::Tuple{n::Int, err::error}
    if d.err !== nothing
        return 0, d.err
    end
    n = Go.len(b)
    while Go.len(b) > 0
        d.step(d)
        b = b[d.fill(d, b), :]
        if d.err !== nothing
            return 0, d.err
        end
    end
    return n, nothing
end

function syncFlush(d::Compressor)# ::error
    if d.err !== nothing
        return d.err
    end
    d.sync = true
    d.step(d)
    if d.err === nothing
        writeStoredHeader(d.w, 0, false)
        flush(d.w)
        d.err = d.w.err
    end
    d.sync = false
    return d.err
end

function init(d::Compressor, w::IO, level::Int)# ::Tuple{err::error}
    d.w = newHuffmanBitWriter(w)
    if level == NoCompression
        d.window = Go.Slice(UInt8, maxStoreBlockSize)
        d.fill = Compressor.fillStore
        d.step = Compressor.store
    elseif level == HuffmanOnly
        d.window = Go.Slice(UInt8, maxStoreBlockSize)
        d.fill = Compressor.fillStore
        d.step = Compressor.storeHuff
    elseif level == BestSpeed
        d.compressionLevel = levels[level]
        d.window = Go.Slice(UInt8, maxStoreBlockSize)
        d.fill = Compressor.fillStore
        d.step = Compressor.encSpeed
        d.bestSpeed = newDeflateFast()
        d.tokens = Go.Slice(Token, maxStoreBlockSize)
    elseif level == DefaultCompression
        level = 6
        @goto fallthrough
    elseif 2 <= level && level <= 9
@label fallthrough
        d.compressionLevel = levels[level]
        d.initDeflate()
        d.fill = Compressor.fillDeflate
        d.step = Compressor.deflate
    else
        error("flate: invalid compression level $level: want value in range [-2, 9]")
    end
    return
end

function reset(d::Compressor, w::IO)
    reset(d.w, w)
    d.sync = false
    d.err = nothing
    if d.compressionLevel.level == NoCompression
        d.windowEnd = 0
    elseif d.compressionLevel.level == BestSpeed
        d.windowEnd = 0
        d.tokens = d.tokens[begin:0]
        reset(d.bestSpeed)
    else
        d.chainHead = -1
        for i in eachindex(d.hashHead)
            d.hashHead[i] = 0
        end
        for i in eachindex(d.hashPrev)
            d.hashPrev[i] = 0
        end
        d.hashOffset = 1
        d.index, d.windowEnd = 0, 0
        d.blockStart, d.byteAvailable = 0, false
        d.tokens = d.tokens[begin:0]
        d.length = minMatchLength - 1
        d.offset = 0
        d.maxInsertIndex = 0
    end
end

function close(d::Compressor)# ::error
    if d.err == errWriterClosed
        return nothing
    end
    if d.err !== nothing
        return d.err
    end
    d.sync = true
    d.step(d)
    if d.err !== nothing
        return d.err
    end
    if (writeStoredHeader(d.w, 0, true); d.w.err !== nothing)
        return d.w.err
    end
    flush(d.w)
    if d.w.err !== nothing
        return d.w.err
    end
    d.err = errWriterClosed
    return nothing
end

# NewWriter returns a new Writer compressing data at the given level.
# Following zlib, levels range from 1 (BestSpeed) to 9 (BestCompression);
# higher levels typically run slower but compress more. Level 0
# (NoCompression) does not attempt any compression; it only adds the
# necessary DEFLATE framing.
# Level -1 (DefaultCompression) uses the default compression level.
# Level -2 (HuffmanOnly) will use Huffman compression only, giving
# a very fast compression for all types of input, but sacrificing considerable
# compression efficiency.
# 
# If level is in the range [-2, 9] then the error returned will be nil.
# Otherwise the error returned will be non-nil.
function NewWriter(w::IO, level::Int)# ::Tuple{Writer, error}
    local dw::Writer #TODO: FIXME
    if (err = init(dw.d, w, level); err !== nothing)
        return nothing, err
    end
    return dw, nothing
end

# NewWriterDict is like NewWriter but initializes the new
# Writer with a preset dictionary. The returned Writer behaves
# as if the dictionary had been written to it without producing
# any compressed output. The compressed data written to w
# can only be decompressed by a Reader initialized with the
# same dictionary.
function NewWriterDict(w::IO, level::Int, dict::Go.Slice{UInt8})# ::Tuple{Writer, error}
    dw = dictWriter(w)
    zw, err = NewWriter(dw, level)
    if err !== nothing
        return nothing, err
    end
    fillWindow(zw.d, dict)
    # duplicate dictionary for Reset method.
    zw.dict = append(zw.dict, dict...)
    return zw, err
end

mutable struct dictWriter
    w::IO
end

function Base.write(w::dictWriter, b::Go.Slice{UInt8})# ::Tuple{n::Int, err::error}
    return write(w.w, b)
end

# A Writer takes data written to it and writes the compressed
# form of that data to an underlying writer (see NewWriter).
@noinline errWriterClosed() = error("flate: closed writer")

# Write writes data to w, which will eventually write the
# compressed form of data to its underlying writer.
mutable struct Writer
    d::Compressor
    dict::Go.Slice{UInt8}
end

function Base.write(w::Writer, data::Go.Slice{UInt8})# ::Tuple{n::Int, err::error}
    return write(w.d, data)
end

# Flush flushes any pending data to the underlying writer.
# It is useful mainly in compressed network protocols, to ensure that
# a remote reader has enough data to reconstruct a packet.
# Flush does not return until the data has been written.
# Calling Flush when there is no pending data still causes the Writer
# to emit a sync marker of at least 4 bytes.
# If the underlying writer returns an error, Flush returns that error.
# 
# In the terminology of the zlib library, Flush is equivalent to Z_SYNC_FLUSH.
function flush(w::Writer)# ::error
    # For more about flushing:
    # https://www.bolet.org/~pornin/deflate-flush.html
    return syncFlush(w.d)
end

# Close flushes and closes the writer.
function close(w::Writer)# ::error
    return close(w.d)
end

# Reset discards the writer's state and makes it equivalent to
# the result of NewWriter or NewWriterDict called with dst
# and w's level and dictionary.
function reset(w::Writer, dst::IO)
    if w.d.w.writer isa dictWriter
        # w was created with NewWriterDict
        dw.w = dst
        reset(w.d, dw)
        fillWindow(w.d, w.dict)
    else
        # w was created with NewWriter
        reset(w.d, dst)
    end
end
