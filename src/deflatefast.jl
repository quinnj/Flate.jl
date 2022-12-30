#  DeflateFast maintains the table for matches,
#  and the previous byte block for cross block matching.
mutable struct TableEntry
    val::UInt32
    offset::Int32
end
TableEntry() = TableEntry(0, 0)

mutable struct DeflateFast
    table::Go.Array{TableEntry} # tableSize length
    prev::Go.Slice{UInt8}
    cur::Int32
end

DeflateFast(prev::Go.Slice{UInt8}) = DeflateFast(
    Go.Array([TableEntry() for _ = 1:tableSize]),
    prev,
    Int32(0),
)

function newDeflateFast()# ::DeflateFast
    return DeflateFast(
        Go.Array([TableEntry() for _ = 1:tableSize]),
        Go.Slice(UInt8, 0, maxStoreBlockSize),
        maxStoreBlockSize,
    )
end

#  encode encodes a block given in src and appends tokens
#  to dst and returns the result.
function encode(e::DeflateFast, dst::Go.Slice{Token}, src::Go.Slice{UInt8})# ::Go.Slice{Token}
    #  Ensure that e.cur doesn't wrap.
    if e.cur >= bufferReset
        shiftOffsets(e)
    end

    #  This check isn't in the Snappy implementation, but there, the caller
    #  instead of the callee handles this case.
    if length(src) < minNonLiteralBlockSize
        e.cur += maxStoreBlockSize
        e.prev = e.prev[begin:0]
        return emitLiteral(dst, src)
    end

    #  sLimit is when to stop looking for offset/length copies. The inputMargin
    #  lets us use a fast path for emitLiteral in the main loop, while we are
    #  looking for copies.
    sLimit = Int32(length(src) - inputMargin)
    #  nextEmit is where in src the next emitLiteral should start from.
    nextEmit = Int32(0)
    s = Int32(0)
    cv = load32(src, s)
    nextHash = hash(cv)
    while true
        #  Copied from the C++ snappy implementation:
        # 
        #  Heuristic match skipping: If 32 bytes are scanned with no matches
        #  found, start looking only at every other byte. If 32 more bytes are
        #  scanned (or skipped), look at every third byte, etc.. When a match
        #  is found, immediately go back to looking at every byte. This is a
        #  small loss (~5% performance, ~0.1% density) for compressible data
        #  due to more bookkeeping, but for non-compressible data (such as
        #  JPEG) it's a huge win since the compressor quickly "realizes" the
        #  data is incompressible and doesn't bother looking for matches
        #  everywhere.
        # 
        #  The "skip" variable keeps track of how many bytes there are since
        #  the last match; dividing it by 32 (ie. right-shifting by five) gives
        #  the number of bytes to move ahead for each iteration.
        skip = Int32(32)
        nextS = s
        local candidate::TableEntry

        while true
            s = nextS
            bytesBetweenHashLookups = skip >> 5
            nextS = s + bytesBetweenHashLookups
            skip += bytesBetweenHashLookups
            if nextS > sLimit
                @goto emitRemainder
            end

            candidate = e.table[nextHash & tableMask]
            now = load32(src, nextS)
            e.table[nextHash & tableMask] = TableEntry(cv, s + e.cur)
            nextHash = hash(now)

            offset = s - (candidate.offset - e.cur)
            if offset > maxMatchOffset || cv != candidate.val
                #  Out of range or not matched.
                cv = now
                continue
            end
            break
        end

        #  A 4-byte match has been found. We'll later see if more than 4 bytes
        #  match. But, prior to the match, src[nextEmit:s] are unmatched. Emit
        #  them as literal bytes.
        dst = emitLiteral(dst, src[nextEmit:s])
        # Call emitCopy, and then see if another emitCopy could be our next
        # move. Repeat until we find no match for the input immediately after
        # what was consumed by the last emitCopy call.
        #
        # If we exit this loop normally then we need to call emitLiteral next,
        # though we don't yet know how big the literal will be. We handle that
        # by proceeding to the next iteration of the main loop. We also can
        # exit this loop via goto if we get close to exhausting the input.
        while true
            #  Invariant: we have a 4-byte match at s, and no need to emit any
            #  literal bytes prior to s.
            #  Extend the 4-byte match as long as possible.
            # 
            s += 4
            t = candidate.offset - e.cur + 4
            l = matchLen(e, s, t, src)
            #  matchToken is flate's equivalent of Snappy's emitCopy. (length,offset)
            dst = Go.append(dst,
                matchToken(
                    UInt32(l + 4 - baseMatchLength),
                    UInt32(s - t - baseMatchOffset),
                ),
            )
            s += l
            nextEmit = s
            if s >= sLimit
                @goto emitRemainder
            end

            #  We could immediately start working at s now, but to improve
            #  compression we first update the hash table at s-1 and at s. If
            #  another emitCopy is not our next move, also calculate nextHash
            #  at s+1. At least on GOARCH=amd64, these three hash calculations
            #  are faster as one load64 call (with some shifts) instead of
            #  three load32 calls.
            x = load64(src, s - 1)
            prevHash = hash(x % UInt32)
            e.table[prevHash & tableMask] =
                TableEntry((x % UInt32), e.cur + s - 1)
            x >>= 8
            currHash = hash((x % UInt32))
            candidate = e.table[currHash & tableMask]
            e.table[currHash & tableMask] = TableEntry((x % UInt32), e.cur + s)

            offset = s - (candidate.offset - e.cur)
            if offset > maxMatchOffset || (x % UInt32) != candidate.val
                cv = (x >> 8) % UInt32
                nextHash = hash(cv)
                s += 1
                break
            end
        end
    end

@label emitRemainder
    if Int(nextEmit) < length(src)
        dst = emitLiteral(dst, src[nextEmit, :])
    end
    e.cur += Int32(length(src))
    e.prev = e.prev[begin:length(src)]
    copy(e.prev, src)
    return dst
end

function emitLiteral(dst::Go.Slice{Token}, lit::Go.Slice{UInt8})# ::Vector{Token}
    for v in lit
        dst = Go.append(dst, literalToken(UInt32(v)))
    end
    return dst
end

#  matchLen returns the match length between src[s:] and src[t:].
#  t can be negative to indicate the match is starting in e.prev.
#  We assume that src[s-4:s] and src[t-4:t] already match.
function matchLen(e::DeflateFast, s::Integer, t::Integer, src::Go.Slice{UInt8})# ::Int32
    s1 = Int(s) + maxMatchLength - 4
    if s1 > length(src)
        s1 = length(src)
    end
    #  If we are inside the current block
    if t >= 0
        b = src[t, :]
        a = src[s:s1]
        b = b[begin:length(a)]
        for i in eachindex(a)
            if a[i] != b[i]
                return Int32(i)
            end
        end
        return Int32(length(a))
    end

    #  We found a match in the previous block.
    tp = Int32(length(e.prev)) + t
    if tp < 0
        return 0
    end

    #  Extend the match to be as long as possible.
    a = src[s:s1]
    b = e.prev[tp, :]
    if length(b) > length(a)
        b = b[begin:length(a)]
    end
    a = a[begin:length(b)]
    for i in eachindex(b)
        if a[i] != b[i]
            return Int32(i)
        end
    end
    #  If we reached our limit, we matched everything we are
    #  allowed to in the previous block and we return.
    n = Int32(length(b))
    if Int(s + n) == s1
        return n
    end
    #  Continue looking for more matches in the current block.
    a = src[s+n:s1]
    b = src[begin:length(a)]
    for i in eachindex(a)
        if a[i] != b[i]
            return Int32(i) + n
        end
    end
    return Int32(length(a)) + n
end

#  Reset resets the encoding history.
#  This ensures that no matches are made to the previous block.
function Base.reset(e::DeflateFast)
    e.prev = e.prev[begin:0]
    #  Bump the offset, so all matches will fail distance check.
    #  Nothing should be >= e.cur in the table.
    e.cur += maxMatchOffset
    #  Protect against e.cur wraparound.
    if e.cur >= bufferReset
        shiftOffsets(e)
    end
    return
end

#  shiftOffsets will shift down all match offset.
#  This is only called in rare situations to prevent integer overflow.
# 
#  See https://golang.org/issue/18636 and https://github.com/golang/go/issues/34121.
function shiftOffsets(e::DeflateFast)
    if length(e.prev) == 0
        for i in eachindex(e.table)
            e.table[i] = TableEntry()
        end
        e.cur = maxMatchOffset + 1
        return
    end
    for i in eachindex(e.table)
        v = e.table[i].offset - e.cur + maxMatchOffset + 1
        if v < 0
            #  We want to reset e.cur to maxMatchOffset + 1, so we need to shift
            #  all table entries down by (e.cur - (maxMatchOffset + 1)).
            #  Because we ignore matches > maxMatchOffset, we can cap
            #  any negative offsets at 0.
            v = 0
        end
        e.table[i].offset = v
    end
    e.cur = maxMatchOffset + 1
    return
end
