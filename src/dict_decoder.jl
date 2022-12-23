#  init initializes DictDecoder to have a sliding window dictionary of the given
#  size. If a preset dict is provided, it will initialize the dictionary with
mutable struct DictDecoder
    hist::Go.Slice{UInt8}
    wrPos::Int
    rdPos::Int
    full::Bool
end

DictDecoder() = DictDecoder(Go.Slice(UInt8, 0), 0, 0, false)
DictDecoder(hist) = DictDecoder(hist, 0, 0, false)

#  the contents of dict.
function init(dd::DictDecoder, size::Int, dict::Go.Slice{UInt8})
    dd.wrPos = 0
    dd.rdPos = 0
    dd.full = false
    if Go.cap(dd.hist) < size
        dd.hist = Go.Slice(UInt8, size)
    end
    dd.hist = dd.hist[begin:size]
    if Go.len(dict) > Go.len(dd.hist)
        dict = dict[(Go.len(dict)-Go.len(dd.hist)), :]
    end

    dd.wrPos = copy(dd.hist, dict)
    if dd.wrPos == Go.len(dd.hist)
        dd.wrPos = 0
        dd.full = true
    end
    dd.rdPos = dd.wrPos
    return
end

#  histSize reports the total amount of historical data in the dictionary.
function histSize(dd::DictDecoder)# ::Int
    if dd.full
        return Go.len(dd.hist)
    end
    return dd.wrPos
end

#  availRead reports the number of bytes that can be flushed by readFlush.
function availRead(dd::DictDecoder)# ::Int
    return dd.wrPos - dd.rdPos
end

#  availWrite reports the available amount of output buffer space.
function availWrite(dd::DictDecoder)# ::Int
    return Go.len(dd.hist) - dd.wrPos
end

#  writeSlice returns a slice of the available buffer to write data to.
# 
#  This invariant will be kept: length(s) <= availWrite()
function writeSlice(dd::DictDecoder)# ::Go.Slice{UInt8}
    return dd.hist[dd.wrPos, :]
end

#  writeMark advances the writer pointer by cnt.
# 
#  This invariant must be kept: 0 <= cnt <= availWrite()
function writeMark(dd::DictDecoder, cnt::Int)
    dd.wrPos += cnt
end

#  writeByte writes a single byte to the dictionary.
# 
#  This invariant must be kept: 0 < availWrite()
function writeByte(dd::DictDecoder, c::UInt8)
    dd.hist[dd.wrPos] = c
    dd.wrPos += 1
end

#  writeCopy copies a string at a given (dist, length) to the output.
#  This returns the number of bytes copied and may be less than the requested
#  length if the available space in the output buffer is too small.
# 
#  This invariant must be kept: 0 < dist <= histSize()
function writeCopy(dd::DictDecoder, dist::Int, length::Int)# ::Int
    dstBase = dd.wrPos
    dstPos = dstBase
    srcPos = dstPos - dist
    endPos = dstPos + length
    if endPos > Go.len(dd.hist)
        endPos = Go.len(dd.hist)
    end

    #  Copy non-overlapping section after destination position.
    # 
    #  This section is non-overlapping in that the copy Go.len for this section
    #  is always less than or equal to the backwards distance. This can occur
    #  if a distance refers to data that wraps-around in the buffer.
    #  Thus, a backwards copy is performed here; that is, the exact bytes in
    #  the source prior to the copy is placed in the destination.
    if srcPos < 0
        srcPos += Go.len(dd.hist)
        dstPos += copy(dd.hist[dstPos:endPos], dd.hist[srcPos, :])
        srcPos = 0
    end

    while dstPos < endPos
        dstPos += copy(dd.hist[dstPos:endPos], dd.hist[srcPos:dstPos])
    end

    dd.wrPos = dstPos
    return dstPos - dstBase
end

#  tryWriteCopy tries to copy a string at a given (distance, length) to the
#  output. This specialized version is optimized for short distances.
# 
#  This method is designed to be inlined for performance reasons.
# 
#  This invariant must be kept: 0 < dist <= histSize()
function tryWriteCopy(dd::DictDecoder, dist::Int, length::Int)# ::Int
    dstPos = dd.wrPos
    endPos = dstPos + length
    if dstPos < dist || endPos > Go.len(dd.hist)
        return 0
    end
    dstBase = dstPos
    srcPos = dstPos - dist
    while dstPos < endPos
        dstPos += copy(dd.hist[dstPos:endPos], dd.hist[srcPos:dstPos])
    end
    dd.wrPos = dstPos
    return dstPos - dstBase
end

#  readFlush returns a slice of the historical buffer that is ready to be
#  emitted to the user. The data returned by readFlush must be fully consumed
#  before calling any other DictDecoder methods.
function readFlush(dd::DictDecoder)# ::Go.Slice{UInt8}
    toRead = dd.hist[dd.rdPos:dd.wrPos]
    dd.rdPos = dd.wrPos
    if dd.wrPos == Go.len(dd.hist)
        dd.wrPos, dd.rdPos = 0, 0
        dd.full = true
    end
    return toRead
end
