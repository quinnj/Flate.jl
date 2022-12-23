#  Copyright 2009 The Go Authors. All rights reserved.
#  Use of this source code is governed by a BSD-style
#  license that can be found in the LICENSE file.
module flate

mutable struct deflateTest
    in::Vector{UInt8}
    level::Int
    out::Vector{UInt8}
end

mutable struct deflateInflateTest
    in::Vector{UInt8}
end

mutable struct reverseBitsTest
    in::UInt16
    bitCount::UInt8
    out::UInt16
end

deflateTests = Vector{deflateTest}[
    [Vector{UInt8}[], 0, Vector{UInt8}[1, 0, 0, 255, 255]],
    [Vector{UInt8}[17], -1, Vector{UInt8}[18, 4, 4, 0, 0, 255, 255]],
    [Vector{UInt8}[17], DefaultCompression, Vector{UInt8}[18, 4, 4, 0, 0, 255, 255]],
    [Vector{UInt8}[17], 4, Vector{UInt8}[18, 4, 4, 0, 0, 255, 255]],
    [Vector{UInt8}[17], 0, Vector{UInt8}[0, 1, 0, 254, 255, 17, 1, 0, 0, 255, 255]],
    [Vector{UInt8}[17, 18], 0, Vector{UInt8}[0, 2, 0, 253, 255, 17, 18, 1, 0, 0, 255, 255]],
    [
        Vector{UInt8}[17, 17, 17, 17, 17, 17, 17, 17],
        0,
        Vector{UInt8}[0, 8, 0, 247, 255, 17, 17, 17, 17, 17, 17, 17, 17, 1, 0, 0, 255, 255],
    ],
    [Vector{UInt8}[], 2, Vector{UInt8}[1, 0, 0, 255, 255]],
    [Vector{UInt8}[17], 2, Vector{UInt8}[18, 4, 4, 0, 0, 255, 255]],
    [Vector{UInt8}[17, 18], 2, Vector{UInt8}[18, 20, 2, 4, 0, 0, 255, 255]],
    [
        Vector{UInt8}[17, 17, 17, 17, 17, 17, 17, 17],
        2,
        Vector{UInt8}[18, 132, 2, 64, 0, 0, 0, 255, 255],
    ],
    [Vector{UInt8}[], 9, Vector{UInt8}[1, 0, 0, 255, 255]],
    [Vector{UInt8}[17], 9, Vector{UInt8}[18, 4, 4, 0, 0, 255, 255]],
    [Vector{UInt8}[17, 18], 9, Vector{UInt8}[18, 20, 2, 4, 0, 0, 255, 255]],
    [
        Vector{UInt8}[17, 17, 17, 17, 17, 17, 17, 17],
        9,
        Vector{UInt8}[18, 132, 2, 64, 0, 0, 0, 255, 255],
    ],
]

deflateInflateTests = Vector{deflateInflateTest}[
    [Vector{UInt8}[]],
    [Vector{UInt8}[17]],
    [Vector{UInt8}[17, 18]],
    [Vector{UInt8}[17, 17, 17, 17, 17, 17, 17, 17]],
    [Vector{UInt8}[17, 16, 19, 65, 33, 33, 65, 19, 135, 120, 19]],
    [largeDataChunk()],
]

reverseBitsTests = Vector{reverseBitsTest}[
    [1, 1, 1],
    [1, 2, 2],
    [1, 3, 4],
    [1, 4, 8],
    [1, 5, 16],
    [17, 5, 17],
    [257, 9, 257],
    [29, 5, 23],
]

function largeDataChunk()# ::Vector{UInt8}
    result = make(Vector{UInt8}, 100000)
    for (i) in result
        result[i] = UInt8(i * i & 255)
    end

    return result
end
function TestBulkHash4(t::testing.T)
    for (_, x) in deflateTests
        y = x.out
        if length(y) < minMatchLength
            continue
        end

        y = append(y, y...)
        j = 4
        while j < length(y)
            y = y[begin:j]
            dst = make(Vector{UInt32}, length(y) - minMatchLength + 1)
            for (i) in dst
                dst[i] = UInt32(i + 100)
            end

            bulkHash4(y, dst)
            for (i, got) in dst
                want = hash4(y[i, :])
                if got != want && got == UInt32(i) + 100
                    t.Errorf(
                        "Len:%d Index:%d, want 0x%08x but not modified",
                        length(y),
                        i,
                        want,
                    )
                elseif got != want
                    t.Errorf(
                        "Len:%d Index:%d, got 0x%08x want:0x%08x",
                        length(y),
                        i,
                        got,
                        want,
                    )
                end

            end

            j += 1
        end

    end

end
function TestDeflate(t::testing.T)
    for (_, h) in deflateTests
        local buf::bytes.Buffer

        w, err = NewWriter(buf, h.level)
        if err !== nothing
            t.Errorf("NewWriter: %v", err)
            continue
        end

        w.Write(h.in)
        w.Close()
        if !bytes.Equal(buf.Bytes(), h.out)
            t.Errorf(
                "Deflate(%d, %x) = \\n%#v, want \\n%#v",
                h.level,
                h.in,
                buf.Bytes(),
                h.out,
            )
        end

    end

end
function TestWriterClose(t::testing.T)
    b = new(bytes.Buffer)
    zw, err = NewWriter(b, 6)
    if err !== nothing
        t.Fatalf("NewWriter: %v", err)
    end

    if (c, err = zw.Write(convert(Vector{UInt8}, "Test")); err !== nothing || c != 4)
        t.Fatalf("Write to not closed writer: %s, %d", err, c)
    end

    if (err = zw.Close(); err !== nothing)
        t.Fatalf("Close: %v", err)
    end

    afterClose = b.Len()
    if (c, err = zw.Write(convert(Vector{UInt8}, "Test")); err=== nothing || c != 0)
        t.Fatalf("Write to closed writer: %v, %d", err, c)
    end

    if (err = zw.Flush(); err=== nothing)
        t.Fatalf("Flush to closed writer: %s", err)
    end

    if (err = zw.Close(); err !== nothing)
        t.Fatalf("Close: %v", err)
    end

    if afterClose != b.Len()
        t.Fatalf(
            "Writer wrote data after close. After close: %d. After writes on closed stream: %d",
            afterClose,
            b.Len(),
        )
    end

end
#  A sparseReader returns a stream consisting of 0s followed by 1<<16 1s.
#  This tests missing hash references in a very large input.
mutable struct sparseReader
    l::Int64
    cur::Int64
end

function Read(r::sparseReader, b::Vector{UInt8})# ::Tuple{n::Int, err::error}
    if r.cur >= r.l
        return 0, io.EOF
    end

    n = length(b)
    cur = r.cur + Int64(n)
    if cur > r.l
        n -= Int(cur - r.l)
        cur = r.l
    end

    for (i) in b[0:n]
        if r.cur + Int64(i) >= r.l - 1 << 16
            b[i] = 1
        else
            b[i] = 0
        end

    end

    r.cur = cur
    return
end
function TestVeryLongSparseChunk(t::testing.T)
    if testing.Short()
        t.Skip("skipping sparse chunk during short test")
    end

    w, err = NewWriter(io.Discard, 1)
    if err !== nothing
        t.Errorf("NewWriter: %v", err)
        return
    end

    if (_, err = io.Copy(w, sparseReader[l = 2.3e9]); err !== nothing)
        t.Errorf("Compress failed: %v", err)
        return
    end

end
mutable struct syncBuffer
    buf::bytes.Buffer
    mu::sync.RWMutex
    closed::Bool
    ready::Channel{Bool}
end

function newSyncBuffer()# ::syncBuffer
    return syncBuffer[ready = make(Channel{Bool}, 1)]

end
function Read(b::syncBuffer, p::Vector{UInt8})# ::Tuple{n::Int, err::error}
    while true
        b.mu.RLock()
        n, err = b.buf.Read(p)
        b.mu.RUnlock()
        if n > 0 || b.closed
            return
        end

        take!(b.ready)
    end

end
function signal(b::syncBuffer)
    @select begin
        @send begin
            put!(b.ready, true)
        end
        @default begin end
    end

end
function Write(b::syncBuffer, p::Vector{UInt8})# ::Tuple{n::Int, err::error}
    n, err = b.buf.Write(p)
    b.signal()
    return
end
function WriteMode(b::syncBuffer)
    b.mu.Lock()
end
function ReadMode(b::syncBuffer)
    b.mu.Unlock()
    b.signal()
end
function Close(b::syncBuffer)# ::error
    b.closed = true
    b.signal()
    return nothing
end
function testSync(t::testing.T, level::Int, input::Vector{UInt8}, name::String)
    if length(input) == 0
        return
    end

    t.Logf("--testSync %d, %d, %s", level, length(input), name)
    buf = newSyncBuffer()
    buf1 = new(bytes.Buffer)
    buf.WriteMode()
    w, err = NewWriter(io.MultiWriter(buf, buf1), level)
    if err !== nothing
        t.Errorf("NewWriter: %v", err)
        return
    end

    r = NewReader(buf)
    i = 0
    while i < 2
        local lo::Int, hi::Int

        if i == 0
            lo, hi = 0, length(input) + 1 / 2
        else
            lo, hi = length(input) + 1 / 2, length(input)
        end

        t.Logf("#%d: write %d-%d", i, lo, hi)
        if (_, err = w.Write(input[lo:hi]); err !== nothing)
            t.Errorf("testSync: write: %v", err)
            return
        end

        if i == 0
            if (err = w.Flush(); err !== nothing)
                t.Errorf("testSync: flush: %v", err)
                return
            end

        else
            if (err = w.Close(); err !== nothing)
                t.Errorf("testSync: close: %v", err)
            end

        end

        buf.ReadMode()
        out = make(Vector{UInt8}, hi - lo + 1)
        m, err = io.ReadAtLeast(r, out, hi - lo)
        t.Logf("#%d: read %d", i, m)
        if m != hi - lo || err !== nothing
            t.Errorf(
                "testSync/%d (%d, %d, %s): read %d: %d, %v (%d left)",
                i,
                level,
                length(input),
                name,
                hi - lo,
                m,
                err,
                buf.buf.Len(),
            )
            return
        end

        if !bytes.Equal(input[lo:hi], out[begin:hi-lo])
            t.Errorf(
                "testSync/%d: read wrong bytes: %x vs %x",
                i,
                input[lo:hi],
                out[begin:hi-lo],
            )
            return
        end

        #  This test originally checked that after reading
        #  the first half of the input, there was nothing left
        #  in the read buffer (buf.buf.Len() != 0) but that is
        #  not necessarily the case: the write Flush may emit
        #  some extra framing bits that are not necessary
        #  to process to obtain the first half of the uncompressed
        #  data. The test ran correctly most of the time, because
        #  the background goroutine had usually read even
        #  those extra bits by now, but it's not a useful thing to
        #  check.
        buf.WriteMode()
        i += 1
    end

    buf.ReadMode()
    out = make(Vector{UInt8}, 10)
    if (n, err = r.Read(out); n > 0 || err != io.EOF)
        t.Errorf(
            "testSync (%d, %d, %s): final Read: %d, %v (hex: %x)",
            level,
            length(input),
            name,
            n,
            err,
            out[0:n],
        )
    end

    if buf.buf.Len() != 0
        t.Errorf("testSync (%d, %d, %s): extra data at end", level, length(input), name)
    end

    r.Close()
    #  stream should work for ordinary reader too
    r = NewReader(buf1)
    out, err = io.ReadAll(r)
    if err !== nothing
        t.Errorf("testSync: read: %s", err)
        return
    end

    r.Close()
    if !bytes.Equal(input, out)
        t.Errorf(
            "testSync: decompress(compress(data)) != data: level=%d input=%s",
            level,
            name,
        )
    end

end
function testToFromWithLevelAndLimit(
    t::testing.T,
    level::Int,
    input::Vector{UInt8},
    name::String,
    limit::Int,
)
    local buffer::bytes.Buffer

    w, err = NewWriter(buffer, level)
    if err !== nothing
        t.Errorf("NewWriter: %v", err)
        return
    end

    w.Write(input)
    w.Close()
    if limit > 0 && buffer.Len() > limit
        t.Errorf(
            "level: %d, length(compress(data)) = %d > limit = %d",
            level,
            buffer.Len(),
            limit,
        )
        return
    end

    if limit > 0
        t.Logf(
            "level: %d, size:%.2f%%, %d b\\n",
            level,
            Float64(buffer.Len() * 100) / Float64(limit),
            buffer.Len(),
        )
    end

    r = NewReader(buffer)
    out, err = io.ReadAll(r)
    if err !== nothing
        t.Errorf("read: %s", err)
        return
    end

    r.Close()
    if !bytes.Equal(input, out)
        t.Errorf("decompress(compress(data)) != data: level=%d input=%s", level, name)
        return
    end

    testSync(t, level, input, name)
end
function testToFromWithLimit(
    t::testing.T,
    input::Vector{UInt8},
    name::String,
    limit::Vector{Int},
)
    i = 0
    while i < 10
        testToFromWithLevelAndLimit(t, i, input, name, limit[i])
        i += 1
    end

    #  Test HuffmanCompression
    testToFromWithLevelAndLimit(t, -2, input, name, limit[10])
end
function TestDeflateInflate(t::testing.T)
    t.Parallel()
    for (i, h) in deflateInflateTests
        if testing.Short() && length(h.in) > 10000
            continue
        end

        testToFromWithLimit(t, h.in, string("#%d", i), Vector{Int}[])
    end

end
function TestReverseBits(t::testing.T)
    for (_, h) in reverseBitsTests
        if (v = reverseBits(h.in, h.bitCount); v != h.out)
            t.Errorf("reverseBits(%v,%v) = %v, want %v", h.in, h.bitCount, v, h.out)
        end

    end

end
mutable struct deflateInflateStringTest
    filename::String
    label::String
    limit::Vector{Int}
end

deflateInflateStringTests = Vector{deflateInflateStringTest}[
    [
        "../testdata/e.txt",
        "2.718281828...",
        Vector{Int}[
            100018,
            50650,
            50960,
            51150,
            50930,
            50790,
            50790,
            50790,
            50790,
            50790,
            43683,
        ],
    ],
    [
        "../../testdata/Isaac.Newton-Opticks.txt",
        "Isaac.Newton-Opticks",
        Vector{Int}[
            567248,
            218338,
            198211,
            193152,
            181100,
            175427,
            175427,
            173597,
            173422,
            173422,
            325240,
        ],
    ],
]

function TestDeflateInflateString(t::testing.T)
    t.Parallel()
    if testing.Short() && testenv.Builder() == ""
        t.Skip("skipping in short mode")
    end

    for (_, test) in deflateInflateStringTests
        gold, err = os.ReadFile(test.filename)
        if err !== nothing
            t.Error(err)
        end

        testToFromWithLimit(t, gold, test.label, test.limit)
        if testing.Short()
            break
        end

    end

end
function TestReaderDict(t::testing.T)
    const dict = "hello world"
    const text = "hello again world"

    local b::bytes.Buffer

    w, err = NewWriter(b, 5)
    if err !== nothing
        t.Fatalf("NewWriter: %v", err)
    end

    w.Write(convert(Vector{UInt8}, dict))
    w.Flush()
    b.Reset()
    w.Write(convert(Vector{UInt8}, text))
    w.Close()
    r = NewReaderDict(b, convert(Vector{UInt8}, dict))
    data, err = io.ReadAll(r)
    if err !== nothing
        t.Fatal(err)
    end

    if String(data) != "hello again world"
        t.Fatalf("read returned %q want %q", String(data), text)
    end

end
function TestWriterDict(t::testing.T)
    const dict = "hello world"
    const text = "hello again world"

    local b::bytes.Buffer

    w, err = NewWriter(b, 5)
    if err !== nothing
        t.Fatalf("NewWriter: %v", err)
    end

    w.Write(convert(Vector{UInt8}, dict))
    w.Flush()
    b.Reset()
    w.Write(convert(Vector{UInt8}, text))
    w.Close()
    local b1::bytes.Buffer

    w, _ = NewWriterDict(b1, 5, convert(Vector{UInt8}, dict))
    w.Write(convert(Vector{UInt8}, text))
    w.Close()
    if !bytes.Equal(b1.Bytes(), b.Bytes())
        t.Fatalf("writer wrote %q want %q", b1.Bytes(), b.Bytes())
    end

end
#  See https://golang.org/issue/2508
function TestRegression2508(t::testing.T)
    if testing.Short()
        t.Logf("test disabled with -short")
        return
    end

    w, err = NewWriter(io.Discard, 1)
    if err !== nothing
        t.Fatalf("NewWriter: %v", err)
    end

    buf = make(Vector{UInt8}, 1024)
    i = 0
    while i < 131072
        if (_, err = w.Write(buf); err !== nothing)
            t.Fatalf("writer failed: %v", err)
        end

        i += 1
    end

    w.Close()
end
function TestWriterReset(t::testing.T)
    t.Parallel()
    level = 0
    while level <= 9
        if testing.Short() && level > 1
            break
        end

        w, err = NewWriter(io.Discard, level)
        if err !== nothing
            t.Fatalf("NewWriter: %v", err)
        end

        buf = convert(Vector{UInt8}, "hello world")
        n = 1024
        if testing.Short()
            n = 10
        end

        i = 0
        while i < n
            w.Write(buf)
            i += 1
        end

        w.Reset(io.Discard)
        wref, err = NewWriter(io.Discard, level)
        if err !== nothing
            t.Fatalf("NewWriter: %v", err)
        end

        #  DeepEqual doesn't compare functions.
        w.d.fill, wref.d.fill = nothing, nothing
        w.d.step, wref.d.step = nothing, nothing
        w.d.bulkHasher, wref.d.bulkHasher = nothing, nothing
        w.d.bestSpeed, wref.d.bestSpeed = nothing, nothing
        #  hashMatch is always overwritten when used.
        copy(w.d.hashMatch[:], wref.d.hashMatch[:])
        if length(w.d.tokens) != 0
            t.Errorf(
                "level %d Writer not reset after Reset. %d tokens were present",
                level,
                length(w.d.tokens),
            )
        end

        #  As long as the length is 0, we don't care about the content.
        w.d.tokens = wref.d.tokens
        #  We don't care if there are values in the window, as long as it is at d.index is 0
        w.d.window = wref.d.window
        if !reflect.DeepEqual(w, wref)
            t.Errorf("level %d Writer not reset after Reset", level)
        end

        level += 1
    end

    levels = Vector{Int}[0, 1, 2, 5, 9]

    for (_, level) in levels
        t.Run(fmt.Sprint(level), ((t::testing.T) -> begin
            testResetOutput(t, level, nothing)
        end))
    end

    t.Run(
        "dict",
        (
            (t::testing.T) -> begin
                for (_, level) in levels
                    t.Run(
                        fmt.Sprint(level),
                        ((t::testing.T) -> begin
                            testResetOutput(t, level, nothing)
                        end),
                    )
                end

            end
        ),
    )
end
function testResetOutput(t::testing.T, level::Int, dict::Vector{UInt8})
    writeData = (
        (w::Writer) -> begin
            msg = convert(Vector{UInt8}, "now is the time for all good gophers")
            w.Write(msg)
            w.Flush()
            hello = convert(Vector{UInt8}, "hello world")
            i = 0
            while i < 1024
                w.Write(hello)
                i += 1
            end

            fill = bytes.Repeat(convert(Vector{UInt8}, "x"), 65000)
            w.Write(fill)
        end
    )
    buf = new(bytes.Buffer)
    local w::Writer

    local err::error

    if dict === nothing
        w, err = NewWriter(buf, level)
    else
        w, err = NewWriterDict(buf, level, dict)
    end

    if err !== nothing
        t.Fatalf("NewWriter: %v", err)
    end

    writeData(w)
    w.Close()
    out1 = buf.Bytes()
    buf2 = new(bytes.Buffer)
    w.Reset(buf2)
    writeData(w)
    w.Close()
    out2 = buf2.Bytes()
    if length(out1) != length(out2)
        t.Errorf("got %d, expected %d bytes", length(out2), length(out1))
        return
    end

    if !bytes.Equal(out1, out2)
        mm = 0
        for (i, b) in out1[begin:length(out2)]
            if b != out2[i]
                t.Errorf("mismatch index %d: %#02x, expected %#02x", i, out2[i], b)
            end

            mm += 1
            if mm == 10
                t.Fatal("Stopping")
            end

        end

    end

    t.Logf("got %d bytes", length(out1))
end
#  TestBestSpeed tests that round-tripping through deflate and then inflate
#  recovers the original input. The Write sizes are near the thresholds in the
#  compressor.encSpeed method (0, 16, 128), as well as near maxStoreBlockSize
#  (65535).
function TestBestSpeed(t::testing.T)
    t.Parallel()
    abc = make(Vector{UInt8}, 128)
    for (i) in abc
        abc[i] = UInt8(i)
    end

    abcabc = bytes.Repeat(abc, 131072 / length(abc))
    local want::Vector{UInt8}

    testCases = Vector{Vector{Int}}[
        [65536, 0],
        [65536, 1],
        [65536, 1, 256],
        [65536, 1, 65536],
        [65536, 14],
        [65536, 15],
        [65536, 16],
        [65536, 16, 256],
        [65536, 16, 65536],
        [65536, 127],
        [65536, 128],
        [65536, 128, 256],
        [65536, 128, 65536],
        [65536, 129],
        [65536, 65536, 256],
        [65536, 65536, 65536],
    ]

    for (i, tc) in testCases
        if i >= 3 && testing.Short()
            break
        end

        for (_, firstN) in Vector{Int}[1, 65534, 65535, 65536, 65537, 131072]

            tc[0] = firstN
            @label outer
            for (_, flush) in Vector{Bool}[false, true]

                buf = new(bytes.Buffer)
                want = want[begin:0]
                w, err = NewWriter(buf, BestSpeed)
                if err !== nothing
                    t.Errorf(
                        "i=%d, firstN=%d, flush=%t: NewWriter: %v",
                        i,
                        firstN,
                        flush,
                        err,
                    )
                    continue
                end

                for (_, n) in tc
                    want = append(want, abcabc[begin:n]...)
                    if (_, err = w.Write(abcabc[begin:n]); err !== nothing)
                        t.Errorf(
                            "i=%d, firstN=%d, flush=%t: Write: %v",
                            i,
                            firstN,
                            flush,
                            err,
                        )
                        @goto GoTranspiler.Identifier("outer")
                    end

                    if !flush
                        continue
                    end

                    if (err = w.Flush(); err !== nothing)
                        t.Errorf(
                            "i=%d, firstN=%d, flush=%t: Flush: %v",
                            i,
                            firstN,
                            flush,
                            err,
                        )
                        @goto GoTranspiler.Identifier("outer")
                    end

                end

                if (err = w.Close(); err !== nothing)
                    t.Errorf("i=%d, firstN=%d, flush=%t: Close: %v", i, firstN, flush, err)
                    continue
                end

                r = NewReader(buf)
                got, err = io.ReadAll(r)
                if err !== nothing
                    t.Errorf(
                        "i=%d, firstN=%d, flush=%t: ReadAll: %v",
                        i,
                        firstN,
                        flush,
                        err,
                    )
                    continue
                end

                r.Close()
                if !bytes.Equal(got, want)
                    t.Errorf(
                        "i=%d, firstN=%d, flush=%t: corruption during deflate-then-inflate",
                        i,
                        firstN,
                        flush,
                    )
                    continue
                end

            end

        end

    end

end
#  failWriter fails with errIO exactly at the nth call to Write.
errIO = errors.New("IO error")
mutable struct failWriter
    n::Int
end

function Write(w::failWriter, b::Vector{UInt8})# ::Tuple{Int, error}
    w.n -= 1
    if w.n == -1
        return 0, errIO
    end

    return length(b), nothing
end
function TestWriterPersistentWriteError(t::testing.T)
    t.Parallel()
    d, err = os.ReadFile("../../testdata/Isaac.Newton-Opticks.txt")
    if err !== nothing
        t.Fatalf("ReadFile: %v", err)
    end

    #  Keep this test short
    d = d[begin:10000]
    zw, err = NewWriter(nothing, DefaultCompression)
    if err !== nothing
        t.Fatalf("NewWriter: %v", err)
    end

    i = 0
    while i < 1000
        fw = failWriter[i]

        zw.Reset(fw)
        _, werr = zw.Write(d)
        cerr = zw.Close()
        ferr = zw.Flush()
        if werr != errIO && werr !== nothing
            t.Errorf("test %d, mismatching Write error: got %v, want %v", i, werr, errIO)
        end

        if cerr != errIO && fw.n < 0
            t.Errorf("test %d, mismatching Close error: got %v, want %v", i, cerr, errIO)
        end

        if ferr != errIO && fw.n < 0
            t.Errorf("test %d, mismatching Flush error: got %v, want %v", i, ferr, errIO)
        end

        if fw.n >= 0
            #  At this point, the failure threshold was sufficiently high enough
            #  that we wrote the whole stream without any errors.
            return
        end

        i += 1
    end

end
function TestWriterPersistentFlushError(t::testing.T)
    zw, err = NewWriter(failWriter[0], DefaultCompression)
    if err !== nothing
        t.Fatalf("NewWriter: %v", err)
    end

    flushErr = zw.Flush()
    closeErr = zw.Close()
    _, writeErr = zw.Write(convert(Vector{UInt8}, "Test"))
    checkErrors(Vector{error}[closeErr, flushErr, writeErr], errIO, t)
end
function TestWriterPersistentCloseError(t::testing.T)
    #  If underlying writer return error on closing stream we should persistent this error across all writer calls.
    zw, err = NewWriter(failWriter[0], DefaultCompression)
    if err !== nothing
        t.Fatalf("NewWriter: %v", err)
    end

    closeErr = zw.Close()
    flushErr = zw.Flush()
    _, writeErr = zw.Write(convert(Vector{UInt8}, "Test"))
    checkErrors(Vector{error}[closeErr, flushErr, writeErr], errIO, t)
    #  After closing writer we should persistent "write after close" error across Flush and Write calls, but return nil
    #  on next Close calls.
    local b::bytes.Buffer

    zw.Reset(b)
    err = zw.Close()
    if err !== nothing
        t.Fatalf("First call to close returned error: %s", err)
    end

    err = zw.Close()
    if err !== nothing
        t.Fatalf("Second call to close returned error: %s", err)
    end

    flushErr = zw.Flush()
    _, writeErr = zw.Write(convert(Vector{UInt8}, "Test"))
    checkErrors(Vector{error}[flushErr, writeErr], errWriterClosed, t)
end
function checkErrors(got::Vector{error}, want::error, t::testing.T)
    t.Helper()
    for (_, err) in got
        if err != want
            t.Errorf("Errors dosn't match\\nWant: %s\\nGot: %s", want, got)
        end

    end

end
function TestBestSpeedMatch(t::testing.T)
    t.Parallel()
    cases = Vector{
        NamedTuple{
            (previous, current, t, s, want),
            Tuple{Vector{UInt8},Vector{UInt8},Int32,Int32,Int32},
        },
    }[
        [
            previous = Vector{UInt8}[0, 0, 0, 1, 2],
            current = Vector{UInt8}[3, 4, 5, 0, 1, 2, 3, 4, 5],
            t = -3,
            s = 3,
            want = 6,
        ],
        [
            previous = Vector{UInt8}[0, 0, 0, 1, 2],
            current = Vector{UInt8}[2, 4, 5, 0, 1, 2, 3, 4, 5],
            t = -3,
            s = 3,
            want = 3,
        ],
        [
            previous = Vector{UInt8}[0, 0, 0, 1, 1],
            current = Vector{UInt8}[3, 4, 5, 0, 1, 2, 3, 4, 5],
            t = -3,
            s = 3,
            want = 2,
        ],
        [
            previous = Vector{UInt8}[0, 0, 0, 1, 2],
            current = Vector{UInt8}[2, 2, 2, 2, 1, 2, 3, 4, 5],
            t = -1,
            s = 0,
            want = 4,
        ],
        [
            previous = Vector{UInt8}[0, 0, 0, 1, 2, 3, 4, 5, 2, 2],
            current = Vector{UInt8}[2, 2, 2, 2, 1, 2, 3, 4, 5],
            t = -7,
            s = 4,
            want = 5,
        ],
        [
            previous = Vector{UInt8}[9, 9, 9, 9, 9],
            current = Vector{UInt8}[2, 2, 2, 2, 1, 2, 3, 4, 5],
            t = -1,
            s = 0,
            want = 0,
        ],
        [
            previous = Vector{UInt8}[9, 9, 9, 9, 9],
            current = Vector{UInt8}[9, 2, 2, 2, 1, 2, 3, 4, 5],
            t = 0,
            s = 1,
            want = 0,
        ],
        [
            previous = Vector{UInt8}[],
            current = Vector{UInt8}[9, 2, 2, 2, 1, 2, 3, 4, 5],
            t = -5,
            s = 1,
            want = 0,
        ],
        [
            previous = Vector{UInt8}[],
            current = Vector{UInt8}[9, 2, 2, 2, 1, 2, 3, 4, 5],
            t = -1,
            s = 1,
            want = 0,
        ],
        [
            previous = Vector{UInt8}[],
            current = Vector{UInt8}[2, 2, 2, 2, 1, 2, 3, 4, 5],
            t = 0,
            s = 1,
            want = 3,
        ],
        [
            previous = Vector{UInt8}[3, 4, 5],
            current = Vector{UInt8}[3, 4, 5],
            t = -3,
            s = 0,
            want = 3,
        ],
        [
            previous = make(Vector{UInt8}, 1000),
            current = make(Vector{UInt8}, 1000),
            t = -1000,
            s = 0,
            want = maxMatchLength - 4,
        ],
        [
            previous = make(Vector{UInt8}, 200),
            current = make(Vector{UInt8}, 500),
            t = -200,
            s = 0,
            want = maxMatchLength - 4,
        ],
        [
            previous = make(Vector{UInt8}, 200),
            current = make(Vector{UInt8}, 500),
            t = 0,
            s = 1,
            want = maxMatchLength - 4,
        ],
        [
            previous = make(Vector{UInt8}, maxMatchLength - 4),
            current = make(Vector{UInt8}, 500),
            t = -maxMatchLength - 4,
            s = 0,
            want = maxMatchLength - 4,
        ],
        [
            previous = make(Vector{UInt8}, 200),
            current = make(Vector{UInt8}, 500),
            t = -200,
            s = 400,
            want = 100,
        ],
        [
            previous = make(Vector{UInt8}, 10),
            current = make(Vector{UInt8}, 500),
            t = 200,
            s = 400,
            want = 100,
        ],
    ]

    for (i, c) in cases
        e = DeflateFast[prev = c.previous]

        got = e.matchLen(c.s, c.t, c.current)
        if got != c.want
            t.Errorf("Test %d: match length, want %d, got %d", i, c.want, got)
        end

    end

end
function TestBestSpeedMaxMatchOffset(t::testing.T)
    t.Parallel()
    const abc, xyz = "abcdefgh", "stuvwxyz"

    for (_, matchBefore) in Vector{Bool}[false, true]

        for (_, extra) in
            Vector{Int}[0, inputMargin-1, inputMargin, inputMargin+1, 2*inputMargin]

            offsetAdj = -5
            while offsetAdj <= +5
                report = (
                    (desc::String, err::error) -> begin
                        t.Errorf(
                            "matchBefore=%t, extra=%d, offsetAdj=%d: %s%v",
                            matchBefore,
                            extra,
                            offsetAdj,
                            desc,
                            err,
                        )
                    end
                )
                offset = maxMatchOffset + offsetAdj
                #  Make src to be a []byte of the form
                # 	"%s%s%s%s%s" % (abc, zeros0, xyzMaybe, abc, zeros1)
                #  where:
                # 	zeros0 is approximately maxMatchOffset zeros.
                # 	xyzMaybe is either xyz or the empty string.
                # 	zeros1 is between 0 and 30 zeros.
                #  The difference between the two abc's will be offset, which
                #  is maxMatchOffset plus or minus a small adjustment.
                src = make(Vector{UInt8}, offset + length(abc) + extra)
                copy(src, abc)
                if !matchBefore
                    copy(src[offset-length(xyz), :], xyz)
                end

                copy(src[offset, :], abc)
                buf = new(bytes.Buffer)
                w, err = NewWriter(buf, BestSpeed)
                if err !== nothing
                    report("NewWriter: ", err)
                    continue
                end

                if (_, err = w.Write(src); err !== nothing)
                    report("Write: ", err)
                    continue
                end

                if (err = w.Close(); err !== nothing)
                    report("Writer.Close: ", err)
                    continue
                end

                r = NewReader(buf)
                dst, err = io.ReadAll(r)
                r.Close()
                if err !== nothing
                    report("ReadAll: ", err)
                    continue
                end

                if !bytes.Equal(dst, src)
                    report("", error("bytes differ after round-tripping"))
                    continue
                end

                offsetAdj += 1
            end

        end

    end

end
function TestBestSpeedShiftOffsets(t::testing.T)
    #  Test if shiftoffsets properly preserves matches and resets out-of-range matches
    #  seen in https://github.com/golang/go/issues/4142
    enc = newDeflateFast()
    #  testData may not generate internal matches.
    testData = make(Vector{UInt8}, 32)
    rng = rand.New(rand.NewSource(0))
    for (i) in testData
        testData[i] = UInt8(rng.Uint32())
    end

    #  Encode the testdata with clean state.
    #  Second part should pick up matches from the first block.
    wantFirstTokens = length(enc.encode(nothing, testData))
    wantSecondTokens = length(enc.encode(nothing, testData))
    if wantFirstTokens <= wantSecondTokens
        t.Fatalf("test needs matches between inputs to be generated")
    end

    #  Forward the current indicator to before wraparound.
    enc.cur = bufferReset - Int32(length(testData))
    #  Part 1 before wrap, should match clean state.
    got = length(enc.encode(nothing, testData))
    if wantFirstTokens != got
        t.Errorf("got %d, want %d tokens", got, wantFirstTokens)
    end

    #  Verify we are about to wrap.
    if enc.cur != bufferReset
        t.Errorf("got %d, want e.cur to be at bufferReset (%d)", enc.cur, bufferReset)
    end

    #  Part 2 should match clean state as well even if wrapped.
    got = length(enc.encode(nothing, testData))
    if wantSecondTokens != got
        t.Errorf("got %d, want %d token", got, wantSecondTokens)
    end

    #  Verify that we wrapped.
    if enc.cur >= bufferReset
        t.Errorf("want e.cur to be < bufferReset (%d), got %d", bufferReset, enc.cur)
    end

    #  Forward the current buffer, leaving the matches at the bottom.
    enc.cur = bufferReset
    enc.shiftOffsets()
    #  Ensure that no matches were picked up.
    got = length(enc.encode(nothing, testData))
    if wantFirstTokens != got
        t.Errorf("got %d, want %d tokens", got, wantFirstTokens)
    end

end
function TestMaxStackSize(t::testing.T)
    #  This test must not run in parallel with other tests as debug.SetMaxStack
    #  affects all goroutines.
    n = debug.SetMaxStack(1 << 16)
    @defer debug.SetMaxStack(n)
    local wg::sync.WaitGroup

    @defer wg.Wait()
    b = make(Vector{UInt8}, 1 << 20)
    level = HuffmanOnly
    while level <= BestCompression
        #  Run in separate goroutine to increase probability of stack regrowth.
        wg.Add(1)
        Threads.@spawn begin
            (
                (level::Int) -> begin
                    @defer wg.Done()
                    zw, err = NewWriter(io.Discard, level)
                    if err !== nothing
                        t.Errorf("level %d, NewWriter() = %v, want nil", level, err)
                    end

                    if (n, err = zw.Write(b); n != length(b) || err !== nothing)
                        t.Errorf(
                            "level %d, Write() = (%d, %v), want (%d, nil)",
                            level,
                            n,
                            err,
                            length(b),
                        )
                    end

                    if (err = zw.Close(); err !== nothing)
                        t.Errorf("level %d, Close() = %v, want nil", level, err)
                    end

                    zw.Reset(io.Discard)
                end
            )(
                level,
            )
        end

        level += 1
    end

end
end # module
