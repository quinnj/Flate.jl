using Flate, Test, Flate.GoTypes

struct deflateTest
    in::Go.Slice{UInt8}
    level::Int
    out::Go.Slice{UInt8}
end

struct deflateInflateTest
    in::Go.Slice{UInt8}
end

struct reverseBitsTest
    in::UInt16
    bitCount::UInt8
    out::UInt16
end

function largeDataChunk()# ::Go.Slice{UInt8}
    result = Go.Slice(UInt8, 100000)
    for i in eachindex(result)
        result[i] = UInt8(i * i & 255)
    end
    return result
end

deflateTests = deflateTest[
    deflateTest(Go.Slice(UInt8[]), 0, Go.Slice(UInt8[1, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17]), -1, Go.Slice(UInt8[18, 4, 4, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17]), Flate.DefaultCompression, Go.Slice(UInt8[18, 4, 4, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17]), 4, Go.Slice(UInt8[18, 4, 4, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17]), 0, Go.Slice(UInt8[0, 1, 0, 254, 255, 17, 1, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17, 18]), 0, Go.Slice(UInt8[0, 2, 0, 253, 255, 17, 18, 1, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17, 17, 17, 17, 17, 17, 17, 17]), 0, Go.Slice(UInt8[0, 8, 0, 247, 255, 17, 17, 17, 17, 17, 17, 17, 17, 1, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[]), 2, Go.Slice(UInt8[1, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17]), 2, Go.Slice(UInt8[18, 4, 4, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17, 18]), 2, Go.Slice(UInt8[18, 20, 2, 4, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17, 17, 17, 17, 17, 17, 17, 17]), 2, Go.Slice(UInt8[18, 132, 2, 64, 0, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[]), 9, Go.Slice(UInt8[1, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17]), 9, Go.Slice(UInt8[18, 4, 4, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17, 18]), 9, Go.Slice(UInt8[18, 20, 2, 4, 0, 0, 255, 255])),
    deflateTest(Go.Slice(UInt8[17, 17, 17, 17, 17, 17, 17, 17]), 9, Go.Slice(UInt8[18, 132, 2, 64, 0, 0, 0, 255, 255])),
]

deflateInflateTests = deflateInflateTest[
    deflateInflateTest(Go.Slice(UInt8[])),
    deflateInflateTest(Go.Slice(UInt8[17])),
    deflateInflateTest(Go.Slice(UInt8[17, 18])),
    deflateInflateTest(Go.Slice(UInt8[17, 17, 17, 17, 17, 17, 17, 17])),
    deflateInflateTest(Go.Slice(UInt8[17, 16, 19, 65, 33, 33, 65, 19, 135, 120, 19])),
    deflateInflateTest(largeDataChunk()),
]

reverseBitsTests = reverseBitsTest[
    reverseBitsTest(1, 1, 1),
    reverseBitsTest(1, 2, 2),
    reverseBitsTest(1, 3, 4),
    reverseBitsTest(1, 4, 8),
    reverseBitsTest(1, 5, 16),
    reverseBitsTest(17, 5, 17),
    reverseBitsTest(257, 9, 257),
    reverseBitsTest(29, 5, 23),
]

@testset "TestBulkHash4" begin
    for x in deflateTests
        y = x.out
        if length(y) < Flate.minMatchLength
            continue
        end
        y = Go.append(y, y...)
        for j = 4:(length(y) - 1)
            y = y[begin:j]
            dst = Go.Slice(UInt32, length(y) - Flate.minMatchLength + 1)
            for i in eachindex(dst)
                dst[i] = UInt32(i + 100)
            end
            Flate.bulkHash4(y, dst)
            for (i, got) in Go.each(dst)
                want = Flate.hash4(y[i, :])
                @test got == want
            end
        end
    end
end

@testset "TestDeflate" begin
    for h in deflateTests
        buf = IOBuffer()
        w = Flate.NewWriter(buf, h.level)
        write(w, h.in)
        close(w)
        @test Go.Slice(buf) == h.out
    end
end

@testset "TestWriterClose" begin
    b = IOBuffer()
    zw = Flate.NewWriter(b, 6)
    c = Flate.write(zw, Go.Slice("Test"))
    @test c == 4
    close(zw)
    afterClose = position(b)
    # error to write to closed writer
    # c = Flate.write(zw, Go.Slice("Test"))
    # error to flush closed writer
    # flush(zw)
    # no error to close closed writer
    # close(zw)
    # no additional writes happen on closed writer
    @test afterClose == position(b)
end

#  A sparseReader returns a stream consisting of 0s followed by 1<<16 1s.
#  This tests missing hash references in a very large input.
# mutable struct sparseReader
#     l::Int64
#     cur::Int64
# end

# function Read(r::sparseReader, b::Go.Slice{UInt8})# ::Tuple{n::Int, err::error}
#     if r.cur >= r.l
#         return 0, io.EOF
#     end

#     n = length(b)
#     cur = r.cur + Int64(n)
#     if cur > r.l
#         n -= Int(cur - r.l)
#         cur = r.l
#     end

#     for (i) in b[0:n]
#         if r.cur + Int64(i) >= r.l - 1 << 16
#             b[i] = 1
#         else
#             b[i] = 0
#         end

#     end

#     r.cur = cur
#     return
# end

# @testset "TestVeryLongSparseChunk" begin
#     w = Flate.NewWriter(devnull, 1)
#     if (_, err = io.Copy(w, sparseReader[l = 2.3e9]); err !== nothing)
#         t.Errorf("Compress failed: %v", err)
#         return
#     end
# end

mutable struct syncBuffer
    buf::IOBuffer
    mu::ReentrantLock
    closed::Bool
    ready::Channel{Bool}
end

function newSyncBuffer()# ::syncBuffer
    return syncBuffer(IOBuffer(), ReentrantLock(), false, Channel{Bool}(1))
end

function Read(b::syncBuffer, p::Go.Slice{UInt8})# ::Tuple{n::Int, err::error}
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
    put!(b.ready, true)
end

function Write(b::syncBuffer, p::Go.Slice{UInt8})# ::Tuple{n::Int, err::error}
    write(b.buf, p)
    signal(b)
    return
end

function WriteMode(b::syncBuffer)
    lock(b.mu)
end

function ReadMode(b::syncBuffer)
    unlock(b.mu)
    signal(b)
end

function Close(b::syncBuffer)# ::error
    b.closed = true
    signal(b)
    return
end

# function testSync(level::Int, input::Go.Slice{UInt8}, name::String)
#     if length(input) == 0
#         return
#     end
#     @info("--testSync $level, $(length(input)), $name")
#     buf = newSyncBuffer()
#     buf1 = IOBuffer()
#     WriteMode(buf)
#     w = Flate.NewWriter(io.MultiWriter(buf, buf1), level)
#     r = NewReader(buf)
#     i = 0
#     while i < 2
#         local lo::Int, hi::Int
#         if i == 0
#             lo, hi = 0, length(input) + 1 / 2
#         else
#             lo, hi = length(input) + 1 / 2, length(input)
#         end
#         t.Logf("#%d: write %d-%d", i, lo, hi)
#         if (_, err = write(w, input[lo:hi]); err !== nothing)
#             t.Errorf("testSync: write: %v", err)
#             return
#         end
#         if i == 0
#             if (err = flush(w); err !== nothing)
#                 t.Errorf("testSync: flush: %v", err)
#                 return
#             end
#         else
#             if (err = close(w); err !== nothing)
#                 t.Errorf("testSync: close: %v", err)
#             end

#         end
#         buf.ReadMode()
#         out = Go.Slice(UInt8, hi - lo + 1)
#         m, err = io.ReadAtLeast(r, out, hi - lo)
#         t.Logf("#%d: read %d", i, m)
#         if m != hi - lo || err !== nothing
#             t.Errorf(
#                 "testSync/%d (%d, %d, %s): read %d: %d, %v (%d left)",
#                 i,
#                 level,
#                 length(input),
#                 name,
#                 hi - lo,
#                 m,
#                 err,
#                 buf.buf.Len(),
#             )
#             return
#         end
#         if !bytes.Equal(input[lo:hi], out[begin:hi-lo])
#             t.Errorf(
#                 "testSync/%d: read wrong bytes: %x vs %x",
#                 i,
#                 input[lo:hi],
#                 out[begin:hi-lo],
#             )
#             return
#         end
#         #  This test originally checked that after reading
#         #  the first half of the input, there was nothing left
#         #  in the read buffer (buf.buf.Len() != 0) but that is
#         #  not necessarily the case: the write Flush may emit
#         #  some extra framing bits that are not necessary
#         #  to process to obtain the first half of the uncompressed
#         #  data. The test ran correctly most of the time, because
#         #  the background goroutine had usually read even
#         #  those extra bits by now, but it's not a useful thing to
#         #  check.
#         buf.WriteMode()
#         i += 1
#     end
#     buf.ReadMode()
#     out = Go.Slice(UInt8, 10)
#     if (n, err = r.Read(out); n > 0 || err != io.EOF)
#         t.Errorf(
#             "testSync (%d, %d, %s): final Read: %d, %v (hex: %x)",
#             level,
#             length(input),
#             name,
#             n,
#             err,
#             out[0:n],
#         )
#     end
#     if buf.buf.Len() != 0
#         t.Errorf("testSync (%d, %d, %s): extra data at end", level, length(input), name)
#     end
#     r.Close()
#     #  stream should work for ordinary reader too
#     r = NewReader(buf1)
#     out, err = io.ReadAll(r)
#     if err !== nothing
#         t.Errorf("testSync: read: %s", err)
#         return
#     end
#     r.Close()
#     if !bytes.Equal(input, out)
#         t.Errorf(
#             "testSync: decompress(compress(data)) != data: level=%d input=%s",
#             level,
#             name,
#         )
#     end
# end

# function testToFromWithLevelAndLimit(
#     t::testing.T,
#     level::Int,
#     input::Go.Slice{UInt8},
#     name::String,
#     limit::Int,
# )
#     local buffer::bytes.Buffer
#     w = Flate.NewWriter(buffer, level)
#     write(w, input)
#     close(w)
#     if limit > 0 && buffer.Len() > limit
#         t.Errorf(
#             "level: %d, length(compress(data)) = %d > limit = %d",
#             level,
#             buffer.Len(),
#             limit,
#         )
#         return
#     end
#     if limit > 0
#         t.Logf(
#             "level: %d, size:%.2f%%, %d b\\n",
#             level,
#             Float64(buffer.Len() * 100) / Float64(limit),
#             buffer.Len(),
#         )
#     r = NewReader(buffer)
#     out, err = io.ReadAll(r)
#     if err !== nothing
#         t.Errorf("read: %s", err)
#         return
#     end
#     r.Close()
#     if !bytes.Equal(input, out)
#         t.Errorf("decompress(compress(data)) != data: level=%d input=%s", level, name)
#         return
#     end
#     testSync(t, level, input, name)
# end

# function testToFromWithLimit(
#     t::testing.T,
#     input::Go.Slice{UInt8},
#     name::String,
#     limit::Go.Slice{Int},
# )
#     i = 0
#     while i < 10
#         testToFromWithLevelAndLimit(t, i, input, name, limit[i])
#         i += 1
#     end
#     #  Test HuffmanCompression
#     testToFromWithLevelAndLimit(t, -2, input, name, limit[10])
# end

# @testset "TestDeflateInflate" begin
#     t.Parallel()
#     for (i, h) in deflateInflateTests
#         if testing.Short() && length(h.in) > 10000
#             continue
#         end
#         testToFromWithLimit(t, h.in, string("#%d", i), Go.Slice{Int}[])
#     end
# end

@testset "TestReverseBits" begin
    for h in reverseBitsTests
        @test Flate.reverseBits(h.in, h.bitCount) == h.out
    end
end

mutable struct deflateInflateStringTest
    filename::String
    label::String
    limit::Go.Slice{Int}
end

deflateInflateStringTests = deflateInflateStringTest[
    deflateInflateStringTest(
        "../testdata/e.txt",
        "2.718281828...",
        Go.Slice(Int[100018, 50650, 50960, 51150, 50930, 50790, 50790, 50790, 50790, 50790, 43683]),
    ),
    deflateInflateStringTest(
        "../../testdata/Isaac.Newton-Opticks.txt",
        "Isaac.Newton-Opticks",
        Go.Slice(Int[567248, 218338, 198211, 193152, 181100, 175427, 175427, 173597, 173422, 173422, 325240]),
    ),
]

# @testset "TestDeflateInflateString" begin
#     for (_, test) in deflateInflateStringTests
#         gold = read(test.filename)
#         testToFromWithLimit(t, gold, test.label, test.limit)
#     end
# end

# @testset "TestReaderDict" begin
#     dict = "hello world"
#     text = "hello again world"
#     b = IOBuffer()
#     w = Flate.NewWriter(b, 5)
#     write(w, Go.Slice(dict))
#     flush(w)
#     seekstart(b)
#     write(w, Go.Slice(text))
#     close(w)
#     r = Flate.NewReaderDict(b, Go.Slice(dict))
#     data = readavailable(r)
#     if String(data) != "hello again world"
#         t.Fatalf("read returned %q want %q", String(data), text)
#     end
# end

@testset "TestWriterDict" begin
    dict = "hello world"
    text = "hello again world"
    b = IOBuffer()
    w = Flate.NewWriter(b, 5)
    write(w, Go.Slice(dict))
    flush(w)
    seekstart(b)
    write(w, Go.Slice(text))
    close(w)
    b1 = IOBuffer()
    w = Flate.NewWriterDict(b1, 5, Go.Slice(dict))
    write(w, Go.Slice(text))
    close(w)
    bb = take!(b)
    bb1 = take!(b1)
    @test_broken bb == bb1
end

#  See https://golang.org/issue/2508
@testset "TestRegression2508" begin
    w = Flate.NewWriter(devnull, 1)
    buf = Go.Slice(UInt8, 1024)
    for i = 0:131071
        write(w, buf)
    end
    close(w)
    @test true
end

function testResetOutput(level::Int, dict::Union{Nothing, Go.Slice{UInt8}})
    writeData = (
        (w::Flate.Writer) -> begin
            msg = Go.Slice("now is the time for all good gophers")
            write(w, msg)
            flush(w)
            hello = Go.Slice("hello world")
            for _ = 0:1023
                write(w, hello)
            end
            fill = Go.Slice(repeat("x", 65000))
            write(w, fill)
        end
    )
    buf = IOBuffer()
    local w::Flate.Writer
    if dict === nothing
        w = Flate.NewWriter(buf, level)
    else
        w = NewWriterDict(buf, level, dict)
    end
    writeData(w)
    close(w)
    out1 = take!(buf)
    buf2 = IOBuffer()
    reset(w, buf2)
    writeData(w)
    close(w)
    out2 = take!(buf2)
    @test length(out1) == length(out2)
    @test out1 == out2
    @info("got $(length(out1)) bytes")
end

@testset "TestWriterReset" begin
    for level = 0:9
        w = Flate.NewWriter(devnull, level)
        buf = Go.Slice("hello world")
        n = 1024
        for _ = 0:(n - 1)
            write(w, buf)
        end
        reset(w, devnull)
        wref = Flate.NewWriter(devnull, level)
        #  hashMatch is always overwritten when used.
        copy(w.d.hashMatch[:], wref.d.hashMatch[:])
        @test length(w.d.tokens) == 0
        #  As long as the length is 0, we don't care about the content.
        w.d.tokens = wref.d.tokens
        #  We don't care if there are values in the window, as long as it is at d.index is 0
        w.d.window = wref.d.window
        @test w == wref
    end
    levels = Go.Slice(Int[0, 1, 2, 5, 9])

    @sync for level in levels
        Threads.@spawn testResetOutput(level, nothing)
    end
end

#  TestBestSpeed tests that round-tripping through deflate and then inflate
#  recovers the original input. The Write sizes are near the thresholds in the
#  compressor.encSpeed method (0, 16, 128), as well as near maxStoreBlockSize
#  (65535).
# @testset "TestBestSpeed" begin
#     t.Parallel()
#     abc = Go.Slice(UInt8, 128)
#     for (i) in abc
#         abc[i] = UInt8(i)
#     end

#     abcabc = bytes.Repeat(abc, 131072 / length(abc))
#     local want::Go.Slice{UInt8}

#     testCases = Go.Slice{Go.Slice{Int}}[
#         [65536, 0],
#         [65536, 1],
#         [65536, 1, 256],
#         [65536, 1, 65536],
#         [65536, 14],
#         [65536, 15],
#         [65536, 16],
#         [65536, 16, 256],
#         [65536, 16, 65536],
#         [65536, 127],
#         [65536, 128],
#         [65536, 128, 256],
#         [65536, 128, 65536],
#         [65536, 129],
#         [65536, 65536, 256],
#         [65536, 65536, 65536],
#     ]

#     for (i, tc) in testCases
#         if i >= 3 && testing.Short()
#             break
#         end

#         for (_, firstN) in Go.Slice{Int}[1, 65534, 65535, 65536, 65537, 131072]

#             tc[0] = firstN
#             @label outer
#             for (_, flush) in Go.Slice{Bool}[false, true]

#                 buf = IOBuffer()
#                 want = want[begin:0]
#                 w = Flate.NewWriter(buf, BestSpeed)
#                 if err !== nothing
#                     t.Errorf(
#                         "i=%d, firstN=%d, flush=%t: NewWriter: %v",
#                         i,
#                         firstN,
#                         flush,
#                         err,
#                     )
#                     continue
#                 end

#                 for (_, n) in tc
#                     want = Go.append(want, abcabc[begin:n]...)
#                     if (_, err = write(w, abcabc[begin:n]); err !== nothing)
#                         t.Errorf(
#                             "i=%d, firstN=%d, flush=%t: Write: %v",
#                             i,
#                             firstN,
#                             flush,
#                             err,
#                         )
#                         @goto GoTranspiler.Identifier("outer")
#                     end

#                     if !flush
#                         continue
#                     end

#                     if (err = flush(w); err !== nothing)
#                         t.Errorf(
#                             "i=%d, firstN=%d, flush=%t: Flush: %v",
#                             i,
#                             firstN,
#                             flush,
#                             err,
#                         )
#                         @goto GoTranspiler.Identifier("outer")
#                     end

#                 end

#                 if (err = close(w); err !== nothing)
#                     t.Errorf("i=%d, firstN=%d, flush=%t: Close: %v", i, firstN, flush, err)
#                     continue
#                 end

#                 r = NewReader(buf)
#                 got, err = io.ReadAll(r)
#                 if err !== nothing
#                     t.Errorf(
#                         "i=%d, firstN=%d, flush=%t: ReadAll: %v",
#                         i,
#                         firstN,
#                         flush,
#                         err,
#                     )
#                     continue
#                 end

#                 r.Close()
#                 if !bytes.Equal(got, want)
#                     t.Errorf(
#                         "i=%d, firstN=%d, flush=%t: corruption during deflate-then-inflate",
#                         i,
#                         firstN,
#                         flush,
#                     )
#                     continue
#                 end

#             end

#         end

#     end

# end

#  failWriter fails with errIO exactly at the nth call to Write.
# errIO = errors.New("IO error")
# mutable struct failWriter
#     n::Int
# end

# function Write(w::failWriter, b::Go.Slice{UInt8})# ::Tuple{Int, error}
#     w.n -= 1
#     if w.n == -1
#         return 0, errIO
#     end

#     return length(b), nothing
# end
# @testset "TestWriterPersistentWriteError" begin
#     t.Parallel()
#     d, err = os.ReadFile("../../testdata/Isaac.Newton-Opticks.txt")
#     if err !== nothing
#         t.Fatalf("ReadFile: %v", err)
#     end

#     #  Keep this test short
#     d = d[begin:10000]
#     zw = Flate.NewWriter(nothing, DefaultCompression)

#     i = 0
#     while i < 1000
#         fw = failWriter[i]

#         zw.Reset(fw)
#         _, werr = zw.Write(d)
#         cerr = zw.Close()
#         ferr = zw.Flush()
#         if werr != errIO && werr !== nothing
#             t.Errorf("test %d, mismatching Write error: got %v, want %v", i, werr, errIO)
#         end

#         if cerr != errIO && fw.n < 0
#             t.Errorf("test %d, mismatching Close error: got %v, want %v", i, cerr, errIO)
#         end

#         if ferr != errIO && fw.n < 0
#             t.Errorf("test %d, mismatching Flush error: got %v, want %v", i, ferr, errIO)
#         end

#         if fw.n >= 0
#             #  At this point, the failure threshold was sufficiently high enough
#             #  that we wrote the whole stream without any errors.
#             return
#         end

#         i += 1
#     end

# end
# @testset "TestWriterPersistentFlushError" begin
#     zw = Flate.NewWriter(failWriter[0], DefaultCompression)

#     flushErr = zw.Flush()
#     closeErr = zw.Close()
#     _, writeErr = zw.Write(Go.Slice("Test"))
#     checkErrors(Go.Slice{error}[closeErr, flushErr, writeErr], errIO, t)
# end
# @testset "TestWriterPersistentCloseError" begin
#     #  If underlying writer return error on closing stream we should persistent this error across all writer calls.
#     zw = Flate.NewWriter(failWriter[0], DefaultCompression)

#     closeErr = zw.Close()
#     flushErr = zw.Flush()
#     _, writeErr = zw.Write(Go.Slice("Test"))
#     checkErrors(Go.Slice{error}[closeErr, flushErr, writeErr], errIO, t)
#     #  After closing writer we should persistent "write after close" error across Flush and Write calls, but return nil
#     #  on next Close calls.
#     b = IOBuffer()

#     zw.Reset(b)
#     err = zw.Close()
#     if err !== nothing
#         t.Fatalf("First call to close returned error: %s", err)
#     end

#     err = zw.Close()
#     if err !== nothing
#         t.Fatalf("Second call to close returned error: %s", err)
#     end

#     flushErr = zw.Flush()
#     _, writeErr = zw.Write(Go.Slice("Test"))
#     checkErrors(Go.Slice{error}[flushErr, writeErr], errWriterClosed, t)
# end
# function checkErrors(got::Go.Slice{error}, want::error, t::testing.T)
#     t.Helper()
#     for (_, err) in got
#         if err != want
#             t.Errorf("Errors dosn't match\\nWant: %s\\nGot: %s", want, got)
#         end

#     end

# end

@testset "TestBestSpeedMatch" begin
    cases = Go.Slice([
        (
            previous = Go.Slice(UInt8[0, 0, 0, 1, 2]),
            current = Go.Slice(UInt8[3, 4, 5, 0, 1, 2, 3, 4, 5]),
            t = -3,
            s = 3,
            want = 6,
        ),
        (
            previous = Go.Slice(UInt8[0, 0, 0, 1, 2]),
            current = Go.Slice(UInt8[2, 4, 5, 0, 1, 2, 3, 4, 5]),
            t = -3,
            s = 3,
            want = 3,
        ),
        (
            previous = Go.Slice(UInt8[0, 0, 0, 1, 1]),
            current = Go.Slice(UInt8[3, 4, 5, 0, 1, 2, 3, 4, 5]),
            t = -3,
            s = 3,
            want = 2,
        ),
        (
            previous = Go.Slice(UInt8[0, 0, 0, 1, 2]),
            current = Go.Slice(UInt8[2, 2, 2, 2, 1, 2, 3, 4, 5]),
            t = -1,
            s = 0,
            want = 4,
        ),
        (
            previous = Go.Slice(UInt8[0, 0, 0, 1, 2, 3, 4, 5, 2, 2]),
            current = Go.Slice(UInt8[2, 2, 2, 2, 1, 2, 3, 4, 5]),
            t = -7,
            s = 4,
            want = 5,
        ),
        (
            previous = Go.Slice(UInt8[9, 9, 9, 9, 9]),
            current = Go.Slice(UInt8[2, 2, 2, 2, 1, 2, 3, 4, 5]),
            t = -1,
            s = 0,
            want = 0,
        ),
        (
            previous = Go.Slice(UInt8[9, 9, 9, 9, 9]),
            current = Go.Slice(UInt8[9, 2, 2, 2, 1, 2, 3, 4, 5]),
            t = 0,
            s = 1,
            want = 0,
        ),
        (
            previous = Go.Slice(UInt8[]),
            current = Go.Slice(UInt8[9, 2, 2, 2, 1, 2, 3, 4, 5]),
            t = -5,
            s = 1,
            want = 0,
        ),
        (
            previous = Go.Slice(UInt8[]),
            current = Go.Slice(UInt8[9, 2, 2, 2, 1, 2, 3, 4, 5]),
            t = -1,
            s = 1,
            want = 0,
        ),
        (
            previous = Go.Slice(UInt8[]),
            current = Go.Slice(UInt8[2, 2, 2, 2, 1, 2, 3, 4, 5]),
            t = 0,
            s = 1,
            want = 3,
        ),
        (
            previous = Go.Slice(UInt8[3, 4, 5]),
            current = Go.Slice(UInt8[3, 4, 5]),
            t = -3,
            s = 0,
            want = 3,
        ),
        (
            previous = Go.Slice(UInt8, 1000),
            current = Go.Slice(UInt8, 1000),
            t = -1000,
            s = 0,
            want = Flate.maxMatchLength - 4,
        ),
        (
            previous = Go.Slice(UInt8, 200),
            current = Go.Slice(UInt8, 500),
            t = -200,
            s = 0,
            want = Flate.maxMatchLength - 4,
        ),
        (
            previous = Go.Slice(UInt8, 200),
            current = Go.Slice(UInt8, 500),
            t = 0,
            s = 1,
            want = Flate.maxMatchLength - 4,
        ),
        (
            previous = Go.Slice(UInt8, Flate.maxMatchLength - 4),
            current = Go.Slice(UInt8, 500),
            t = -(Flate.maxMatchLength - 4),
            s = 0,
            want = Flate.maxMatchLength - 4,
        ),
        (
            previous = Go.Slice(UInt8, 200),
            current = Go.Slice(UInt8, 500),
            t = -200,
            s = 400,
            want = 100,
        ),
        (
            previous = Go.Slice(UInt8, 10),
            current = Go.Slice(UInt8, 500),
            t = 200,
            s = 400,
            want = 100,
        ),
    ])

    for c in cases
        e = Flate.DeflateFast(c.previous)
        got = Flate.matchLen(e, c.s, c.t, c.current)
        @test got == c.want
    end
end

# @testset "TestBestSpeedMaxMatchOffset" begin
#     abc, xyz = "abcdefgh", "stuvwxyz"
#     for matchBefore in (false, true)
#         for extra in (0, inputMargin-1, inputMargin, inputMargin+1, 2*inputMargin)
#             for offsetAdj = -5:5
#                 report = (
#                     (desc::String, err::error) -> begin
#                         t.Errorf(
#                             "matchBefore=%t, extra=%d, offsetAdj=%d: %s%v",
#                             matchBefore,
#                             extra,
#                             offsetAdj,
#                             desc,
#                             err,
#                         )
#                     end
#                 )
#                 offset = maxMatchOffset + offsetAdj
#                 #  Make src to be a []byte of the form
#                 # 	"%s%s%s%s%s" % (abc, zeros0, xyzMaybe, abc, zeros1)
#                 #  where:
#                 # 	zeros0 is approximately maxMatchOffset zeros.
#                 # 	xyzMaybe is either xyz or the empty string.
#                 # 	zeros1 is between 0 and 30 zeros.
#                 #  The difference between the two abc's will be offset, which
#                 #  is maxMatchOffset plus or minus a small adjustment.
#                 src = Go.Slice(UInt8, offset + sizeof(abc) + extra)
#                 copy(src, abc)
#                 if !matchBefore
#                     copy(src[offset-length(xyz), :], xyz)
#                 end
#                 copy(src[offset, :], abc)
#                 buf = IOBuffer()
#                 w = Flate.NewWriter(buf, Flate.BestSpeed)
#                 write(w, src)
#                 close(w)
#                 r = Flate.NewReader(buf)
#                 dst = readavailable(r)
#                 close(r)
#                 @test dst == src
#             end
#         end
#     end
# end

@testset "TestBestSpeedShiftOffsets" begin
    #  Test if shiftoffsets properly preserves matches and resets out-of-range matches
    #  seen in https://github.com/golang/go/issues/4142
    enc = Flate.newDeflateFast()
    #  testData may not generate internal matches.
    testData = Go.Slice(rand(UInt8, 32))
    #  Encode the testdata with clean state.
    #  Second part should pick up matches from the first block.
    wantFirstTokens = Go.len(Flate.encode(enc, Go.Slice(Flate.Token, 0), testData))
    wantSecondTokens = Go.len(Flate.encode(enc, Go.Slice(Flate.Token, 0), testData))
    @test wantFirstTokens > wantSecondTokens
    #  Forward the current indicator to before wraparound.
    enc.cur = Flate.bufferReset - Int32(Go.len(testData))
    #  Part 1 before wrap, should match clean state.
    got = Go.len(Flate.encode(enc, Go.Slice(Flate.Token, 0), testData))
    @test wantFirstTokens == got
    #  Verify we are about to wrap.
    @test enc.cur == Flate.bufferReset
    #  Part 2 should match clean state as well even if wrapped.
    got = Go.len(Flate.encode(enc, Go.Slice(Flate.Token, 0), testData))
    @test wantSecondTokens == got
    #  Verify that we wrapped.
    @test enc.cur < Flate.bufferReset
    #  Forward the current buffer, leaving the matches at the bottom.
    enc.cur = Flate.bufferReset
    Flate.shiftOffsets(enc)
    #  Ensure that no matches were picked up.
    got = Go.len(Flate.encode(enc, Go.Slice(Flate.Token, 0), testData))
    @test wantFirstTokens == got
end
