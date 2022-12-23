#  Copyright 2012 The Go Authors. All rights reserved.
#  Use of this source code is governed by a BSD-style
#  license that can be found in the LICENSE file.
module flate

function BenchmarkEncode(b::testing.B)
    doBench(
        b,
        (
            (b::testing.B, buf0::Vector{UInt8}, level::Int, n::Int) -> begin
                b.StopTimer()
                b.SetBytes(Int64(n))
                buf1 = make(Vector{UInt8}, n)
                i = 0
                while i < n
                    if length(buf0) > n - i
                        buf0 = buf0[begin:n-i]
                    end

                    copy(buf1[i, :], buf0)
                    i += length(buf0)
                end

                buf0 = nothing
                w, err = NewWriter(io.Discard, level)
                if err !== nothing
                    b.Fatal(err)
                end

                runtime.GC()
                b.StartTimer()
                i = 0
                while i < b.N
                    w.Reset(io.Discard)
                    w.Write(buf1)
                    w.Close()
                    i += 1
                end

            end
        ),
    )
end
#  errorWriter is a writer that fails after N writes.
mutable struct errorWriter
    N::Int
end

function Write(e::errorWriter, b::Vector{UInt8})# ::Tuple{Int, error}
    if e.N <= 0
        return 0, io.ErrClosedPipe
    end

    e.N -= 1
    return length(b), nothing
end
#  Test if errors from the underlying writer is passed upwards.
function TestWriteError(t::testing.T)
    t.Parallel()
    buf = new(bytes.Buffer)
    n = 65536
    if !testing.Short()
        n *= 4
    end

    i = 0
    while i < n
        fmt.Fprintf(buf, "asdasfasf%d%dfghfgujyut%dyutyu\\n", i, i, i)
        i += 1
    end

    in = buf.Bytes()
    #  We create our own buffer to control number of writes.
    copyBuffer = make(Vector{UInt8}, 128)
    l = 0
    while l < 10
        fail = 1
        while fail <= 256
            #  Fail after 'fail' writes
            ew = errorWriter[N = fail]

            w, err = NewWriter(ew, l)
            if err !== nothing
                t.Fatalf("NewWriter: level %d: %v", l, err)
            end

            n, err = io.CopyBuffer(
                w,
                NamedTuple{(Reader,),Tuple{io.Reader}}[bytes.NewBuffer(in)],
                copyBuffer,
            )
            if err=== nothing
                t.Fatalf("Level %d: Expected an error, writer was %#v", l, ew)
            end

            n2, err = w.Write(Vector{UInt8}[1, 2, 2, 3, 4, 5])
            if n2 != 0
                t.Fatal("Level", l, "Expected 0 length write, got", n)
            end

            if err=== nothing
                t.Fatal("Level", l, "Expected an error")
            end

            err = w.Flush()
            if err=== nothing
                t.Fatal("Level", l, "Expected an error on flush")
            end

            err = w.Close()
            if err=== nothing
                t.Fatal("Level", l, "Expected an error on close")
            end

            w.Reset(io.Discard)
            n2, err = w.Write(Vector{UInt8}[1, 2, 3, 4, 5, 6])
            if err !== nothing
                t.Fatal("Level", l, "Got unexpected error after reset:", err)
            end

            if n2 == 0
                t.Fatal("Level", l, "Got 0 length write, expected > 0")
            end

            if testing.Short()
                return
            end

            fail *= 2
        end

        l += 1
    end

end
#  Test if two runs produce identical results
#  even when writing different sizes to the Writer.
function TestDeterministic(t::testing.T)
    t.Parallel()
    i = 0
    while i <= 9
        t.Run(fmt.Sprint("L", i), ((t::testing.T) -> begin
            testDeterministic(i, t)
        end))
        i += 1
    end

    t.Run("LM2", ((t::testing.T) -> begin
        testDeterministic(-2, t)
    end))
end
function testDeterministic(i::Int, t::testing.T)
    t.Parallel()
    #  Test so much we cross a good number of block boundaries.
    length = maxStoreBlockSize * 30 + 500

    if testing.Short()
        length /= 10
    end

    #  Create a random, but compressible stream.
    rng = rand.New(rand.NewSource(1))
    t1 = make(Vector{UInt8}, length)
    for (i) in t1
        t1[i] = UInt8(rng.Int63() & 7)
    end

    #  Do our first encode.
    local b1::bytes.Buffer

    br = bytes.NewBuffer(t1)
    w, err = NewWriter(b1, i)
    if err !== nothing
        t.Fatal(err)
    end

    #  Use a very small prime sized buffer.
    cbuf = make(Vector{UInt8}, 787)
    _, err = io.CopyBuffer(w, NamedTuple{(Reader,),Tuple{io.Reader}}[br], cbuf)
    if err !== nothing
        t.Fatal(err)
    end

    w.Close()
    #  We choose a different buffer size,
    #  bigger than a maximum block, and also a prime.
    local b2::bytes.Buffer

    cbuf = make(Vector{UInt8}, 81761)
    br2 = bytes.NewBuffer(t1)
    w2, err = NewWriter(b2, i)
    if err !== nothing
        t.Fatal(err)
    end

    _, err = io.CopyBuffer(w2, NamedTuple{(Reader,),Tuple{io.Reader}}[br2], cbuf)
    if err !== nothing
        t.Fatal(err)
    end

    w2.Close()
    b1b = b1.Bytes()
    b2b = b2.Bytes()
    if !bytes.Equal(b1b, b2b)
        t.Errorf(
            "level %d did not produce deterministic result, result mismatch, length(a) = %d, length(b) = %d",
            i,
            length(b1b),
            length(b2b),
        )
    end

end
#  TestDeflateFast_Reset will test that encoding is consistent
#  across a warparound of the table offset.
#  See https://github.com/golang/go/issues/34121
function TestDeflateFast_Reset(t::testing.T)
    buf = new(bytes.Buffer)
    n = 65536
    i = 0
    while i < n
        fmt.Fprintf(buf, "asdfasdfasdfasdf%d%dfghfgujyut%dyutyu\\n", i, i, i)
        i += 1
    end

    #  This is specific to level 1.
    const level = 1

    in = buf.Bytes()
    offset = 1
    if testing.Short()
        offset = 256
    end

    #  We do an encode with a clean buffer to compare.
    local want::bytes.Buffer

    w, err = NewWriter(want, level)
    if err !== nothing
        t.Fatalf("NewWriter: level %d: %v", level, err)
    end

    #  Output written 3 times.
    w.Write(in)
    w.Write(in)
    w.Write(in)
    w.Close()
    while offset <= 256
        w, err = NewWriter(io.Discard, level)
        if err !== nothing
            t.Fatalf("NewWriter: level %d: %v", level, err)
        end

        i = 0
        while i < bufferReset - length(in) - offset - maxMatchOffset / maxMatchOffset
            #  skip ahead to where we are close to wrap around...
            w.d.reset(nothing)
            i += 1
        end

        local got::bytes.Buffer

        w.Reset(got)
        i = 0
        while i < 3
            _, err = w.Write(in)
            if err !== nothing
                t.Fatal(err)
            end

            i += 1
        end

        err = w.Close()
        if err !== nothing
            t.Fatal(err)
        end

        if !bytes.Equal(got.Bytes(), want.Bytes())
            t.Fatalf(
                "output did not match at wraparound, length(want)  = %d, length(got) = %d",
                want.Len(),
                got.Len(),
            )
        end

        offset *= 2
    end

end
end # module
