using Flate, Test, Flate.GoTypes

@testset "TestReset" begin
    ss = Go.Slice(["lorem ipsum izzle fo rizzle", "the quick brown fox jumped over"])
    deflated = Go.Slice([IOBuffer(), IOBuffer()])
    for (i, s) in Go.each(ss)
        w = Flate.NewWriter(deflated[i], 1)
        write(w, Go.Slice(s))
        close(w)
    end

    inflated = Go.Slice([IOBuffer(), IOBuffer()])
    f = Flate.NewReader(deflated[0])
    copy(inflated[0], f)
    reset(deflated[1], nothing)
    copy(inflated[1], f)
    close(f)
    for (i, s) in Go.each(ss)
        @test s == String(take!(inflated[i]))
    end
end

@testset "TestReaderTruncated" begin
    vectors = Vector{NamedTuple{(input, output),Tuple{String,String}}}[
        ["\\x00", ""],
        ["\\x00\\f", ""],
        ["\\x00\\f\\x00", ""],
        ["\\x00\\f\\x00\\xf3\\xff", ""],
        ["\\x00\\f\\x00\\xf3\\xffhello", "hello"],
        ["\\x00\\f\\x00\\xf3\\xffhello, world", "hello, world"],
        ["\\x02", ""],
        ["\\xf2H\\xcd", "He"],
        ["\\xf2H͙0a\\u0084\\t", "Hel\\x90\\x90\\x90\\x90\\x90"],
        ["\\xf2H͙0a\\u0084\\t\\x00", "Hel\\x90\\x90\\x90\\x90\\x90"],
    ]

    for (i, v) in vectors
        r = strings.NewReader(v.input)
        zr = NewReader(r)
        b, err = io.ReadAll(zr)
        if err != io.ErrUnexpectedEOF
            t.Errorf("test %d, error mismatch: got %v, want io.ErrUnexpectedEOF", i, err)
        end

        if String(b) != v.output
            t.Errorf("test %d, output mismatch: got %q, want %q", i, b, v.output)
        end
    end
end

@testset "TestResetDict" begin
    dict = convert(Vector{UInt8}, "the lorem fox")
    ss = Vector{String}["lorem ipsum izzle fo rizzle", "the quick brown fox jumped over"]

    deflated = make(Vector{bytes.Buffer}, length(ss))
    for (i, s) in ss
        w, _ = NewWriterDict(deflated[i], DefaultCompression, dict)
        w.Write(convert(Vector{UInt8}, s))
        w.Close()
    end

    inflated = make(Vector{bytes.Buffer}, length(ss))
    f = NewReader(nothing)
    for (i) in inflated
        (f::Resetter).Reset(deflated[i], dict)
        io.Copy(inflated[i], f)
    end

    f.Close()
    for (i, s) in ss
        if s != inflated[i].String()
            t.Errorf("inflated[%d]:\\ngot  %q\\nwant %q", i, inflated[i], s)
        end
    end
end
