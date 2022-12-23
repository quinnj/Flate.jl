#  Copyright 2014 The Go Authors. All rights reserved.
#  Use of this source code is governed by a BSD-style
#  license that can be found in the LICENSE file.
module flate

function TestReset(t::testing.T)
    ss = Vector{String}["lorem ipsum izzle fo rizzle", "the quick brown fox jumped over"]

    deflated = make(Vector{bytes.Buffer}, 2)
    for (i, s) in ss
        w, _ = NewWriter(deflated[i], 1)
        w.Write(convert(Vector{UInt8}, s))
        w.Close()
    end

    inflated = make(Vector{bytes.Buffer}, 2)
    f = NewReader(deflated[0])
    io.Copy(inflated[0], f)
    (f::Resetter).Reset(deflated[1], nothing)
    io.Copy(inflated[1], f)
    f.Close()
    for (i, s) in ss
        if s != inflated[i].String()
            t.Errorf("inflated[%d]:\\ngot  %q\\nwant %q", i, inflated[i], s)
        end

    end

end
function TestReaderTruncated(t::testing.T)
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
function TestResetDict(t::testing.T)
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
end # module
