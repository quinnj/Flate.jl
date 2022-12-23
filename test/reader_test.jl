#  Copyright 2012 The Go Authors. All rights reserved.
#  Use of this source code is governed by a BSD-style
#  license that can be found in the LICENSE file.
module flate

function TestNlitOutOfRange(t::testing.T)
    #  Trying to decode this bogus flate data, which has a Huffman table
    #  with nlit=288, should not error.
    io.Copy(
        io.Discard,
        NewReader(
            strings.NewReader(
                "\\xfc\\xfe\\x36\\xe7\\x5e\\x1c\\xef\\xb3\\x55\\x58\\x77\\xb6\\x56\\xb5\\x43\\xf4" +
                "\\x6f\\xf2\\xd2\\xe6\\x3d\\x99\\xa0\\x85\\x8c\\x48\\xeb\\xf8\\xda\\x83\\x04\\x2a" +
                "\\x75\\xc4\\xf8\\x0f\\x12\\x11\\xb9\\xb4\\x4b\\x09\\xa0\\xbe\\x8b\\x91\\x4c",
            ),
        ),
    )
end
suites = Vector{NamedTuple{(name, file),Tuple{String,String}}}[#  Digits is the digits of the irrational number e. Its decimal representation
    #  does not repeat, but there are only 10 possible digits, so it should be
    #  reasonably compressible.
    #  Newton is Isaac Newtons's educational text on Opticks.
    ["Digits", "../testdata/e.txt"],
    ["Newton", "../../testdata/Isaac.Newton-Opticks.txt"],
]

function BenchmarkDecode(b::testing.B)
    doBench(
        b,
        (
            (b::testing.B, buf0::Vector{UInt8}, level::Int, n::Int) -> begin
                b.ReportAllocs()
                b.StopTimer()
                b.SetBytes(Int64(n))
                compressed = new(bytes.Buffer)
                w, err = NewWriter(compressed, level)
                if err !== nothing
                    b.Fatal(err)
                end

                i = 0
                while i < n
                    if length(buf0) > n - i
                        buf0 = buf0[begin:n-i]
                    end

                    io.Copy(w, bytes.NewReader(buf0))
                    i += length(buf0)
                end

                w.Close()
                buf1 = compressed.Bytes()
                buf0, compressed, w = nothing, nothing, nothing
                runtime.GC()
                b.StartTimer()
                i = 0
                while i < b.N
                    io.Copy(io.Discard, NewReader(bytes.NewReader(buf1)))
                    i += 1
                end

            end
        ),
    )
end
levelTests = Vector{NamedTuple{(name, level),Tuple{String,Int}}}[
    ["Huffman", HuffmanOnly],
    ["Speed", BestSpeed],
    ["Default", DefaultCompression],
    ["Compression", BestCompression],
]

sizes = Vector{NamedTuple{(name, n),Tuple{String,Int}}}[
    ["1e4", 10000.0],
    ["1e5", 100000.0],
    ["1e6", 1.0e6],
]

function doBench(b::testing.B, f::Function) #=(b::testing.B, buf::Vector{UInt8}, level::Int, n::Int)=#
    for (_, suite) in suites
        buf, err = os.ReadFile(suite.file)
        if err !== nothing
            b.Fatal(err)
        end

        if length(buf) == 0
            b.Fatalf("test file %q has no data", suite.file)
        end

        for (_, l) in levelTests
            for (_, s) in sizes
                b.Run(
                    suite.name + "/" + l.name + "/" + s.name,
                    ((b::testing.B) -> begin
                        f(b, buf, l.level, s.n)
                    end),
                )
            end

        end

    end

end
end # module
