using Flate, Test, Flate.GoTypes

@testset "TestReset" begin
    s = Go.Slice("lorem ipsum izzle fo rizzle")
    deflated = IOBuffer()
    w = Flate.NewWriter(deflated, 1)
    write(w, s)
    close(w)
    seekstart(deflated)
    inflated = IOBuffer()
    f = Flate.NewReader(deflated)
    write(inflated, f)
    s1 = take!(inflated)
    @test String(s) == String(s1)
    
    deflated = IOBuffer()
    f = reset(f, deflated, Go.Slice(UInt8, 0))
    s = Go.Slice("the quick brown fox jumped over")
    w = Flate.NewWriter(deflated, 1)
    write(w, s)
    close(w)
    seekstart(deflated)
    inflated = IOBuffer()
    f = Flate.NewReader(deflated)
    write(inflated, f)
    s1 = take!(inflated)
    @test String(s) == String(s1)
end

# @testset "TestReaderTruncated" begin
#     vectors = [
#         ["\u00", ""],
#         ["\u00\f", ""],
#         ["\u00\f\u00", ""],
#         ["\u00\f\u00\uf3\uff", ""],
#         ["\u00\f\u00\uf3\uffhello", "hello"],
#         ["\u00\f\u00\uf3\uffhello, world", "hello, world"],
#         ["\u02", ""],
#         ["\uf2H\ucd", "He"],
#         ["\uf2H͙0a\u0084\t", "Hel\u90\u90\u90\u90\u90"],
#         ["\uf2H͙0a\u0084\t\u00", "Hel\u90\u90\u90\u90\u90"],
#     ]

#     for v in vectors
#         r = IOBuffer(v[1])
#         zr = Flate.NewReader(r)
#         out = IOBuffer()
#         write(out, zr)
#         str = String(take!(out))

#         if String(b) != v.output
#             t.Errorf("test %d, output mismatch: got %q, want %q", i, b, v.output)
#         end
#     end
# end

@testset "TestResetDict" begin
    dict = Go.Slice("the lorem fox")
    ss = ["lorem ipsum izzle fo rizzle", "the quick brown fox jumped over"]
    deflated = [IOBuffer(), IOBuffer()]
    for (i, s) in enumerate(ss)
        w = Flate.NewWriterDict(deflated[i], Flate.DefaultCompression, dict)
        write(w, Go.Slice(s))
        close(w)
    end
    foreach(seekstart, deflated)
    inflated = [IOBuffer(), IOBuffer()]
    f = Flate.NewReader(IOBuffer())
    for i = 1:2
        f = reset(f, deflated[i], dict)
        write(inflated[i], f)
    end
    for (i, s) in enumerate(ss)
        @test String(take!(inflated[i])) == s
    end
end
