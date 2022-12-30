#  In performance critical applications, Reset can be used to discard the
#  current compressor or Decompressor state and reinitialize them quickly
#  by taking advantage of previously allocated memory.
@testset "Example_reset" begin
    proverbs = [
        "Don't communicate by sharing memory, share memory by communicating.\n",
        "Concurrency is not parallelism.\n",
        "The bigger the interface, the weaker the abstraction.\n",
        "Documentation is for users.\n",
    ]

    b = IOBuffer()
    zw = Flate.NewWriter(IOBuffer(), Flate.DefaultCompression)
    zr = Flate.NewReader(IOBuffer())
    for s in proverbs
        take!(b)
        #  Reset the compressor and encode from some input stream.
        reset(zw, b)
        write(zw, Go.Slice(s))
        close(zw)
        seekstart(b)
        zr = reset(zr, b, Go.Slice(UInt8, 0))
        out = sprint(write, zr)
        @test out == s
    end
end

#  A preset dictionary can be used to improve the compression ratio.
#  The downside to using a dictionary is that the compressor and Decompressor
#  must agree in advance what dictionary to use.
@testset "Example_dictionary" begin
    #  The data to compress should (but is not required to) contain frequent
    #  The dictionary is a string of bytes. When compressing some input data,
    #  the compressor will attempt to substitute substrings with matches found
    #  in the dictionary. As such, the dictionary should only contain substrings
    #  that are expected to be found in the actual data stream.
    #  substrings that match those in the dictionary.
    dict = "<?xml version=\"1.0\"?>" * "<book>" * "<data>" * "<meta name=\"" * "\" content=\""
    data = "<?xml version=\"1.0\"?>\n<book>\n\t<meta name=\"title\" content=\"The Go Programming Language\"/>\n\t<meta name=\"authors\" content=\"Alan Donovan and Brian Kernighan\"/>\n\t<meta name=\"published\" content=\"2015-10-26\"/>\n\t<meta name=\"isbn\" content=\"978-0134190440\"/>\n\t<data>...</data>\n</book>\n"
    #  Compress the data using the specially crafted dictionary.
    b = IOBuffer()
    zw = Flate.NewWriterDict(b, Flate.DefaultCompression, Go.Slice(dict))
    write(zw, Go.Slice(data))
    close(zw)
    seekstart(b)
    #  The Decompressor must use the same dictionary as the compressor.
    #  Otherwise, the input may appear as corrupted.
    zr = Flate.NewReaderDict(b, Go.Slice(dict))
    out = sprint(write, zr)
    @test out == data
    #  Substitute all of the bytes in the dictionary with a '#' to visually
    #  demonstrate the approximate effectiveness of using a preset dictionary.
    hashDict = Go.Slice(dict)
    for i in eachindex(hashDict)
        hashDict[i] = UInt8('#')
    end

    seekstart(b)
    zr = Flate.NewReaderDict(b, hashDict)
    out = sprint(write, zr)
    # doesn't equal because we used all '#' as dictionary
    @test out != data
end
