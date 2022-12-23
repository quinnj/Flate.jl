#  Copyright 2016 The Go Authors. All rights reserved.
#  Use of this source code is governed by a BSD-style
#  license that can be found in the LICENSE file.
module flate_test

#  In performance critical applications, Reset can be used to discard the
#  current compressor or Decompressor state and reinitialize them quickly
#  by taking advantage of previously allocated memory.
function Example_reset()
    proverbs = Vector{String}[
        "Don't communicate by sharing memory, share memory by communicating.\\n",
        "Concurrency is not parallelism.\\n",
        "The bigger the interface, the weaker the abstraction.\\n",
        "Documentation is for users.\\n",
    ]

    local r::strings.Reader

    local b::bytes.Buffer

    buf = make(Vector{UInt8}, 32 << 10)
    zw, err = flate.NewWriter(nothing, flate.DefaultCompression)
    if err !== nothing
        error(err)
    end

    zr = flate.NewReader(nothing)
    for (_, s) in proverbs
        r.Reset(s)
        b.Reset()
        #  Reset the compressor and encode from some input stream.
        zw.Reset(b)
        if (_, err = io.CopyBuffer(zw, r, buf); err !== nothing)
            error(err)
        end

        if (err = zw.Close(); err !== nothing)
            error(err)
        end

        #  Reset the Decompressor and decode to some output stream.
        if (err = (zr::flate.Resetter).Reset(b, nothing); err !== nothing)
            error(err)
        end

        if (_, err = io.CopyBuffer(os.Stdout, zr, buf); err !== nothing)
            error(err)
        end

        if (err = zr.Close(); err !== nothing)
            error(err)
        end

    end

end
#  A preset dictionary can be used to improve the compression ratio.
#  The downside to using a dictionary is that the compressor and Decompressor
#  must agree in advance what dictionary to use.
function Example_dictionary()
    #  The data to compress should (but is not required to) contain frequent
    #  The dictionary is a string of bytes. When compressing some input data,
    #  the compressor will attempt to substitute substrings with matches found
    #  in the dictionary. As such, the dictionary should only contain substrings
    #  that are expected to be found in the actual data stream.
    #  substrings that match those in the dictionary.
    const dict =
        "<?xml version=\"1.0\"?>" + "<book>" + "<data>" + "<meta name=\"" + "\" content=\""

    const data = "<?xml version=\"1.0\"?>\n<book>\n\t<meta name=\"title\" content=\"The Go Programming Language\"/>\n\t<meta name=\"authors\" content=\"Alan Donovan and Brian Kernighan\"/>\n\t<meta name=\"published\" content=\"2015-10-26\"/>\n\t<meta name=\"isbn\" content=\"978-0134190440\"/>\n\t<data>...</data>\n</book>\n"

    #  Compress the data using the specially crafted dictionary.
    local b::bytes.Buffer

    zw, err = flate.NewWriterDict(b, flate.DefaultCompression, convert(Vector{UInt8}, dict))
    if err !== nothing
        error(err)
    end

    if (_, err = io.Copy(zw, strings.NewReader(data)); err !== nothing)
        error(err)
    end

    if (err = zw.Close(); err !== nothing)
        error(err)
    end

    #  The Decompressor must use the same dictionary as the compressor.
    #  Otherwise, the input may appear as corrupted.
    println("Decompressed output using the dictionary:")
    zr = flate.NewReaderDict(bytes.NewReader(b.Bytes()), convert(Vector{UInt8}, dict))
    if (_, err = io.Copy(os.Stdout, zr); err !== nothing)
        error(err)
    end

    if (err = zr.Close(); err !== nothing)
        error(err)
    end

    println()
    #  Substitute all of the bytes in the dictionary with a '#' to visually
    #  demonstrate the approximate effectiveness of using a preset dictionary.
    println("Substrings matched by the dictionary are marked with #:")
    hashDict = convert(Vector{UInt8}, dict)
    for (i) in hashDict
        hashDict[i] = '#'
    end

    zr = flate.NewReaderDict(b, hashDict)
    if (_, err = io.Copy(os.Stdout, zr); err !== nothing)
        error(err)
    end

    if (err = zr.Close(); err !== nothing)
        error(err)
    end

end
#  DEFLATE is suitable for transmitting compressed data across the network.
function Example_synchronization()
    local wg::sync.WaitGroup

    @defer wg.Wait()
    #  Use io.Pipe to simulate a network connection.
    #  A real network application should take care to properly close the
    #  underlying connection.
    rp, wp = io.Pipe()
    #  Start a goroutine to act as the transmitter.
    wg.Add(1)
    Threads.@spawn begin
        (
            () -> begin
                @defer wg.Done()
                zw, err = flate.NewWriter(wp, flate.BestSpeed)
                if err !== nothing
                    error(err)
                end

                b = make(Vector{UInt8}, 256)
                for (_, m) in
                    strings.Fields("A long time ago in a galaxy far, far away...")
                    #  We use a simple framing format where the first byte is the
                    #  message length, followed the message itself.
                    b[0] = UInt8(copy(b[1, :], m))
                    if (_, err = zw.Write(b[begin:1+length(m)]); err !== nothing)
                        error(err)
                    end

                    #  Flush ensures that the receiver can read all data sent so far.
                    if (err = zw.Flush(); err !== nothing)
                        error(err)
                    end

                end

                if (err = zw.Close(); err !== nothing)
                    error(err)
                end

            end
        )()
    end

    #  Start a goroutine to act as the receiver.
    wg.Add(1)
    Threads.@spawn begin
        (() -> begin
            @defer wg.Done()
            zr = flate.NewReader(rp)
            b = make(Vector{UInt8}, 256)
            while true
                #  Read the message length.
                #  This is guaranteed to return for every corresponding
                #  Flush and Close on the transmitter side.
                if (_, err = io.ReadFull(zr, b[begin:1]); err !== nothing)
                    if err == io.EOF
                        #  The transmitter closed the stream
                        break
                    end

                    error(err)
                end

                #  Read the message content.
                n = Int(b[0])
                if (_, err = io.ReadFull(zr, b[begin:n]); err !== nothing)
                    error(err)
                end

                print("Received %d bytes: %s\\n", n, b[begin:n])
            end

            println()
            if (err = zr.Close(); err !== nothing)
                error(err)
            end

        end)()
    end

end
end # module
