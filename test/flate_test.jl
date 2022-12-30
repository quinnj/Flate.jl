#  The following test should not error.
@testset "TestIssue5915" begin
    bits = Go.Slice([4, 0, 0, 6, 4, 3, 2, 3, 3, 4, 4, 5, 0, 0, 0, 0, 5, 5, 6,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 8, 6, 0, 11, 0, 8, 0, 6, 6, 10, 8
    ])
    h = Flate.HuffmanDecoder()
    @test !Flate.init(h, bits)
end

#  The following test should not error.
@testset "TestIssue5962" begin
    bits = Go.Slice([4, 0, 0, 6, 4, 3, 2, 3, 3, 4, 4, 5, 0, 0, 0, 0,
        5, 5, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11])
    h = Flate.HuffmanDecoder()
    @test !Flate.init(h, bits)
end

#  The following test should not error.
@testset "TestIssue6255" begin
    bits1 = Go.Slice([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11])
    bits2 = Go.Slice([11, 13])
    h = Flate.HuffmanDecoder()
    @test Flate.init(h, bits1)
    @test !Flate.init(h, bits2)
end

@testset "TestInvalidEncoding" begin
    #  Initialize Huffman decoder to recognize "0".
    h = Flate.HuffmanDecoder()
    @test Flate.init(h, Go.Slice([1]))
    #  Initialize Decompressor with invalid Huffman coding.
    f = Flate.Decompressor(
        IOBuffer(UInt8[255]),
        Go.Array(Int, 0),
        Go.Array(Int, 0),
        Flate.DictDecoder(),
        Flate.nextBlock
    )
    @test_throws Flate.CorruptInputError Flate.huffSym(f, h)
end

@testset "TestInvalidBits" begin
    oversubscribed = Go.Slice([1, 2, 3, 4, 4, 5])
    incomplete = Go.Slice([1, 2, 4, 4])
    h = Flate.HuffmanDecoder()
    @test !Flate.init(h, oversubscribed)
    @test !Flate.init(h, incomplete)
end

# @testset "TestStreams" begin
#     #  To verify any of these hexstrings as valid or invalid flate streams
#     #  according to the C zlib library, you can use the Python wrapper library:
#     #  >>> hex_string = "010100feff11"
#     #  >>> import zlib
#     #  >>> zlib.decompress(hex_string.decode("hex"), -15) # Negative means raw DEFLATE
#     #  '\x11'
#     testCases = Vector{NamedTuple{(desc, stream, want),Tuple{String,String,String}}}[
#         [
#             "degenerate HCLenTree",
#             "05e0010000000000100000000000000000000000000000000000000000000000" +
#             "00000000000000000004",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, empty HLitTree, empty HDistTree",
#             "05e0010400000000000000000000000000000000000000000000000000000000" +
#             "00000000000000000010",
#             "fail",
#         ],
#         [
#             "empty HCLenTree",
#             "05e0010000000000000000000000000000000000000000000000000000000000" +
#             "00000000000000000010",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree, empty HDistTree, use missing HDist symbol",
#             "000100feff000de0010400000000100000000000000000000000000000000000" +
#             "0000000000000000000000000000002c",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree, degenerate HDistTree, use missing HDist symbol",
#             "000100feff000de0010000000000000000000000000000000000000000000000" +
#             "00000000000000000610000000004070",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, empty HLitTree, empty HDistTree",
#             "05e0010400000000100400000000000000000000000000000000000000000000" +
#             "0000000000000000000000000008",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, empty HLitTree, degenerate HDistTree",
#             "05e0010400000000100400000000000000000000000000000000000000000000" +
#             "0000000000000000000800000008",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, degenerate HLitTree, degenerate HDistTree, use missing HLit symbol",
#             "05e0010400000000100000000000000000000000000000000000000000000000" +
#             "0000000000000000001c",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree, too large HDistTree",
#             "edff870500000000200400000000000000000000000000000000000000000000" +
#             "000000000000000000080000000000000004",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree, empty HDistTree, excessive repeater code",
#             "edfd870500000000200400000000000000000000000000000000000000000000" +
#             "000000000000000000e8b100",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree, empty HDistTree of normal length 30",
#             "05fd01240000000000f8ffffffffffffffffffffffffffffffffffffffffffff" +
#             "ffffffffffffffffff07000000fe01",
#             "",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree, empty HDistTree of excessive length 31",
#             "05fe01240000000000f8ffffffffffffffffffffffffffffffffffffffffffff" +
#             "ffffffffffffffffff07000000fc03",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, over-subscribed HLitTree, empty HDistTree",
#             "05e001240000000000fcffffffffffffffffffffffffffffffffffffffffffff" +
#             "ffffffffffffffffff07f00f",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, under-subscribed HLitTree, empty HDistTree",
#             "05e001240000000000fcffffffffffffffffffffffffffffffffffffffffffff" +
#             "fffffffffcffffffff07f00f",
#             "fail",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree with single code, empty HDistTree",
#             "05e001240000000000f8ffffffffffffffffffffffffffffffffffffffffffff" +
#             "ffffffffffffffffff07f00f",
#             "01",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree with multiple codes, empty HDistTree",
#             "05e301240000000000f8ffffffffffffffffffffffffffffffffffffffffffff" +
#             "ffffffffffffffffff07807f",
#             "01",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree, degenerate HDistTree, use valid HDist symbol",
#             "000100feff000de0010400000000100000000000000000000000000000000000" +
#             "0000000000000000000000000000003c",
#             "00000000",
#         ],
#         [
#             "complete HCLenTree, degenerate HLitTree, degenerate HDistTree",
#             "05e0010400000000100000000000000000000000000000000000000000000000" +
#             "0000000000000000000c",
#             "",
#         ],
#         [
#             "complete HCLenTree, degenerate HLitTree, empty HDistTree",
#             "05e0010400000000100000000000000000000000000000000000000000000000" +
#             "00000000000000000004",
#             "",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree, empty HDistTree, spanning repeater code",
#             "edfd870500000000200400000000000000000000000000000000000000000000" +
#             "000000000000000000e8b000",
#             "",
#         ],
#         [
#             "complete HCLenTree with length codes, complete HLitTree, empty HDistTree",
#             "ede0010400000000100000000000000000000000000000000000000000000000" +
#             "0000000000000000000400004000",
#             "",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree, degenerate HDistTree, use valid HLit symbol 284 with count 31",
#             "000100feff00ede0010400000000100000000000000000000000000000000000" +
#             "000000000000000000000000000000040000407f00",
#             "0000000000000000000000000000000000000000000000000000000000000000" +
#             "0000000000000000000000000000000000000000000000000000000000000000" +
#             "0000000000000000000000000000000000000000000000000000000000000000" +
#             "0000000000000000000000000000000000000000000000000000000000000000" +
#             "0000000000000000000000000000000000000000000000000000000000000000" +
#             "0000000000000000000000000000000000000000000000000000000000000000" +
#             "0000000000000000000000000000000000000000000000000000000000000000" +
#             "0000000000000000000000000000000000000000000000000000000000000000" +
#             "000000",
#         ],
#         [
#             "complete HCLenTree, complete HLitTree, degenerate HDistTree, use valid HLit and HDist symbols",
#             "0cc2010d00000082b0ac4aff0eb07d27060000ffff",
#             "616263616263",
#         ],
#         ["fixed block, use reserved symbol 287", "33180700", "fail"],
#         ["raw block", "010100feff11", "11"],
#         [
#             "issue 10426 - over-subscribed HCLenTree causes a hang",
#             "344c4a4e494d4b070000ff2e2eff2e2e2e2e2eff",
#             "fail",
#         ],
#         [
#             "issue 11030 - empty HDistTree unexpectedly leads to error",
#             "05c0070600000080400fff37a0ca",
#             "",
#         ],
#         [
#             "issue 11033 - empty HDistTree unexpectedly leads to error",
#             "050fb109c020cca5d017dcbca044881ee1034ec149c8980bbc413c2ab35be9dc" +
#             "b1473449922449922411202306ee97b0383a521b4ffdcf3217f9f7d3adb701",
#             "3130303634342068652e706870005d05355f7ed957ff084a90925d19e3ebc6d0" + "c6d7",
#         ],
#     ]
#     for (i, tc) in testCases
#         data, err = hex.DecodeString(tc.stream)
#         if err !== nothing
#             t.Fatal(err)
#         end
#         data, err = io.ReadAll(NewReader(bytes.NewReader(data)))
#         if tc.want == "fail"
#             if err=== nothing
#                 t.Errorf("#%d (%s): got nil error, want non-nil", i, tc.desc)
#             end
#         else
#             if err !== nothing
#                 t.Errorf("#%d (%s): %v", i, tc.desc, err)
#                 continue
#             end
#             if (got = hex.EncodeToString(data); got != tc.want)
#                 t.Errorf("#%d (%s):\\ngot  %q\\nwant %q", i, tc.desc, got, tc.want)
#             end
#         end
#     end
# end

@testset "TestTruncatedStreams" begin
    data = Go.Slice("\u00\f\u00\uf3\uffhello, world\u01\u00\u00\uff\uff")
    for i = 1:(length(data) - 2)
        r = Flate.NewReader(IOBuffer(copy(data[begin:i])))
        @test_throws Flate.CorruptInputError sprint(write, r)
    end
end

#  Verify that flate.Reader.Read returns (n, io.EOF) instead
#  of (n, nil) + (0, io.EOF) when possible.
# 
#  This helps net/http.Transport reuse HTTP/1 connections more
#  aggressively.
# 
#  See https://github.com/google/go-github/pull/317 for background.
@testset "TestReaderEarlyEOF" begin
    testSizes = [
        1, 2, 3, 4, 5, 6, 7, 8,
        100, 1000, 10000, 100000,
        128, 1024, 16384, 131072,
        # Testing multiples of windowSize triggers the case
        # where Read will fail to return an early io.EOF.
        Flate.windowSize * 1, Flate.windowSize * 2, Flate.windowSize * 3,
    ]
    maxSize = maximum(testSizes)
    readBuf = Go.Slice(UInt8, 40)
    data = Go.Slice([(i % UInt8) for i = 0:(maxSize - 1)])
    for sz in testSizes
        for flush in (true, false)
            #  Do we expect early io.EOF?
            earlyEOF = true
            buf = IOBuffer()
            w = Flate.NewWriter(buf, 5)
            write(w, data[begin:sz])
            if flush
                #  If a Flush occurs after all the actual data, the flushing
                #  semantics dictate that we will observe a (0, io.EOF) since
                #  Read must return data before it knows that the stream ended.
                Flate.flush(w)
                earlyEOF = false
            end
            close(w)
            seekstart(buf)
            r = Flate.NewReader(buf)
            out = sprint(write, r)
            @test out == String(copy(data[begin:sz]))
        end
    end
end
