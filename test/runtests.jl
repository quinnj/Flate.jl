using Test, Flate, Flate.GoTypes

@testset "Flate" begin

include("gotypes.jl")
include("dict_decoder_test.jl")
include("huffman_bit_writer_test.jl")

end