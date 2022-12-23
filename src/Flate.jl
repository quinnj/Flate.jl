module Flate

include("gotypes.jl")
import .GoTypes: Go

include("consts.jl")
include("token.jl")
include("huffman_code.jl")
include("huffman_bit_writer.jl")
include("dict_decoder.jl")
include("deflatefast.jl")
include("deflate.jl")
include("inflate.jl")

# huffOffset is a static offset encoder used for huffman only encoding.
# It can be reused since we will not be encoding offset values.
const huffOffset = Ref{HuffmanEncoder}()

function __init__()
    offsetFreq = Go.Slice(Int32, offsetCodeCount)
    offsetFreq[0] = 1
    huffOffset[] = newHuffmanEncoder(offsetCodeCount)
    generate(huffOffset[], offsetFreq, 15)
end

end
