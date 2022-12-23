#  HCode is a huffman code with a bit code and bit Go.len.
mutable struct HCode
    code::UInt16
    len::UInt16
end

#  A LevelInfo describes the state of the constructed tree for a given depth.
mutable struct LiteralNode
    literal::UInt16
    freq::Int32
end

struct ByLiteral
    nodes::Go.Slice{LiteralNode}
end
ByLiteral() = ByLiteral(Go.Slice(LiteralNode, 0))

struct ByFreq
    nodes::Go.Slice{LiteralNode}
end
ByFreq() = ByFreq(Go.Slice(LiteralNode, 0))

#  set sets the code and Go.len of an HCode.
mutable struct LevelInfo
    level::Int32
    lastFreq::Int32
    nextCharFreq::Int32
    nextPairFreq::Int32
    needed::Int32
end
LevelInfo(level::Integer, lastFreq::Integer, nextCharFreq::Integer, nextPairFreq::Integer) =
    LevelInfo(level, lastFreq, nextCharFreq, nextPairFreq, Int32(0))
LevelInfo() = LevelInfo(0, 0, 0, 0, 0)

mutable struct HuffmanEncoder
    codes::Go.Slice{HCode}
    freqcache::Go.Slice{LiteralNode}
    bitCount::Go.Array{Int32}
    lns::ByLiteral
    lfs::ByFreq
end
HuffmanEncoder(codes::Go.Slice{HCode}) = HuffmanEncoder(codes, Go.Slice(LiteralNode, 0), Go.Array(Int32, 17), ByLiteral(), ByFreq())

function set(h::HCode, code::Integer, length::Integer)
    h.len = length
    h.code = code
    return
end

function maxNode()# ::LiteralNode
    return LiteralNode(typemax(UInt16), typemax(Int32))
end

function newHuffmanEncoder(size::Int)# ::HuffmanEncoder
    codes = Go.Slice(HCode, size)
    for i = eachindex(codes)
        codes[i] = HCode(0, 0)
    end
    return HuffmanEncoder(codes)
end

#  Generates a HuffmanCode corresponding to the fixed literal table
function generateFixedLiteralEncoding()# ::HuffmanEncoder
    h = newHuffmanEncoder(maxNumLit)
    codes = h.codes
    for ch = UInt16(0):UInt16(maxNumLit-1)
        local bits::UInt16
        local size::UInt16
        if ch < 144
            #  size 8, 000110000  .. 10111111
            bits = ch + 48
            size = 8
        elseif ch < 256
            #  size 9, 110010000 .. 111111111
            bits = ch + 400 - 144
            size = 9
        elseif ch < 280
            #  size 7, 0000000 .. 0010111
            bits = ch - 256
            size = 7
        else
            #  size 8, 11000000 .. 11000111
            bits = ch + 192 - 280
            size = 8
        end
        codes[ch] = HCode(reverseBits(bits, UInt8(size)), size)
    end
    return h
end

function generateFixedOffsetEncoding()# ::HuffmanEncoder
    h = newHuffmanEncoder(30)
    codes = h.codes
    for ch in eachindex(codes)
        codes[ch] = HCode(reverseBits(UInt16(ch), UInt8(5)), 5)
    end
    return h
end

function bitLength(h::HuffmanEncoder, freq::Go.Slice{Int32})# ::Int
    total::Int = 0
    for (i, f) in Go.each(freq)
        if f != 0
            total += Int(f) * Int(h.codes[i].len)
        end
    end
    return total
end

#  bitCounts computes the number of literals assigned to each bit size in the Huffman encoding.
#  It is only called when list.Go.len >= 3.
const maxBitsLimit = 16

#  The cases of 0, 1, and 2 literals are handled by special case code.
# 
#  list is an array of the literals with non-zero frequencies
#  and their associated frequencies. The array is in order of increasing
#  frequency and has as its last element a special element with frequency
#  MaxInt32.
# 
#  maxBits is the maximum number of bits that should be used to encode any literal.
#  It must be less than 16.
# 
#  bitCounts returns an integer slice in which slice[i] indicates the number of literals
#  that should be encoded in i bits.
function bitCounts(h::HuffmanEncoder, list::Go.Slice{LiteralNode}, maxBits::Integer)# ::Vector{Int32}
    if maxBits >= maxBitsLimit
        error("flate: maxBits too large")
    end

    n = Int32(Go.len(list))
    list = list[0:n+1]
    list[n] = maxNode()
    #  The tree can't have greater depth than n - 1, no matter what. This
    #  saves a little bit of work in some small cases
    if maxBits > n - 1
        maxBits = n - 1
    end

    #  leafCounts[i] counts the number of literals at the left
    #  Create information about each of the levels.
    #  A bogus "Level 0" whose sole purpose is so that
    #  level1.prev.needed==0.  This makes level1.nextPairFreq
    #  be a legitimate value that never gets chosen.
    #  of ancestors of the rightmost node at level i.
    levels = Go.Array(LevelInfo, maxBitsLimit)
    for i = 0:(maxBitsLimit - 1)
        levels[i] = LevelInfo()
    end

    #  leafCounts[i][j] is the number of literals at the left
    #  of the level j ancestor.
    leafCounts = Go.Array(Int32, maxBitsLimit, maxBitsLimit)

    for level = Int32(1):maxBits
        #  For every level, the first two items are the first two characters.
        #  We initialize the levels as if we had already figured this out.
        levels[level] = LevelInfo(
            level,
            list[1].freq,
            list[2].freq,
            list[0].freq + list[1].freq,
        )
        leafCounts[level][level] = 2
        if level == 1
            levels[level].nextPairFreq = typemax(Int32)
        end
    end

    #  We need a total of 2*n - 2 items at top level and have already generated 2.
    levels[maxBits].needed = 2 * n - 4
    level = maxBits
    while true
        l = levels[level]
        if l.nextPairFreq == typemax(Int32) && l.nextCharFreq == typemax(Int32)
            #  We've run out of both leafs and pairs.
            #  End all calculations for this level.
            #  To make sure we never come back to this level or any lower level,
            #  set nextPairFreq impossibly large.
            l.needed = 0
            levels[level+1].nextPairFreq = typemax(Int32)
            level += 1
            continue
        end

        prevFreq = l.lastFreq
        if l.nextCharFreq < l.nextPairFreq
            #  The next item on this row is a leaf node.
            _n = leafCounts[level][level] + 1
            l.lastFreq = l.nextCharFreq
            #  Lower leafCounts are the same of the previous node.
            leafCounts[level][level] = _n
            l.nextCharFreq = list[_n].freq
        else
            #  The next item on this row is a pair from the previous row.
            #  nextPairFreq isn't valid until we generate two
            #  more values in the level below
            l.lastFreq = l.nextPairFreq
            #  Take leaf counts from the lower level, except counts[level] remains the same.
            copy(leafCounts[level][begin:level], leafCounts[level-1][begin:level])
            levels[l.level-1].needed = 2
        end

        if (l.needed -= 1; l.needed == 0)
            #  We've done everything we need to do for this level.
            #  Continue calculating one level up. Fill in nextPairFreq
            #  of that level with the sum of the two nodes we've just calculated on
            #  this level.
            if l.level == maxBits
                #  All done!
                break
            end
            levels[l.level+1].nextPairFreq = prevFreq + l.lastFreq
            level += 1
        else
            while levels[level-1].needed > 0
                level -= 1
            end
        end
    end

    #  Somethings is wrong if at the end, the top level is null or hasn't used
    #  all of the leaves.
    if leafCounts[maxBits][maxBits] != n
        error("leafCounts[maxBits][maxBits] != n")
    end

    bitCount = h.bitCount[begin:maxBits+1]
    bits = 1
    counts = leafCounts[maxBits]
    for level = maxBits:-1:1
        #  chain.leafCount gives the number of literals requiring at least "bits"
        #  bits to encode.
        bitCount[bits] = counts[level] - counts[level-1]
        bits += 1
    end
    return bitCount
end

#  Look at the leaves and assign them a bit count and an encoding as specified
#  in RFC 1951 3.2.2
function assignEncodingAndSize(
    h::HuffmanEncoder,
    bitCount::Go.Slice{Int32},
    list::Go.Slice{LiteralNode},
)
    code::UInt16 = 0
    for (n, bits) in Go.each(bitCount)
        code <<= 1
        if n == 0 || bits == 0
            continue
        end

        #  The literals list[Go.len(list)-bits] .. list[Go.len(list)-bits]
        #  are encoded using "bits" bits, and get the values
        #  code, code + 1, ....  The code values are
        #  assigned in literal order (not frequency order).
        chunk = list[Go.len(list)-Int(bits), :]
        sort!(chunk, by=x->x.literal)
        h.lns = ByLiteral(chunk)
        for node in chunk
            h.codes[node.literal] =
                HCode(reverseBits(code, UInt8(n)), UInt16(n))
            code += 1
        end
        list = list[0:Go.len(list)-Int(bits)]
    end

end
#  Update this Huffman Code object to be the minimum code for the specified frequency count.
# 
#  freq is an array of frequencies, in which freq[i] gives the frequency of literal i.
#  maxBits  The maximum number of bits to use for any literal.
function generate(h::HuffmanEncoder, freq::Go.Slice{Int32}, maxBits::Integer)
    if Go.len(h.freqcache) == 0
        #  Allocate a reusable buffer with the longest possible frequency table.
        #  Possible lengths are codegenCodeCount, offsetCodeCount and maxNumLit.
        #  The largest of these is maxNumLit, so we allocate for that case.
        h.freqcache = Go.Slice(LiteralNode, maxNumLit + 1)
    end

    list = h.freqcache[begin:Go.len(freq)+1]
    #  Number of non-zero literals
    count = 0
    for (i, f) in Go.each(freq)
        if f != 0
            list[count] = LiteralNode(UInt16(i), f)
            count += 1
        else
            h.codes[i].len = 0
        end
    end

    list = list[begin:count]
    if count <= 2
        for (i, node) in Go.each(list)
            #  "list" is in order of increasing literal value.
            set(h.codes[node.literal], UInt16(i), 1)
        end
        return
    end
    sort!(list, by = x -> (x.freq, x.literal))
    h.lfs = ByFreq(list)
    #  Get the number of literals for each bit count
    bitCount = bitCounts(h, list, maxBits)
    #  And do the assignment
    assignEncodingAndSize(h, bitCount, list)
end

function reverseBits(number::UInt16, bitLength::UInt8)# ::UInt16
    return bitreverse(number << (16 - bitLength))
end

const fixedLiteralEncoding::HuffmanEncoder = generateFixedLiteralEncoding()
const fixedOffsetEncoding::HuffmanEncoder = generateFixedOffsetEncoding()