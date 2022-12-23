module GoTypes

export Go

module Go

export Array, Slice, len, cap, append, each

abstract type AbstractGoArray{T} <: AbstractVector{T} end

Base.size(x::AbstractGoArray) = (len(x),)
Base.firstindex(::AbstractGoArray) = 0
Base.lastindex(v::AbstractGoArray) = len(v) - 1
# go range indexing is half-open, so need to adjust last down
Base.checkbounds(v::AbstractGoArray, I::AbstractUnitRange) = checkbounds(Bool, v, first(I):(last(I)-1)) || Base.throw_boundserror(v, I)
Base.axes(v::AbstractGoArray) = (ZeroTo(len(v) - 1),)

# utility type like Base.OneTo, but for 0-based indexing
struct ZeroTo <: AbstractUnitRange{Int}
    stop::Int
end

Base.size(x::ZeroTo) = (x.stop - 1,)
Base.firstindex(::ZeroTo) = 0
Base.lastindex(x::ZeroTo) = x.stop
Base.first(r::ZeroTo) = 0
Base.axes(r::ZeroTo) = (r,)
Base.show(io::IO, r::ZeroTo) = print(io, "0:", r.stop)

function Base.getindex(v::ZeroTo, i::Integer)
    @inline
    i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    @boundscheck ((i > -1) & (i <= v.stop)) || Base.throw_boundserror(v, i)
    return Int(i)
end

# 0-based indexing
# fixed length
# data is Base.Vector{T}
struct Array{T} <: AbstractGoArray{T}
    data::Base.Vector{T}

    function Array(::Type{T}, len::Int) where {T}
        x = new{T}(Base.Vector{T}(undef, len))
        if isbitstype(T)
            # memset to zero out bits
            ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), x.data, 0, sizeof(x.data))
        end
        return x
    end

    function Array(::Type{T}, len::Int, len2::Int) where {T}
        x = new{Array{T}}(Base.Vector{Array{T}}(undef, len))
        for i = 1:len
            x.data[i] = Array(T, len2)
        end
        return x
    end

    function Array(x::Base.Vector{T}) where {T}
        return new{T}(x)
    end

    function Array(::Type{T}, x::T...) where {T}
        data = Base.Vector{T}(undef, length(x))
        for i = 1:length(x)
            data[i] = x[i]
        end
        return new{T}(data)
    end
end

Array(x::T...) where {T} = Array(T, x...)
Array(::Type{T}, x...) where {T} = Array([convert(T, v) for v in x])

len(x::Array) = length(x.data)
Base.isassigned(v::Array, i::Int) = isassigned(v.data, i + 1)

Base.@propagate_inbounds function Base.getindex(v::Array, i::Int)
    @boundscheck checkbounds(v, i)
    return v.data[i + 1]
end

Base.@propagate_inbounds function Base.setindex!(v::Array{T}, val, i::Int) where {T}
    @boundscheck checkbounds(v, i)
    v.data[i + 1] = convert(T, val)
    return v
end

# 0-based indexing
# dynamic length
# data is Array{T}
struct Slice{T} <: AbstractGoArray{T}
    data::Base.Vector{T}
    i::Int # 0-based index into data where slice starts
    j::Int # 0-based index into data *after* slice ends (slice doesn't include this index)
end

Slice(io::IOBuffer) = Slice{UInt8}(io.data, 0, io.size)
Slice(x::Base.Vector{T}) where {T} = Slice{T}(x, 0, len(x))
Slice(x::String, i::Int, j::Int) = Slice{UInt8}(unsafe_wrap(Vector{UInt8}, x), i, j)
Slice(x::String) = Slice(x, 0, len(x))
Slice(x::Array{T}, i::Int, j::Int) where {T} = Slice{T}(x.data, i, j)
Slice(x::Array) = Slice(x, 0, len(x))
Slice(::Type{T}, x::T...) where {T} = Slice(Array(T, x...))
Slice(x::T...) where {T} = Slice(Array(x...))

# Slice constructors that generate Array
Slice(::Type{T}, len::Int) where {T} = Slice(Array(T, len))
Slice(::Type{T}, len::Int, cap::Int) where {T} = Slice(Array(T, cap), 0, len)

len(v::Base.Vector) = length(v)
len(v::String) = sizeof(v)
len(v::Slice) = v.j - v.i
cap(v::Slice) = len(v.data) - v.i

Base.isassigned(v::Slice, i::Int) = isassigned(v.data, i + v.i + 1)

Base.@propagate_inbounds function Base.getindex(v::Slice, i::Int)
    @boundscheck checkbounds(v, i)
    return v.data[i + v.i + 1]
end

Base.@propagate_inbounds function Base.setindex!(v::Slice{T}, val, i::Int) where {T}
    @boundscheck checkbounds(v, i)
    v.data[i + v.i + 1] = convert(T, val)
    return v
end

# indexing Array with range produces Slice
# like go, lo:hi is half-open, so hi is not included in slice
Base.@propagate_inbounds function Base.getindex(v::Array{T}, r::UnitRange{Int}) where {T}
    @boundscheck checkbounds(v, r)
    return Slice{T}(v.data, first(r), last(r))
end

# special syntax for the x[0:] case since
# we can't just rely on x[0, :] since end lowers
# to lastindex(x) which is the last *actual* index
# and not 1 past the last index
Base.@propagate_inbounds function Base.getindex(v::Array{T}, i::Int, ::Colon) where {T}
    @boundscheck checkbounds(v, i)
    return Slice{T}(v.data, i, len(v))
end

# for slices, [lo:]
Base.@propagate_inbounds Base.getindex(v::Slice, i::Int, ::Colon) = v[i:len(v)]

# indexing Slice with range produces relative Slice w/ same underlying Array
Base.@propagate_inbounds function Base.getindex(v::Slice{T}, r::UnitRange{Int}) where {T}
    i = first(r) + v.i
    j = max(i, last(r) + v.i)
    @boundscheck begin
        checkbounds(v.data, (i + 1):j)
        i <= j || Base.throw_boundserror(v, r)
    end
    return Slice{T}(v.data, i, j)
end

# indexing Array with : produces Slice
Base.getindex(v::Array, ::Colon) = Slice(v)

# copy between slices
Base.copy(dest::Slice{T}, src::String) where {T} = copy(dest, Slice(src))
function Base.copy(dest::Slice{T}, src::Slice{T}) where {T}
    n = min(len(dest), len(src))
    for i in 0:(n - 1)
        # TODO: what about slices that overlap underlying Array?
        dest[i] = src[i]
    end
    return n
end

# append elements to Slice
# allocates new underlying Array if there isn't enough capacity
function append(v::Slice{T}, x::T...) where {T}
    n = len(v)
    m = n + length(x)
    if m > cap(v)
        resize!(v.data, m)
    end
    v = Slice(v.data, v.i, v.i + m)
    for i in 1:length(x)
        v[n + i - 1] = x[i]
    end
    return v
end

struct EachIterator{T <: AbstractGoArray}
    itr::T
end

each(v::AbstractGoArray) = EachIterator(v)

Base.iterate(itr::EachIterator) = ((0, itr.itr[0]), 1)
Base.iterate(itr::EachIterator, i) = i >= len(itr.itr) ? nothing : ((i, itr.itr[i]), i + 1)
Base.length(itr::EachIterator) = len(itr.itr)
Base.eltype(itr::EachIterator) = Tuple{Int, eltype(itr.itr)}

end # module Go
using .Go
end # module GoTypes