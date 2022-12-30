module GoTypes

export Go

module Go

export Array, Slice, length, cap, append

abstract type AbstractGoArray{T} <: AbstractVector{T} end

Base.size(x::AbstractGoArray) = (length(x),)
Base.firstindex(::AbstractGoArray) = 0
Base.lastindex(v::AbstractGoArray) = length(v) - 1
# go range indexing is half-open, so need to adjust last down
Base.checkbounds(v::AbstractGoArray, I::AbstractUnitRange) = checkbounds(Bool, v, first(I):(last(I)-1)) || Base.throw_boundserror(v, I)
Base.axes(v::AbstractGoArray) = (ZeroTo(length(v) - 1),)

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

Base.@propagate_inbounds function Base.getindex(v::ZeroTo, i::Integer)
    i isa Bool && throw(ArgumentError("invalid index: $i of type Bool"))
    @boundscheck ((i > -1) & (i <= v.stop)) || Base.throw_boundserror(v, i)
    return Int(i)
end

# 0-based indexing
# fixed length
# data is Base.Vector{T}
struct Array{T} <: AbstractGoArray{T}
    data::Base.Vector{T}

    function Array(::Type{T}, length::Integer) where {T}
        x = new{T}(Base.Vector{T}(undef, length))
        if isbitstype(T)
            # memset to zero out bits
            ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), x.data, 0, sizeof(x.data))
        end
        return x
    end

    function Array(::Type{T}, length::Integer, len2::Integer) where {T}
        x = new{Array{T}}(Base.Vector{Array{T}}(undef, length))
        for i = 1:length
            x.data[i] = Array(T, len2)
        end
        return x
    end

    function Array(x::Base.Vector{T}) where {T}
        return new{T}(x)
    end
end

Base.copy(x::Array) = x.data
Base.length(x::Array) = length(x.data)
Base.isassigned(v::Array, i::Integer) = isassigned(v.data, i + 1)

Base.@propagate_inbounds function Base.getindex(v::Array, i::Integer)
    @boundscheck checkbounds(v, i)
    return v.data[i + 1]
end

Base.@propagate_inbounds function Base.setindex!(v::Array{T}, val, i::Integer) where {T}
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
Slice(x::Base.Vector{T}) where {T} = Slice{T}(x, 0, length(x))
Slice(x::String, i::Integer, j::Integer) = Slice{UInt8}(unsafe_wrap(Vector{UInt8}, x), i, j)
Slice(x::String) = Slice(x, 0, sizeof(x))
Slice(x::Array{T}, i::Integer, j::Integer) where {T} = Slice{T}(x.data, i, j)
Slice(x::Array) = Slice(x, 0, length(x))

# Slice constructors that generate Array
Slice(::Type{T}, length::Integer) where {T} = Slice(Array(T, length))
Slice(::Type{T}, length::Integer, cap::Integer) where {T} = Slice(Array(T, cap), 0, length)

Base.similar(a::Slice, ::Type{T}) where {T} = Slice(T, length(a))
Base.similar(::Slice, ::Type{T}, dims::Tuple{ZeroTo}) where {T} = Slice(T, length(dims[1]))

Base.copy(x::Slice) = @view x.data[x.i + 1:x.j]
Base.length(v::Slice) = v.j - v.i
cap(v::Slice) = length(v.data) - v.i

Base.isassigned(v::Slice, i::Integer) = isassigned(v.data, i + v.i + 1)

Base.@propagate_inbounds function Base.getindex(v::Slice, i::Integer)
    @boundscheck checkbounds(v, i)
    return v.data[i + v.i + 1]
end

Base.@propagate_inbounds function Base.setindex!(v::Slice{T}, val, i::Integer) where {T}
    @boundscheck checkbounds(v, i)
    v.data[i + v.i + 1] = convert(T, val)
    return v
end

# indexing Array with range produces Slice
# like go, lo:hi is half-open, so hi is not included in slice
Base.@propagate_inbounds function Base.getindex(v::Array{T}, r::UnitRange{<:Integer}) where {T}
    @boundscheck checkbounds(v, r)
    return Slice{T}(v.data, first(r), last(r))
end

# special syntax for the x[0:] case since
# we can't just rely on x[0, :] since end lowers
# to lastindex(x) which is the last *actual* index
# and not 1 past the last index
Base.@propagate_inbounds function Base.getindex(v::Array{T}, i::Integer, ::Colon) where {T}
    @boundscheck checkbounds(v, i)
    return Slice{T}(v.data, i, length(v))
end

# for slices, [lo:] -> [lo, :]
Base.@propagate_inbounds Base.getindex(v::Slice, i::Integer, ::Colon) = v[i:length(v)]

# indexing Slice with range produces relative Slice w/ same underlying Array
Base.@propagate_inbounds function Base.getindex(v::Slice{T}, r::UnitRange{<:Integer}) where {T}
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
    n = min(length(dest), length(src))
    for i in 0:(n - 1)
        # TODO: what about slices that overlap underlying Array?
        dest[i] = src[i]
    end
    return n
end

# append elements to Slice
# resizes underlying Base.Array if there isn't enough capacity
function append(v::Slice{T}, x::T...) where {T}
    n = length(v)
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

struct RangeIterator{T <: AbstractGoArray}
    itr::T
end

Base.range(v::AbstractGoArray) = RangeIterator(v)
Base.iterate(itr::RangeIterator, i=0) = i >= length(itr.itr) ? nothing : ((i, itr.itr[i]), i + 1)
Base.length(itr::RangeIterator) = length(itr.itr)
Base.eltype(itr::RangeIterator) = Tuple{Int, eltype(itr.itr)}

end # module Go
using .Go
end # module GoTypes