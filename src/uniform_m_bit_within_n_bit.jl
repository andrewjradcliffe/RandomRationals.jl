################
# 2022-07-18
function randrational(r::AbstractRNG, ::Type{Rational{T}}, m::Integer) where {T<:Union{UInt8,UInt16,UInt32,UInt64}}
    n = 8*sizeof(T)
    0 < m ≤ n || throw(ArgumentError("m must be 0 < m ≤ 8*sizeof($T)"))
    x = rand(r, UInt64) >>> (64 - m) % T
    y = one(T) << m - one(T)
    num, den = divgcd(x, y)
    unsafe_rational(num, den)
end
function randrational(r::AbstractRNG, ::Type{Rational{T}}, m::Integer) where {T<:Union{Int8,Int16,Int32,Int64}}
    n = 8*sizeof(T)
    0 < m ≤ n-1 || throw(ArgumentError("m must be 0 < m ≤ 8*sizeof($T)-1"))
    x = rand(r, UInt64) >>> (64 - m) % T
    y = typemax(T) >> (n-1 - m)
    num, den = divgcd(x, y)
    unsafe_rational(num, den)
end
function randrational(r::AbstractRNG, ::Type{Rational{UInt128}}, m::Integer)
    0 < m ≤ 128 || throw(ArgumentError("m must be 0 < m ≤ 8*sizeof(UInt128)"))
    x = rand(r, UInt128) >>> (128 - m)
    y = one(UInt128) << m - one(UInt128)
    num, den = divgcd(x, y)
    unsafe_rational(num, den)
end
function randrational(r::AbstractRNG, ::Type{Rational{Int128}}, m::Integer)
    0 < m ≤ 127 || throw(ArgumentError("m must be 0 < m ≤ 8*sizeof(UInt128)-1"))
    x = rand(r, UInt128) >>> (128 - m) % Int128
    y = typemax(Int128) >> (127 - m)
    num, den = divgcd(x, y)
    unsafe_rational(num, den)
end
randrational(::Type{Rational{T}}, m::Integer) where {T<:Integer} = randrational(default_rng(), Rational{T}, m)
[randrational(Rational{UInt8}, 3) for _ = 1:10]


@benchmark rand(Rational{Int8})
@benchmark randrational(Rational{Int8}, 7)
@benchmark rand(Rational{Int64})
@benchmark randrational(Rational{Int64}, 63)
@benchmark rand(Rational{Int128})
@benchmark randrational(Rational{Int128}, 127)

@benchmark rand(Rational{UInt64})
@benchmark randrational(Rational{UInt64}, 64)
@benchmark randrational(Rational{UInt64}, 0x40)


randrational(Rational{UInt8}, 3)

@code_warntype randrational(default_rng(), Rational{Int64}, 63)
@code_warntype randrational(default_rng(), Rational{Int128}, 127)

#### using a type
abstract type RationalInterval{T<:Integer} end
struct CloseClose01{T<:Integer} <: RationalInterval{T} end
struct CloseClose01_m{T<:Integer, S<:Integer} <: RationalInterval{T}
    m::S
    global unsafe_closeclose01_m(::Type{T}, m::S) where {T,S} = new{T,S}(m)
end
function CloseClose01_m{T}(m::S) where {T<:Unsigned, S<:Integer}
    0 < m ≤ 8*sizeof(T) || throw(ArgumentError("m must be 0 < m ≤ 8*sizeof($T)"))
    unsafe_closeclose01_m(T, m)
end
function CloseClose01_m{T}(m::S) where {T<:Signed, S<:Integer}
    0 < m ≤ 8*sizeof(T)-1 || throw(ArgumentError("m must be 0 < m ≤ 8*sizeof($T)-1"))
    unsafe_closeclose01_m(T, m)
end
Base.eltype(::Type{S}) where {S<:RationalInterval{T}} where {T<:Integer} = Rational{T}

closeclose01_m(::Type{T}, m::Integer) where {T<:Integer} = CloseClose01_m{T}(m)
closeclose01_m(::Type{T}, n::Integer, m::Integer) where {T<:Integer} =
    (n > 128 && throw(ArgumentError("n must be ≤ 128")); _closeclose01_m(T, n, m))
_closeclose01_m(::Type{T}, n::Integer, m::Integer) where {T<:Integer} =
    n > 8*sizeof(T) ? _closeclose01_m(promote_type(T, widen(T)), n, m) : closeclose01_m(T, m)

# f(::Type{T}, m::Integer) where {T} = CloseClose01_m{T}(m)
# f(n::Integer, m::Integer) = f(__closeclose01_m(n), m)
# __closeclose01_m(n::Integer) = __closeclose01_m(__closeclose01_m(Val(n)))
# __closeclose01_m(::Type{T}) where {T} = T
# function __closeclose01_m(::Val{n}) where {n}
#     if n < 8
#         return UInt8
#     elseif n < 16
#         return UInt16
#     elseif n < 32
#         return UInt32
#     elseif n < 64
#         return UInt64
#     elseif n < 128
#         return UInt128
#     else
#         return BigInt
#     end
# end


# function randrational(r::AbstractRNG, i::CloseClose01_m{T, S}) where {T<:Union{UInt8,UInt16,UInt32,UInt64}, S<:Integer}
#     m = i.m
#     x = rand(r, UInt64) >>> (64 - m) % T
#     y = one(T) << m - one(T)
#     num, den = divgcd(x, y)
#     unsafe_rational(num, den)
# end
# function randrational(r::AbstractRNG, i::CloseClose01_m{T, S}) where {T<:Union{Int8,Int16,Int32,Int64}, S<:Integer}
#     m = i.m
#     x = rand(r, UInt64) >>> (64 - m) % T
#     y = typemax(T) >> (8*sizeof(T)-1 - m)
#     num, den = divgcd(x, y)
#     unsafe_rational(num, den)
# end

# Same, but through rand interface
function rand(r::AbstractRNG, sp::SamplerTrivial{CloseClose01_m{T, S}}) where {T<:Union{UInt8,UInt16,UInt32,UInt64}, S<:Integer}
    m = sp[].m
    x = rand(r, UInt64) >>> (64 - m) % T
    y = one(T) << m - one(T)
    num, den = divgcd(x, y)
    unsafe_rational(num, den)
end
function rand(r::AbstractRNG, sp::SamplerTrivial{CloseClose01_m{T, S}}) where {T<:Union{Int8,Int16,Int32,Int64}, S<:Integer}
    m = sp[].m
    x = rand(r, UInt64) >>> (64 - m) % T
    y = typemax(T) >> (8*sizeof(T)-1 - m)
    num, den = divgcd(x, y)
    unsafe_rational(num, den)
end
function rand(r::AbstractRNG, sp::SamplerTrivial{CloseClose01_m{UInt128, S}}) where {S<:Integer}
    m = sp[].m
    x = rand(r, UInt128) >>> (128 - m)
    y = one(UInt128) << m - one(UInt128)
    num, den = divgcd(x, y)
    unsafe_rational(num, den)
end
function rand(r::AbstractRNG, sp::SamplerTrivial{CloseClose01_m{Int128, S}}) where {S<:Integer}
    m = sp[].m
    x = rand(r, UInt128) >>> (128 - m) % Int128
    y = typemax(Int128) >> (127 - m)
    num, den = divgcd(x, y)
    unsafe_rational(num, den)
end

xo = Xoshiro(1234);
cc_64_63 = CloseClose01_m{UInt64}(0x3f)
rand(xo, cc_63_64)
ccu_8_3 = CloseClose01_m{UInt8}(0x03)
rand(ccu_8_3, 5, 2)
cci_8_3 = CloseClose01_m{Int8}(3)
rand(cci_8_3, 5, 2)
cc = CloseClose01_m{UInt64}(53)
Random.seed!(0x1234)
rs = rand(cc, 4)
Random.seed!(0x1234)
fs = rand(4)

sp = SamplerTrivial(cc)
@benchmark rand($sp)
@benchmark rand(Rational{UInt})
@benchmark rand($sp, 3)
@benchmark rand(Rational{UInt}, 3)
