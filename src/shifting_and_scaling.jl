#### On shifting and scaling
# If r is an n-bit rational on the unit interval, the discretization of said interval
# is into 2^n pieces, with lower bound 0/(2^n - 1) and upper bound (2^n - 1)/(2^n - 1)
# The original distance between points is 1/(2^n - 1), such that the sequence is
# 0, 1/(2^n - 1), 2/(2^n - 1), …, (2^n - 2)/(2^n - 1), (2^n - 1)/(2^n - 1)
## If one relocates by adding a constant, c, the lower bound becomes c*(2^n - 1)/(2^n - 1)
# and the upper bound (c+1)*(2^n - 1)/(2^n - 1). The discretization is unaffected; it
# still contains 2^n pieces.
# The distance between points remains 1/(2^n - 1), but the sequence becomes
# c, c + 1/(2^n - 1), c + 2/(2^n - 1), …, c + (2^n - 2)/(2^n - 1), c + (2^n - 1)/(2^n - 1)
## If one re-scales by multiplying by a constant, d, the lower bound remains
# 0/(2^n - 1), but the upper bound becomes d*(2^n - 1)/(2^n - 1).
# The distance between points becomes d/(2^n - 1), such that the sequence becomes
# 0, d/(2^n - 1), 2d/(2^n - 1), …, d*(2^n - 2)/(2^n - 1), d*(2^n - 1)/(2^n - 1)
# If one wanted to achieve the same discretization as the unit interval, one would
# need to use a denominator d*(2^n - 1), such that the distance between points becomes
# d/(d*(2^n - 1)) = 1/(2^n - 1).
# If d*(2^n - 1) can be expressed as an equivalent n′-bit integer, this greatly facilitates
# matters. d = 2^n + 1 is a convenient example, as (2^n + 1)*(2^n - 1) = 2^2n - 1, thus,
# if n=32, then we can achieve an equivalent discretization on the new interval by using n′=64.
# Clearly, it is exceedingly rare for things to work out so nicely, or even to yield a
# denominator which can be expressed as 2^n′ - 1.
# It is the generation of the numerator which causes difficulty; one might consider
# (and has little other recourse than) an accept-reject algorithm to handle
# denominators which cannot be expressed as 2^n′ - 1.
# If we say n=3, d=6, then 1/7 is the original distance, for which the scaled distance
# would be 6/7.  To achieve the same discretization, one needs a new denominator,
# d*(2^n - 1) = 42 = 0b00101010 = 0x2a
# There are 42 feasible bit patterns ≤ 0x2a, and assuming a uniform stream of bits,
# the distribution on them is uniform. We can improve the efficiency of the
# accept-reject step by discarding all integers which are > 0b00111111; that is,
# we need only consider random integers which have ≤ leading zeros than d*(2^n - 1).
# Or, in other words, random m′-bit integers where m′ = n + ceil(Int, log2(d))

# function rand_acceptreject(r::AbstractRNG, ::Type{Rational{T}}, m::Integer, d::Integer) where {T<:Union{UInt8, UInt16, UInt32, UInt64}}
#     n = 8*sizeof(T)
#     0 < m ≤ n || throw(ArgumentError("m must be 0 < m ≤ 8*sizeof($T)"))
#     y = (d * (one(T) << m - one(T))) % T # (d * (T(2)^m - 1)) % T
#     # x = rand(r, UInt64) % T
#     # while x > y
#     #     x = rand(r, UInt64) % T
#     # end
#     m′ = n - leading_zeros(y)
#     x = rand(r, UInt64) >>> (64 - m′) % T
#     while x > y
#         x = rand(r, UInt64) >>> (64 - m′) % T
#     end
#     num, den = divgcd(x, y)
#     unsafe_rational(num, den)
# end
# function rand_acceptreject(r::AbstractRNG, ::Type{Rational{T}}, m::Integer, d::Integer) where {T<:Union{Int8, Int16, Int32, Int64}}
#     n = 8*sizeof(T)
#     0 < m ≤ n-1 || throw(ArgumentError("m must be 0 < m ≤ 8*sizeof($T)-1"))
#     y = (d * (typemax(T) >> (n-1 - m))) % T # (d * (T(2)^m - 1)) % T
#     # x = rand(r, UInt64) % T
#     # while x > y
#     #     x = rand(r, UInt64) % T
#     # end
#     m′ = n - leading_zeros(y)
#     x = rand(r, UInt64) >>> (64 - m′) % T
#     while x > y
#         x = rand(r, UInt64) >>> (64 - m′) % T
#     end
#     num, den = divgcd(x, y)
#     unsafe_rational(num, den)
# end
# rand_acceptreject(::Type{Rational{T}}, m::Integer, d::Integer) where {T<:Integer} = rand_acceptreject(default_rng(), Rational{T}, m, d)

# m, d = 3, 6
# rand_acceptreject(Rational{UInt8}, m, d)
# @benchmark rand_acceptreject(Rational{Int8}, m, d)

# cci_8_3 = CloseClose01_m{Int8}(Int8(3))
# rand(cci_8_3)

#### using a type
using Random
import Random: rand, default_rng, SamplerTrivial
import Base: checked_mul, divgcd, unsafe_rational

abstract type RationalInterval{T<:Integer} end
Base.eltype(::Type{S}) where {S<:RationalInterval{T}} where {T<:Integer} = Rational{T}

denom(::Type{T}, m::Integer, d::Integer) where {T<:Signed} = checked_mul(T(d), typemax(T) >> (8*sizeof(T)-1 - m)) % T
denom(::Type{T}, m::Integer, d::Integer) where {T<:Unsigned} = checked_mul(T(d), (one(T) << m - one(T))) % T
struct CloseClose0d{T<:Integer, S<:Integer} <: RationalInterval{T}
    m::S # size of original m-bit integer
    d::S # rescaling factor for interval, e.g. [0,1] → [0,6] ⟹ d=6
    y::T # d*(2^8sizeof(T) - 1)
    m′::S # number of active bit positions, i.e. size of rescaled -bit integer
    global unsafe_closeclose0d(::Type{T}, m::S, d::S, y::T, m′::S) where {T,S} = new{T,S}(m,d,y,m′)
end
function CloseClose0d{T}(m::S, d::S) where {T<:Unsigned, S<:Integer}
    n = 8*sizeof(T)
    0 < m ≤ n || throw(ArgumentError("m must be 0 < m ≤ 8*sizeof($T)"))
    y = denom(T, m, d)
    m′ = (n - leading_zeros(y)) % S
    unsafe_closeclose0d(T, m, d, y, m′)
end
function CloseClose0d{T}(m::S, d::S) where {T<:Signed, S<:Integer}
    n = 8*sizeof(T)
    0 < m ≤ n-1 || throw(ArgumentError("m must be 0 < m ≤ 8*sizeof($T)-1"))
    y = denom(T, m, d)
    m′ = (n - leading_zeros(y)) % S
    unsafe_closeclose0d(T, m, d, y, m′)
end

function acceptreject(r::AbstractRNG, ::Type{T}, y::T, m′::Integer) where {T<:Integer}
    x = rand(r, UInt64) >>> (64 - m′) % T
    while x > y
        x = rand(r, UInt64) >>> (64 - m′) % T
    end
    x
end
function acceptreject(r::AbstractRNG, ::Type{UInt128}, y::T, m′::Integer)
    x = rand(r, UInt128) >>> (128 - m′)
    while x > y
        x = rand(r, UInt128) >>> (128 - m′)
    end
    x
end
function acceptreject(r::AbstractRNG, ::Type{Int128}, y::T, m′::Integer)
    x = rand(r, UInt128) >>> (128 - m′) % Int128
    while x > y
        x = rand(r, UInt128) >>> (128 - m′) % Int128
    end
    x
end
acceptreject(::Type{T}, y::T, m′::Integer) where {T<:Integer} = acceptreject(default_rng(), T, y, m′)

function rand(r::AbstractRNG, sp::SamplerTrivial{CloseClose0d{T, S}}) where {T<:Integer, S<:Integer}
    (; m, d, y, m′)= sp[]
    x = acceptreject(r, T, y, m′)
    num, den = divgcd(x, y)
    unsafe_rational(num, den)
end

# cci_8_3_6 = CloseClose0d{Int8}(3, 6)
# rand(cci_8_3_6, 3,3)
# # @benchmark rand($cci_8_3_6)
# ccu_8_5_7 = CloseClose0d{UInt8}(5, 7)
# rand(ccu_8_5_7, 3,3)
# # @benchmark rand($cci)

# bitstring(0x2a)
