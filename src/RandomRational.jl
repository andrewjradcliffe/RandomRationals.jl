#
# Date created: 2022-07-12
# Author: Radcliffe
#
#
############################################################################################
# Sketch of generation random rational numbers

using Random
import Random: Sampler, SamplerSimple, Repetition, rand, default_rng, Dims, SamplerType, SamplerTrivial, gentype

rng = Xoshiro()
sp = Sampler(rng, Int)
@timev rand(rng, sp)

@code_warntype rand(Int)
@code_warntype rand(default_rng(), Int)
sp = Sampler(default_rng(), Int)
@code_warntype rand(default_rng(), sp)

@code_warntype rand(Int, 2,2)

dims = Dims((2,2))
rand(Int, dims)
@code_warntype rand(Int, dims)

@code_warntype rand(default_rng(), Int, dims)
r = default_rng()
m = Array{Int}(undef, dims)
@code_warntype rand!(r, m, Int)

sp = Sampler(r, Int)
@code_warntype rand!(r, m, sp)

mf = Array{Float64}(undef, dims)
sp = Sampler(r, Float64)
@code_warntype rand!(r, mf, sp)

m2 = Array{Rational{Int}}(undef, dims)
sp2 = Sampler(r, Rational{Int})
@code_warntype rand!(r, m2, sp2)




# Sampler(::Type{RNG}, ::Type{T}, n::Repetition) where
import Base: unsafe_rational, divgcd

# rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Integer} =
#     # unsafe_rational(rand(r, T), rand(r, T))
#     # safe_rational(rand(r, T), rand(r, T))
#     # unsafe_rational()
#     rationalize(T, rand(r, float(T)), eps(float(T)))
#     # rationalize_closeopen01(T, rand(r, float(T)), eps(float(T)))
#     # Rational{T}(rand(r, float(T)))
#     # one(T) // rand(r, T)
# # rand(r, T) // rand(r, T)
# rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Union{Int16,UInt16}} =
#     rationalize(T, rand(r, Float64), eps(Float32))

# rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Union{Int8,UInt8}} =
#     rationalize(T, rand(r, Float32), √eps(Float32))

# # function rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Unsigned}
# #     n, d = divgcd(rand(r, T), rand(r, T))
# #     unsafe_rational(n, d)
# # end

# rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Integer} =
#     rand(r, T) // typemax(T)

# rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Union{Int8,Int16,Int32,Int64}} =
#     (rand(r, UInt64) >>> (65 - 8*sizeof(T)) % T) // typemax(T)

# rand(r::AbstractRNG, ::SamplerType{Rational{Int128}}) =
#     (rand(r, UInt128) >>> 1 % Int128) // typemax(Int128)

# @code_warntype rand(Rational{Int})
# sp = Sampler(default_rng(), Rational{Int})
# @code_warntype rand(default_rng(), sp)

# function rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Integer}
#     x = rand(r, T)
#     g = gcd(x, typemax(T))
#     unsafe_rational(x ÷ g, typemax(T) ÷ g)
# end

# function rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Union{Int8,Int16,Int32,Int64}}
#     x = rand(r, UInt64) >>> (65 - 8*sizeof(T)) % T
#     g = gcd(x, typemax(T))
#     unsafe_rational(x ÷ g, typemax(T) ÷ g)
# end

# function rand(r::AbstractRNG, ::SamplerType{Rational{Int128}})
#     x = rand(r, UInt128) >>> 1 % Int128
#     g = gcd(x, typemax(Int128))
#     unsafe_rational(x ÷ g, typemax(Int128) ÷ g)
# end

# #
# function rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Unsigned}
#     num, den = divgcd(rand(r, T), typemax(T))
#     unsafe_rational(num, den)
# end

# function rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Union{Int8,Int16,Int32,Int64}}
#     num, den = divgcd(rand(r, UInt64) >>> (65 - 8*sizeof(T)) % T, typemax(T))
#     unsafe_rational(num, den)
# end

# function rand(r::AbstractRNG, ::SamplerType{Rational{Int128}})
#     num, den = divgcd(rand(r, UInt128) >>> 1 % Int128, typemax(Int128))
#     unsafe_rational(num, den)
# end

# # slower
# function rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Signed}
#     x = rand(r, UInt64) >>> (65 - 8*sizeof(T)) % T
#     num, den = divgcd(x, typemax(T))
#     unsafe_rational(num, den)
# end

################
# uniform on the closed interval [0,1]
# one can proceed safely as the checks in unsafe_rational are guaranteed
# by construction; all one needs to do is find the numerator and denominator,
# then form the struct. Otherwise, generic fallback.
rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Integer} =
    rand(r, T) // typemax(T)

rand(r::AbstractRNG, ::SamplerType{Rational{Bool}}) = unsafe_rational(rand(r, Bool), true)

function rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Unsigned}
    num, den = divgcd(rand(r, T), typemax(T))
    unsafe_rational(num, den)
end

function rand(r::AbstractRNG, ::SamplerType{Rational{T}}) where {T<:Union{Int8,Int16,Int32,Int64}}
    num, den = divgcd(rand(r, UInt64) >>> (65 - 8*sizeof(T)) % T, typemax(T))
    unsafe_rational(num, den)
end

function rand(r::AbstractRNG, ::SamplerType{Rational{Int128}})
    num, den = divgcd(rand(r, UInt128) >>> 1 % Int128, typemax(Int128))
    unsafe_rational(num, den)
end
################

@benchmark rand(Rational{Int8})
@benchmark rand(Rational{Int16})
@benchmark rand(Rational{Int32})
@benchmark rand(Rational{Int64})
@benchmark rand(Rational{Int128})

@benchmark rand(Rational{UInt8})
@benchmark rand(Rational{UInt16})
@benchmark rand(Rational{UInt32})
@benchmark rand(Rational{UInt64})
@benchmark rand(Rational{UInt128})


T = Rational{Int64}

is = rand(Int, 50);
mn = typemin(Int)
msk = 0x7fffffffffffffff
ma = 0xffffffffffffffff
mn & msk
Int.(is .& msk)
i1 = Int8(1)
bitstring(-1 & msk)
bitstring(-1 << 1)
bitstring(-1 << 1 >>> 1)
bitstring(mn << 1 >>> 1)
big(mn) << 1 >>> 1
(mn + 1) << 1 >>> 1
bitstring(typemax(Int) + 1)

e = 0x1.0p-53
bitstring(e)
mf = Float64(ma >>> 11)
bitstring(ma >>> 11)

prevfloat(1.0) * maxintfloat()
1.0 * maxintfloat()
nextfloat(1.0) * maxintfloat()
m64 = Int(maxintfloat())
Float64(m64 + 1)
Float64(m64 + 2)

bitstring(m64)
bitstring(m64 + 1)
bitstring(m64 + 2)
bitstring(Float64(m64))
bitstring(Float64(m64 + 1))
bitstring(Float64(m64 + 2))

# Let's use Int64 since it is easier to read off than UInt64's
m64 = Int64(maxintfloat(Float64)) # 2^53
r1 = m64 // typemax(Int64)
r2 = (m64 + 1) // typemax(Int64)
r3 = (m64 + 2) // typemax(Int64)
f1 = m64 / typemax(Int64)
f2 = (m64 + 1) / typemax(Int64)  # No hope since 2^53 + 1 is has no Float64 representation
f3 = (m64 + 2) / typemax(Int64)  # 2^53 + 2 does have a Float64 representation
rationalize(Int64, f1, tol=eps()/2)
rationalize(Int64, f2, tol=eps()/2)
rationalize(Int64, f3, tol=eps()/2)
rationalize(Int64, f3, tol=eps()/10)
rationalize(Int64, f3, tol=eps()/100)
rationalize(Int64, f3, tol=eps()/1000)
rationalize(Int64, f3, tol=eps()/1025)    # We must use a tolerance < 2^-62

# How much of the unit interval is affected?
f(n, p) = (big(2)^n - big(2)^p) / big(2)^n
f(64, 53)
f(63, 53)
# Or, another way to phrase it is: for any float > 2^p / 2^n, the probability mass will be incorrect.
big(2)^53 / big(2)^64    # Threshold for UInt64
big(2)^53 / big(2)^63    # Threshold for Int64

# Let's take a randomly generated Rational{Int64}
num = 3929436088528050567
den = typemax(Int)
r = 3929436088528050567//9223372036854775807
s = (3929436088528050567+1)//9223372036854775807
float(r) == float(s)    # Obviously cannot distinguish between the float(r.num) and float(s.num)
numf = Float64(num)
eps(numf)    # The nearest float (rounds up) -- far away!
i_start = Int(prevfloat(numf))
i_end = Int(numf)
δ_back = num - i_start
δ_forward = i_end - num
# If there were no other roundoff errors, then Float64(i_start + k) == Float64(i_end),
# for 256 < k < 768, but, alas, roundoff in the denominator as well causes this to
# not work out as neatly as one would like.
s = (i_start + 257) // den      # k > 256 gets rounded up, hence, 256 rationals disappear
float(s) == float(r)
s = (i_start + 767) // den      # In total, 511 rationals are stacked into the same float
float(s) == float(r)
# If we were to convert from the floating point representation, the rational corresponding
# to 0.42603031438250555 would exhibit a probability mass 510x greater than it should be.
is = findall(float((i_start + k) // den) == float(r) for k = 257:767);
ks = setdiff(257:767, (257:767)[is])
s = (i_start + 276) // den
float(s)    # rounds down, so not quite consistent, even in rounding.
float.((i_start .+ ks) .// den)    # sometimes rounds up before it should
float((i_start + 729) // den)      # more rounding than one would expect


s = (num + δ_forward) // den    # Anything between

float(s) == float(r)

x = (i_start + 257) // den





rationalize(nextfloat(1/1024), tol=eps()/2)

eps(Float64(2^54))

n, p = 64, 53
(big(2)^n - big(2)^p) / big(2)^n

maxintfloat(Float32) == 2^precision(Float32)
maxintfloat(Float32) / typemax(Int32)
maxintfloat(Float16) / typemax(Int16)
################
# 2022-07-18
g1(::Type{T}, m::Integer) where {T<:Unsigned} = (one(T) << m) - one(T)
g2(::Type{T}, m::Integer) where {T<:Unsigned} = typemax(T) >>> (8*sizeof(T) - m)
@benchmark g1(UInt8, 8)
@benchmark g2(UInt8, 8)
@benchmark g1(UInt64, 64)
@benchmark g2(UInt64, 64)
@code_warntype g1(UInt8, 8)
@code_warntype g2(UInt8, 8)

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
# # Base.eltype(::RationalInterval{T}) where {T<:Integer} = Rational{T}
# Base.eltype(::SamplerTrivial{CloseClose01_m{T, S}}) where {T, S} = Rational{T}
# Base.eltype(::SamplerType{CloseClose01{T}}) where {T} = Rational{T}
# Base.eltype(::Type{SamplerType{<:RationalInterval{T}}}) where {T<:Integer} = Rational{T}

# gentype(::Type{<:RationalInterval{T}}) where {T<:Integer} = Rational{T}
# gentype(::Type{<:SamplerType{CloseClose01{T}}}) where {T<:Integer} = Rational{T}

closeclose01_m(::Type{T}, n::Integer, m::Integer) where {T<:Integer} =
    n > 8*sizeof(T) ? closeclose01_m(promote_type(T, widen(T)), n, m) : closeclose01_m(T, m)
closeclose01_m(n::Integer, m::Integer) = closeclose01_m(UInt8, n, m)
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


Sampler(::Type{RNG}, cc::CloseClose01_m{T, S}, ::Repetition) where {RNG<:AbstractRNG, T<:Integer, S<:Integer} = SamplerTrivial(cc)

SamplerTrivial(cc::CloseClose01_m{T, S}) where {T, S} = SamplerTrivial{CloseClose01_m{T,S},Rational{T}}(cc)

function rand(r::AbstractRNG, ::SamplerType{CloseClose01{T}}) where {T<:Unsigned}
    num, den = divgcd(rand(r, T), typemax(T))
    unsafe_rational(num, den)
end

#### using SamplerSimple
# sp = SamplerSimple(SamplerType{Rational{Int}}(), 3)
# function rand(rng::AbstractRNG, sp::SamplerSimple{SamplerType{Rational{T}}, S::Integer, U::Rational{T}}) where {T<:Integer}
#     randrational(rng, Rational{T}, sp.data)
# end
################################
#### Morgenstern 2006 -- Section 1
# Some attempts with horrible seeds
N = 10^6
g(t) = cos(2 * π * 10^10 * t);    # ∫₀¹ g(t)dt = 0
g²(t) = abs2(g(t));               # ∫₀¹ g²(t)dt = 0.5
mc(r, f, N) = sum(f(rand(r)) for _ = 1:N) / N;

xo = Xoshiro(0x1);
mc(xo, g, N)
mc(xo, g², N)

mt = MersenneTwister(0x1);
mc(mt, g, N)
mc(mt, g², N)

mutable struct LCG10{A<:Integer, S<:Integer, M<:Integer}
    a::A
    s::S
    m::M
end;
rand(r::LCG10, ::Type{Float64}) = _next(r) / r.m;
rand(r::LCG10, ::Type{T}) where {T<:AbstractFloat} = T(_next(r)) / T(r.m);
rand(r::LCG10) = rand(r, Float64);
function _next(r::LCG10)
    (; a, s, m) = r
    s = a * s % m
    r.s = s
    s
end;

lcg10 = LCG10(UInt(427419669081), UInt(0x1), UInt(999999999989));
mc(lcg10, g, N)
mc(lcg10, g², N)
sum(g(Float32(rand(lcg10))) for _ = 1:N) / N
sum(g²(Float32(rand(lcg10))) for _ = 1:N) / N
sum(g(Float16(rand(lcg10))) for _ = 1:N) / N
sum(g²(Float16(rand(lcg10))) for _ = 1:N) / N # Well, barbaric rounding gets us somewhere...
setprecision(BigFloat, 10, base=10)
sum(g(BigFloat(rand(lcg10))) for _ = 1:N) / N # To be fair, not everything is same precision
sum(g²(BigFloat(rand(lcg10))) for _ = 1:N) / N
g(t, ::Type{T}) where {T<:AbstractFloat} = cos(2 * T(π) * 10^10 * t)
g²(t, ::Type{T}) where {T<:AbstractFloat} = abs2(g(t, T))
sum(g(rand(lcg10, BigFloat), BigFloat) for _ = 1:N) / N  # well, puzzling
sum(g²(rand(lcg10, BigFloat), BigFloat) for _ = 1:N) / N
# The above is just a superficial reproduction of the math, but not necessarily the
# exact arithmetic Maple 10 would have used. Could this explain my inability to reproduce?
# Noting that I have never used Maple, but have used MATLAB extensively, I have a hunch
# that Maple 10's decimal floating point was doing some magic behind the scenes
# (to force certain mathematical identities, which are true of real numbers, but not floats).
# One would have a reasonable expectation that BigFloat's (which obeys IEEE rounding) with
# precision set to 10 decimal digits should be able to elicit similar behavior.
# I believe I am in good company (https://people.eecs.berkeley.edu/~wkahan/Mindless.pdf)
# in being suspicious of (decimal) floating point which does not explicitly conform to
# IEEE rounding. Note that I am not trying to dismiss my inability to reproduce
# Morgenstern's results, but without a Maple 10 from 2006, it's probably hopeless.

# On the subject of results in Section 1, they are partly dependent on the nature of LCG's.
# If we attempt to work in rationals as far as possible, carrying the relatively few
# denominators offered by Rational{Int} until the point at which we must use a float (cos),
# we do not observe the problem which seems to emanate from the LCG. (Though, this assumes
# that I could replicate the floating point behavior, which I can't).
const πᵣ = rationalize(Int, π, tol=2^-62) # absurd on its face, no?
𝑔(t) = cos(2 * πᵣ * (10^10 * t))
𝑔²(t) = abs2(𝑔(t))
sum(𝑔(Rational{BigInt}(rand(Rational{Int}))) for _ = 1:N) / N
sum(𝑔²(Rational{BigInt}(rand(Rational{Int}))) for _ = 1:N) / N
sum(g(rand(Rational{Int})) for _ = 1:N) / N
sum(g²(rand(Rational{Int})) for _ = 1:N) / N

π32 = Float32(π)
g32(t) = cos(2 * π32 * 10^10 * t)^2
sum(g32(Float32(rand(lcg10))) for _ = 1:10^6) / 10^6

sum(g(rand()) for _ = 1:10^6) / 10^6

#
# pred(::Type{T}) where {T<:AbstractFloat} = t(T) ≥ maxintfloat(T)
lcg10 = LCG10(UInt(427419669081), UInt(0x1), UInt(999999999989));
setprecision(BigFloat, 10, base=10)
maxintfloat(BigFloat)    # already close to 10^10
t(::Type{T}) where {T<:AbstractFloat} = 2 * T(π) * 10^10 * rand(lcg10, T);
t(BigFloat)
count(t(BigFloat) ≥ maxintfloat(BigFloat) for _ = 1:N) / N    # Loss of precision very often
count(t(Float64) ≥ maxintfloat() for _ = 1:N) / N    # No loss

################################
# Well, there are other floats...
u64 = typemax(UInt64)
lastsubnormal = reinterpret(Float64, u64 >>> 12)
mn64 = u64 >>> 63
Float64(mn64) * 0x1.0p-53

lb(a, c, d) = c + a * (d - c)
ub(b, c, d) = c + b * (d - c)
a, b = 0, 1
c, d = 5, 15
lb(a, c, d)
ub(b, c, d)

################################
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

cci_8_3_6 = CloseClose0d{Int8}(3, 6)
rand(cci_8_3_6, 3,3)
# @benchmark rand($cci_8_3_6)
ccu_8_5_7 = CloseClose0d{UInt8}(5, 7)
rand(ccu_8_5_7, 3,3)
# @benchmark rand($cci)

bitstring(0x2a)
################################
# The problem of reducibility -- Petroni-2019
r8s = rand(Rational{Int8}, 10^6);
c8s = [count(==(i//127), r8s) for i = 0:127];
p = histogram(r8s);
savefig(p, joinpath(pwd(), "Int8_hist.pdf"))
r16s = rand(Rational{Int16}, 10^8);
count_rational(A::AbstractArray{Rational{T}}) where {T} = map(0:typemax(T)) do i
    count(==(i//typemax(T)), A)
end
function tcount_rational(A::AbstractArray{Rational{T}}) where {T}
    r = zero(T):typemax(T)
    B = Vector{Int}(undef, length(r))
    Threads.@threads for i ∈ r
        B[begin + i] = count(==(i//typemax(T)), A)
    end
    B
end
c16s = tcount_rational(r16s);
# c16s = [count(==(i//32767), r16s) for i = 0:32767];
p = bar(c16s);
savefig(p, joinpath(pwd(), "Int16_bar.pdf"))

u8s = rand(Rational{UInt8}, 10^8);
c8s = tcount_rational(u8s);
n8s = numerator.(u8s);
d8s = denominator.(u8s);
nums = sort!(unique(n8s));
dens = sort!(unique(d8s));
nc8s = map(i -> count(==(i), n8s), nums);
dc8s = map(i -> count(==(i), d8s), dens);
p = bar(c8s);
savefig(p, joinpath(pwd(), "UInt8_bar.pdf"))
p = bar(c8s .- Int(10^8 / 256));
vline!(p, dens)
savefig(p, joinpath(pwd(), "UInt8_bar_delta.pdf"))
p = bar(nc8s, xticks=nums);
savefig(p, joinpath(pwd(), "UInt8_bar_nums.pdf"))
p = bar(dc8s, xticks=(1:length(dc8s), dens));
savefig(p, joinpath(pwd(), "UInt8_bar_dens.pdf"))

uu8s = unique(u8s);
ii = findfirst(x -> x.den == 0x05, u8s);
u8s[ii] # 0x03//0x05
fac = 255 ÷ 5
UInt8(fac * 0x03)

for T ∈ (Int8, Int16, Int32, Int64)
    rs = rand(Rational{T}, 10^6)
    p = histogram(rs);
    savefig(p, joinpath(pwd(), "$(T)_hist.pdf"))
end

function rand_petroni(r::AbstractRNG, ::Type{Rational{T}}) where {T<:Integer}
    k = typemax(T)
    m = rand(one(T):k)
    n = rand(zero(T):m)
    num, den = divgcd(n, m)
    unsafe_rational(num, den)
end
rand_petroni(::Type{Rational{T}}) where {T<:Integer} = rand_petroni(default_rng(), Rational{T})

# Alternative: hooking into Random interface
struct Petroni{T<:Integer}
    r::Rational{T}
end
Base.eltype(::Type{S}) where {S<:Petroni{T}} where {T<:Integer} = Petroni{Rational{T}}
unwrap(x::Petroni{T}) where {T} = x.r

function rand(r::AbstractRNG, ::SamplerType{Petroni{T}}) where {T<:Integer}
    k = typemax(T)
    m = rand(one(T):k)
    n = rand(zero(T):m)
    num, den = divgcd(n, m)
    Petroni(unsafe_rational(num, den))
end


u8s_p = [rand_petroni(Rational{UInt8}) for _ = 1:10^8];
fu8s_p = float.(u8s_p);
c8s_p = tcount_rational(u8s_p);
n8s_p = numerator.(u8s_p);
d8s_p = denominator.(u8s_p);
nums_p = sort!(unique(n8s_p));
dens_p = sort!(unique(d8s_p));
nc8s_p = map(i -> count(==(i), n8s_p), nums_p);
dc8s_p = map(i -> count(==(i), d8s_p), dens_p);
p = bar(c8s_p);
savefig(p, joinpath(pwd(), "UInt8_bar_petroni.pdf"))
p = bar(c8s_p .- Int(10^8 / 256));
vline!(p, dens_p)
savefig(p, joinpath(pwd(), "UInt8_bar_delta_petroni.pdf"))
p = histogram(u8s_p);
savefig(p, joinpath(pwd(), "UInt8_hist_petroni.pdf"))
uu8s_p = unique(u8s_p);
savefig(p, joinpath(pwd(), "UInt8_hist_petroni.pdf"))
p = histogram(fu8s_p);
savefig(p, joinpath(pwd(), "Float64_UInt8_hist_petroni.pdf"))
for l ∈ (10, 100, 1000)
    p = histogram(fu8s_p[1:l]);
    savefig(p, joinpath(pwd(), "Float64_UInt8_hist_petroni_$(l).pdf"))
end
p = histogram(n8s_p);
savefig(p, joinpath(pwd(), "UInt8_hist_petroni_nums.pdf"))
p = histogram(d8s_p);
savefig(p, joinpath(pwd(), "UInt8_hist_petroni_dens.pdf"))


u16s_p = [rand_petroni(Rational{UInt16}) for _ = 1:10^8];
fu16s_p = float.(u16s_p);
c16s_p = tcount_rational(u16s_p);
n16s_p = numerator.(u16s_p);
d16s_p = denominator.(u16s_p);
dens_p = sort!(unique(d16s_p));
dc16s_p = asyncmap(i -> fetch(Threads.@spawn count(==(i), d16s_p)), dens_p);
p = bar(c16s_p);
savefig(p, joinpath(pwd(), "UInt16_bar_petroni.pdf"))
p = bar(c16s_p .- Int(10^16 / 256));
vline!(p, dens_p)
savefig(p, joinpath(pwd(), "UInt16_bar_delta_petroni.pdf"))
p = histogram(u16s_p);
savefig(p, joinpath(pwd(), "UInt16_hist_petroni.pdf"))
p = histogram(fu16s_p);
savefig(p, joinpath(pwd(), "Float64_UInt16_hist_petroni.pdf"))
for l ∈ (10, 100, 1000)
    p = histogram(fu16s_p[1:l]);
    savefig(p, joinpath(pwd(), "Float64_UInt16_hist_petroni_$(l).pdf"))
end
p = histogram(n16s_p);
savefig(p, joinpath(pwd(), "UInt16_hist_petroni_nums.pdf"))
p = histogram(d16s_p);
savefig(p, joinpath(pwd(), "UInt16_hist_petroni_dens.pdf"))

####
for T ∈ (Int8, Int16, Int32, Int64) #(UInt32, UInt64)
    us_p = [rand_petroni(Rational{T}) for _ = 1:10^8];
    fus_p = float.(us_p);
    # cs_p = tcount_rational(us_p);
    ns_p = numerator.(us_p);
    ds_p = denominator.(us_p);
    # dens_p = sort!(unique(ds_p));
    # dcs_p = asyncmap(i -> fetch(Threads.@spawn count(==(i), ds_p)), dens_p);
    # p = bar(cs_p);
    # savefig(p, joinpath(pwd(), "$(T)_bar_petroni.pdf"))
    # p = bar(cs_p .- Int(10^16 / 256));
    # vline!(p, dens_p)
    # savefig(p, joinpath(pwd(), "$(T)_bar_delta_petroni.pdf"))
    p = histogram(us_p);
    savefig(p, joinpath(pwd(), "$(T)_hist_petroni.pdf"))
    p = histogram(fus_p);
    savefig(p, joinpath(pwd(), "Float64_$(T)_hist_petroni.pdf"))
    for l ∈ (10, 100, 1000)
        p = histogram(fus_p[1:l]);
        savefig(p, joinpath(pwd(), "Float64_$(T)_hist_petroni_$(l).pdf"))
    end
    p = histogram(ns_p);
    savefig(p, joinpath(pwd(), "$(T)_hist_petroni_nums.pdf"))
    p = histogram(ds_p);
    savefig(p, joinpath(pwd(), "$(T)_hist_petroni_dens.pdf"))
end

############################################################################################
############################################################################################
############################################################################################

us = rand(Rational{Int8}, 10^6);
cs = [count(==(i//typemax(Int8)), us) for i = Int8(0):typemax(Int8)];
fs = rand(10^6);
rs = rationalize.(Int8, fs);
cs2 = [count(==(i//typemax(Int8)), rs) for i = Int8(0):typemax(Int8)];

p1 = histogram(us, nbins=128, normalize=true);
p2 = histogram(rs, nbins=128, normalize=true);
p3 = plot(p1, p2);
savefig(p3, joinpath("/nfs/site/home/aradclif/aradclif/jldir/juliamanual", "rational_Int8_hists.pdf"))

us = rand(Rational{Int}, 10^6);
fs = rand(10^6);
rs = rationalize.(Int, fs);
p1 = histogram(us, label=nothing);
p2 = histogram(rs, label=nothing);
p3 = plot(p1, p2);
savefig(p3, joinpath("/nfs/site/home/aradclif/aradclif/jldir/juliamanual", "rational_Int64_hists.pdf"))




us = rand(Rational{Int8}, 10^6);
fs = Float64.(us);
mean(fs)
var(fs)
using Plots
unicodeplots()
histogram(fs)

for T ∈ (Int8, Int16, Int32, Int64, Int128)
    @btime rand(Rational{T})
end


@benchmark rand(Rational{UInt})
@benchmark rand(Int)
@benchmark rand(Complex{Float64})

@benchmark rand(Rational{BigInt})

@benchmark rand(Rational{Int}, 2^16)
@benchmark rationalize.(rand(2^16))
@benchmark rand(Int, 2^6)

@benchmark rand(Complex{Float64}, 2^6)

c = unsafe_rational(10, typemin(Int))

a
b
d = -10

x, y = rand(Int), rand(Int)
g = gcd(x, y)
div(x, g)

function safe_rational(n::Integer, d::Integer)
    num, den = divgcd(n, d)
    if signbit(den)
        den = -den
        num = -num
    end
    unsafe_rational(num, den)
end



us = rand(Rational{Int}, 10^6);
fs = float.(us);

import Base: divgcd, checked_add, checked_mul

function rationalize_closeopen01(::Type{T}, x::AbstractFloat, tol::Real) where T<:Integer
    p,  q  = one(T), zero(T)#(x < 0 ? -one(T) : one(T)), zero(T)
    pp, qq = zero(T), one(T)

    # x = abs(x) # we know x is positive
    # a = trunc(x) # and a will always be zero
    a = zero(x)
    r = x#-a
    y = one(x)

    tolx = oftype(x, tol)
    nt, t, tt = tolx, zero(tolx), tolx
    ia = np = nq = zero(T)

    # compute the successive convergents of the continued fraction
    #  np // nq = (p*a + pp) // (q*a + qq)
    while r > nt
        try
            ia = convert(T,a)

            np = checked_add(checked_mul(ia,p),pp)
            nq = checked_add(checked_mul(ia,q),qq)
            p, pp = np, p
            q, qq = nq, q
        catch e
            isa(e,InexactError) || isa(e,OverflowError) || rethrow()
            return p // q
        end

        # naive approach of using
        #   x = 1/r; a = trunc(x); r = x - a
        # is inexact, so we store x as x/y
        x, y = y, r
        a, r = divrem(x,y)

        # maintain
        # x0 = (p + (-1)^i * r) / q
        t, tt = nt, t
        nt = a*t+tt
    end

    # find optimal semiconvergent
    # smallest a such that x-a*y < a*t+tt
    a = cld(x-tt,y+t)
    try
        ia = convert(T,a)
        np = checked_add(checked_mul(ia,p),pp)
        nq = checked_add(checked_mul(ia,q),qq)
        return np // nq
    catch e
        isa(e,InexactError) || isa(e,OverflowError) || rethrow()
        return p // q
    end
end

function unsafe_rationalize_closeopen01(::Type{T}, x::AbstractFloat, tol::Real) where T<:Integer
    p,  q  = one(T), zero(T)#(x < 0 ? -one(T) : one(T)), zero(T)
    pp, qq = zero(T), one(T)

    # x = abs(x) # we know x is positive
    # a = trunc(x) # and a will always be zero
    a = zero(x)
    r = x#-a
    y = one(x)

    tolx = oftype(x, tol)
    nt, t, tt = tolx, zero(tolx), tolx
    ia = np = nq = zero(T)

    # compute the successive convergents of the continued fraction
    #  np // nq = (p*a + pp) // (q*a + qq)
    while r > nt
        try
            ia = convert(T,a)

            np = checked_add(checked_mul(ia,p),pp)
            nq = checked_add(checked_mul(ia,q),qq)
            p, pp = np, p
            q, qq = nq, q
        catch e
            isa(e,InexactError) || isa(e,OverflowError) || rethrow()
            return unsafe_rational(p, q)
        end

        # naive approach of using
        #   x = 1/r; a = trunc(x); r = x - a
        # is inexact, so we store x as x/y
        x, y = y, r
        a, r = divrem(x,y)

        # maintain
        # x0 = (p + (-1)^i * r) / q
        t, tt = nt, t
        nt = a*t+tt
    end

    # find optimal semiconvergent
    # smallest a such that x-a*y < a*t+tt
    a = cld(x-tt,y+t)
    try
        ia = convert(T,a)
        np = checked_add(checked_mul(ia,p),pp)
        nq = checked_add(checked_mul(ia,q),qq)
        return unsafe_rational(np, nq)
    catch e
        isa(e,InexactError) || isa(e,OverflowError) || rethrow()
        return unsafe_rational(p, q)
    end
end


function check_closeopen01(::Type{Rational{T}}, lb, ub) where {T<:Integer}
    # ub = 1.0
    # lb = 0.9999999
    ϵ = eps()
    n = 0
    x = prevfloat(ub)
    while x > lb
        # try
        #     rationalize(Rational{T}, lb, ϵ)
        # catch e
        #     println("lb = ", lb, " n = ", n)
        #     rethrow()
        # end
        rationalize(T, x, ϵ)
        x = prevfloat(x)
        n += 1
    end
    n
end

using Test

ϵ = eps()
for _ = 1:10^7
    u = rand()
    @test rationalize_closeopen01(Int, u, ϵ) == unsafe_rationalize_closeopen01(Int, u, ϵ)
end
@benchmark rationalize(Int, $u, $ϵ)
@benchmark rationalize_closeopen01(Int, $u, $ϵ)
@benchmark unsafe_rationalize_closeopen01(Int, $u, $ϵ)
