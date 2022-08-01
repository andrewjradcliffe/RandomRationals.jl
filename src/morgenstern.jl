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
