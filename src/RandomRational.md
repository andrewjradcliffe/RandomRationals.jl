Another long post--

# On usage and flexibility
The above is rigid: n-bit rationals stored in n-bit integers, which, to perform subsequent arithmetic, necessitates a widening of the storage (e.g. `Rational{UInt64}` to `Rational{UInt128`) -- that is, an n-bit rational stored in an n-bit integer is on the precipice of overflow in most circumstances.
However, the above contains a kernel of a flexible idea: m-bit rationals stored in n-bit integers, where m ≤ n -- it reduces to the above at m=n. Such an approach enables one to select any discretization of the unit interval one desires.
A nice toy example can be m = 3, n=8.
It does not require much modification to the methods -- I am not necessarily proposing that it be included as functionality in base, as it is a little awkward as phrased below:
```julia
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
```
An example (without hooking into `rand` interface)
```julia
julia> [randrational(Rational{UInt8}, 3) for _ = 1:10]
10-element Vector{Rational{UInt8}}:
 0x02//0x07
 0x06//0x07
 0x00//0x01
 0x00//0x01
 0x06//0x07
 0x00//0x01
 0x03//0x07
 0x03//0x07
 0x02//0x07
 0x00//0x01
```



# On Morgenstern-2006
```julia
#### Morgenstern 2006 -- Section 1
# Some attempts with horrible seeds
julia> N = 10^6;

julia> g(t) = cos(2 * π * 10^10 * t);    # ∫₀¹ g(t)dt = 0

julia> g²(t) = abs2(g(t));               # ∫₀¹ g²(t)dt = 0.5

julia> mc(r, f, N) = sum(f(rand(r)) for _ = 1:N) / N;

julia> xo = Xoshiro(0x1);

julia> mc(xo, g, N)

-0.0002710728024089284

julia> mc(xo, g², N)
0.5002416981725267

julia> mt = MersenneTwister(0x1);

julia> mc(mt, g, N)
0.0007549637067530266

julia> mc(mt, g², N)
0.5002242685930319

julia> mutable struct LCG10{A<:Integer, S<:Integer, M<:Integer}
           a::A
           s::S
           m::M
       end;

julia> rand(r::LCG10, ::Type{Float64}) = _next(r) / r.m;

julia> rand(r::LCG10, ::Type{T}) where {T<:AbstractFloat} = T(_next(r)) / T(r.m);

julia> rand(r::LCG10) = rand(r, Float64);

julia> function _next(r::LCG10)
           (; a, s, m) = r
           s = a * s % m
           r.s = s
           s
       end;

julia> lcg10 = LCG10(UInt(427419669081), UInt(0x1), UInt(999999999989));

julia> mc(lcg10, g, N)

0.0003320382805466088

julia> mc(lcg10, g², N)
0.49994037168781214

julia> sum(g(Float32(rand(lcg10))) for _ = 1:N) / N
0.000762443525496333

julia> sum(g²(Float32(rand(lcg10))) for _ = 1:N) / N
0.4999173715876127

julia> sum(g(Float16(rand(lcg10))) for _ = 1:N) / N
-0.003556871162906289

julia> sum(g²(Float16(rand(lcg10))) for _ = 1:N) / N # Well, barbaric rounding gets us somewhere...
0.7494309231800658

julia> setprecision(BigFloat, 10, base=10)
10

julia> sum(g(BigFloat(rand(lcg10))) for _ = 1:N) / N # To be fair, not everything is same precision
0.00255783884813

julia> sum(g²(BigFloat(rand(lcg10))) for _ = 1:N) / N
0.498719404946

julia> g(t, ::Type{T}) where {T<:AbstractFloat} = cos(2 * T(π) * 10^10 * t);

julia> g²(t, ::Type{T}) where {T<:AbstractFloat} = abs2(g(t, T));

julia> sum(g(rand(lcg10, BigFloat), BigFloat) for _ = 1:N) / N  # well, puzzling
-0.00287454071531

julia> sum(g²(rand(lcg10, BigFloat), BigFloat) for _ = 1:N) / N
0.501611169253
```
The above is just a superficial reproduction of the math, but not necessarily the exact arithmetic Maple 10 would have used. Could this explain my inability to reproduce? Noting that I have never used Maple, but have used MATLAB extensively, I have a hunch that Maple 10's decimal floating point was doing some magic behind the scenes (to force certain mathematical identities, which are true of real numbers, but not floats). One would have a reasonable expectation that BigFloat's (which obeys IEEE rounding) with precision set to 10 decimal digits should be able to elicit similar behavior. I believe I am in [good company](https://people.eecs.berkeley.edu/~wkahan/Mindless.pdf) in being suspicious of (decimal) floating point which does not explicitly conform to IEEE rounding. Note that I am not trying to dismiss my inability to reproduce Morgenstern's results, but without a Maple 10 from 2006, it's probably hopeless. 
On the subject of results in Section 1, they are partly dependent on the nature of LCG's.

##

Another long post--

# On usage and flexibility
The above is rigid: n-bit rationals stored in n-bit integers, which, to perform subsequent arithmetic, necessitates a widening of the storage (e.g. `Rational{UInt64}` to `Rational{UInt128`) -- that is, an n-bit rational stored in an n-bit integer is on the precipice of overflow in most circumstances.
However, the above contains a kernel of a flexible idea: m-bit rationals stored in n-bit integers, where m ≤ n -- it reduces to the above at m=n. Such an approach enables one to select any discretization of the unit interval one desires.
A nice toy example can be n=8, m=3.
It does not require much modification to the methods -- I am not necessarily proposing that it be included as functionality in base, as it is a little awkward as phrased below:
```julia
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
```
An example (without hooking into `rand` interface)
```julia
julia> [randrational(Rational{UInt8}, 3) for _ = 1:10]
10-element Vector{Rational{UInt8}}:
 0x02//0x07
 0x06//0x07
 0x00//0x01
 0x00//0x01
 0x06//0x07
 0x00//0x01
 0x03//0x07
 0x03//0x07
 0x02//0x07
 0x00//0x01
```

The alternative, which is much nicer to use (largely due to hooking into `rand` interface), is:
```julia
abstract type RationalInterval{T<:Integer} end
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
```
This yields an expressive means of sampling any combination of `n` and `m`.
```julia
julia> xo = Xoshiro(1234);

julia> cc_64_63 = CloseClose01_m{UInt64}(0x3f)
CloseClose01_m{UInt64, UInt8}(0x3f)

julia> rand(xo, cc_63_64)
0x00d9fdf04da6f0d8//0x029cbc14e5e0a72f

julia> ccu_8_3 = CloseClose01_m{UInt8}(0x03)
CloseClose01_m{UInt8, UInt8}(0x03)

julia> rand(ccu_8_3, 5, 2)
5×2 Matrix{Rational{UInt8}}:
 0x00//0x01  0x05//0x07
 0x04//0x07  0x01//0x07
 0x05//0x07  0x05//0x07
 0x02//0x07  0x02//0x07
 0x01//0x01  0x05//0x07

julia> cci_8_3 = CloseClose01_m{Int8}(3)
CloseClose01_m{Int8, Int64}(3)

julia> rand(cci_8_3, 5, 2)
5×2 Matrix{Rational{Int8}}:
 1//7  6//7
 1//7  4//7
 5//7  1//1
 1//7  6//7
 2//7  1//7
```



# On Morgenstern-2006
```julia
#### Morgenstern 2006 -- Section 1
# Some attempts with horrible seeds
julia> N = 10^6;

julia> g(t) = cos(2 * π * 10^10 * t);    # ∫₀¹ g(t)dt = 0

julia> g²(t) = abs2(g(t));               # ∫₀¹ g²(t)dt = 0.5

julia> mc(r, f, N) = sum(f(rand(r)) for _ = 1:N) / N;

julia> xo = Xoshiro(0x1);

julia> mc(xo, g, N)

-0.0002710728024089284

julia> mc(xo, g², N)
0.5002416981725267

julia> mt = MersenneTwister(0x1);

julia> mc(mt, g, N)
0.0007549637067530266

julia> mc(mt, g², N)
0.5002242685930319

julia> mutable struct LCG10{A<:Integer, S<:Integer, M<:Integer}
           a::A
           s::S
           m::M
       end;

julia> rand(r::LCG10, ::Type{Float64}) = _next(r) / r.m;

julia> rand(r::LCG10, ::Type{T}) where {T<:AbstractFloat} = T(_next(r)) / T(r.m);

julia> rand(r::LCG10) = rand(r, Float64);

julia> function _next(r::LCG10)
           (; a, s, m) = r
           s = a * s % m
           r.s = s
           s
       end;

julia> lcg10 = LCG10(UInt(427419669081), UInt(0x1), UInt(999999999989));

julia> mc(lcg10, g, N)

0.0003320382805466088

julia> mc(lcg10, g², N)
0.49994037168781214

julia> sum(g(Float32(rand(lcg10))) for _ = 1:N) / N
0.000762443525496333

julia> sum(g²(Float32(rand(lcg10))) for _ = 1:N) / N
0.4999173715876127

julia> sum(g(Float16(rand(lcg10))) for _ = 1:N) / N
-0.003556871162906289

julia> sum(g²(Float16(rand(lcg10))) for _ = 1:N) / N # Well, barbaric rounding gets us somewhere...
0.7494309231800658

julia> setprecision(BigFloat, 10, base=10)
10

julia> sum(g(BigFloat(rand(lcg10))) for _ = 1:N) / N # To be fair, not everything is same precision
0.00255783884813

julia> sum(g²(BigFloat(rand(lcg10))) for _ = 1:N) / N
0.498719404946

julia> g(t, ::Type{T}) where {T<:AbstractFloat} = cos(2 * T(π) * 10^10 * t);

julia> g²(t, ::Type{T}) where {T<:AbstractFloat} = abs2(g(t, T));

julia> sum(g(rand(lcg10, BigFloat), BigFloat) for _ = 1:N) / N  # well, puzzling
-0.00287454071531

julia> sum(g²(rand(lcg10, BigFloat), BigFloat) for _ = 1:N) / N
0.501611169253
```
The above is just a superficial reproduction of the math, but not necessarily the exact arithmetic Maple 10 would have used. Could this explain my inability to reproduce? Noting that I have never used Maple, but have used MATLAB extensively, I have a hunch that Maple 10's decimal floating point was doing some magic behind the scenes (to force certain mathematical identities, which are true of real numbers, but not floats). One would have a reasonable expectation that BigFloat's (which obeys IEEE rounding) with precision set to 10 decimal digits should be able to elicit similar behavior. I believe I am in [good company](https://people.eecs.berkeley.edu/~wkahan/Mindless.pdf) in being suspicious of (decimal) floating point which does not explicitly conform to IEEE rounding. Note that I am not trying to dismiss my inability to reproduce Morgenstern's results, but without a Maple 10 from 2006, it's probably hopeless. 

On the subject of results in Section 1, they are partly dependent on the nature of LCG's. If we attempt to work in rationals as far as possible, carrying the relatively few denominators offered by Rational{Int} until the point at which we must use a float (cos), we do not observe the problem which seems to emanate from the LCG. (Though, this assumes that I could replicate the floating point behavior, which I can't).
```julia
julia> const πᵣ = rationalize(Int, π, tol=2^-62)
6167950454//1963319607

julia> 𝑔(t) = cos(2 * πᵣ * (10^10 * t));

julia> 𝑔²(t) = abs2(𝑔(t));

julia> sum(𝑔(Rational{BigInt}(rand(Rational{Int}))) for _ = 1:N) / N

-0.0009990287992112217632625414290644167914619440478086315875375977433841484769463525

julia> sum(𝑔²(Rational{BigInt}(rand(Rational{Int}))) for _ = 1:N) / N
0.5003065979441251943945511738233627901854680635895315259578590913865011799718236

julia> sum(g(rand(Rational{Int})) for _ = 1:N) / N

-0.0004671059707741984

julia> sum(g²(rand(Rational{Int})) for _ = 1:N) / N
0.5002582875480501
```

# On #25993 and "life outside the unit interval"
Said thread contains some interesting questions. Whether a uniform distribution can be defined for rationals, rather than just for computer representations of rationals is a thorny question. It is clear that for representations involving n-bit integers with finite n, on the unit interval, a uniform distribution can always be defined. Let us suppose that we have draws from the unit interval, given some n. If we wished instead to have samples from the rationals on [a, b]

Thoughts, @mschauer, @simonbyrne ? 
