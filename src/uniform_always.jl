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

# Related notes

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
