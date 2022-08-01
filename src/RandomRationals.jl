module RandomRationals

using Random
import Random: Sampler, SamplerSimple, rand, default_rng, SamplerType, SamplerTrivial, gentype
import Base: unsafe_rational, divgcd, checked_mul

export RationalInterval

export CloseClose01_m, closeclose01_m
export CloseClose0d, acceptreject, denom
export rand_petroni, Petroni

abstract type RationalInterval{T<:Integer} end
Base.eltype(::Type{S}) where {S<:RationalInterval{T}} where {T<:Integer} = Rational{T}

include("uniform_always.jl")
include("uniform_m_bit_within_n_bit.jl")
include("shifting_and_scaling.jl")
include("petroni.jl")


end
