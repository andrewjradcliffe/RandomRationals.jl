################################
# The problem of reducibility -- Petroni-2019
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
# ################
# # Investigation of suggested implementation which randomizes numerator, but not denominator
# r8s = rand(Rational{Int8}, 10^6);
# c8s = [count(==(i//127), r8s) for i = 0:127];
# p = histogram(r8s);
# savefig(p, joinpath(pwd(), "Int8_hist.pdf"))
# r16s = rand(Rational{Int16}, 10^8);
# count_rational(A::AbstractArray{Rational{T}}) where {T} = map(0:typemax(T)) do i
#     count(==(i//typemax(T)), A)
# end
# function tcount_rational(A::AbstractArray{Rational{T}}) where {T}
#     r = zero(T):typemax(T)
#     B = Vector{Int}(undef, length(r))
#     Threads.@threads for i ∈ r
#         B[begin + i] = count(==(i//typemax(T)), A)
#     end
#     B
# end
# c16s = tcount_rational(r16s);
# # c16s = [count(==(i//32767), r16s) for i = 0:32767];
# p = bar(c16s);
# savefig(p, joinpath(pwd(), "Int16_bar.pdf"))

# u8s = rand(Rational{UInt8}, 10^8);
# c8s = tcount_rational(u8s);
# n8s = numerator.(u8s);
# d8s = denominator.(u8s);
# nums = sort!(unique(n8s));
# dens = sort!(unique(d8s));
# nc8s = map(i -> count(==(i), n8s), nums);
# dc8s = map(i -> count(==(i), d8s), dens);
# p = bar(c8s);
# savefig(p, joinpath(pwd(), "UInt8_bar.pdf"))
# p = bar(c8s .- Int(10^8 / 256));
# vline!(p, dens)
# savefig(p, joinpath(pwd(), "UInt8_bar_delta.pdf"))
# p = bar(nc8s, xticks=nums);
# savefig(p, joinpath(pwd(), "UInt8_bar_nums.pdf"))
# p = bar(dc8s, xticks=(1:length(dc8s), dens));
# savefig(p, joinpath(pwd(), "UInt8_bar_dens.pdf"))

# uu8s = unique(u8s);
# ii = findfirst(x -> x.den == 0x05, u8s);
# u8s[ii] # 0x03//0x05
# fac = 255 ÷ 5
# UInt8(fac * 0x03)

# for T ∈ (Int8, Int16, Int32, Int64)
#     rs = rand(Rational{T}, 10^6)
#     p = histogram(rs);
#     savefig(p, joinpath(pwd(), "$(T)_hist.pdf"))
# end

# ####
# # Investigation of Petroni's method using random intervals
# u8s_p = [rand_petroni(Rational{UInt8}) for _ = 1:10^8];
# fu8s_p = float.(u8s_p);
# c8s_p = tcount_rational(u8s_p);
# n8s_p = numerator.(u8s_p);
# d8s_p = denominator.(u8s_p);
# nums_p = sort!(unique(n8s_p));
# dens_p = sort!(unique(d8s_p));
# nc8s_p = map(i -> count(==(i), n8s_p), nums_p);
# dc8s_p = map(i -> count(==(i), d8s_p), dens_p);
# p = bar(c8s_p);
# savefig(p, joinpath(pwd(), "UInt8_bar_petroni.pdf"))
# p = bar(c8s_p .- Int(10^8 / 256));
# vline!(p, dens_p)
# savefig(p, joinpath(pwd(), "UInt8_bar_delta_petroni.pdf"))
# p = histogram(u8s_p);
# savefig(p, joinpath(pwd(), "UInt8_hist_petroni.pdf"))
# uu8s_p = unique(u8s_p);
# savefig(p, joinpath(pwd(), "UInt8_hist_petroni.pdf"))
# p = histogram(fu8s_p);
# savefig(p, joinpath(pwd(), "Float64_UInt8_hist_petroni.pdf"))
# for l ∈ (10, 100, 1000)
#     p = histogram(fu8s_p[1:l]);
#     savefig(p, joinpath(pwd(), "Float64_UInt8_hist_petroni_$(l).pdf"))
# end
# p = histogram(n8s_p);
# savefig(p, joinpath(pwd(), "UInt8_hist_petroni_nums.pdf"))
# p = histogram(d8s_p);
# savefig(p, joinpath(pwd(), "UInt8_hist_petroni_dens.pdf"))


# u16s_p = [rand_petroni(Rational{UInt16}) for _ = 1:10^8];
# fu16s_p = float.(u16s_p);
# c16s_p = tcount_rational(u16s_p);
# n16s_p = numerator.(u16s_p);
# d16s_p = denominator.(u16s_p);
# dens_p = sort!(unique(d16s_p));
# dc16s_p = asyncmap(i -> fetch(Threads.@spawn count(==(i), d16s_p)), dens_p);
# p = bar(c16s_p);
# savefig(p, joinpath(pwd(), "UInt16_bar_petroni.pdf"))
# p = bar(c16s_p .- Int(10^16 / 256));
# vline!(p, dens_p)
# savefig(p, joinpath(pwd(), "UInt16_bar_delta_petroni.pdf"))
# p = histogram(u16s_p);
# savefig(p, joinpath(pwd(), "UInt16_hist_petroni.pdf"))
# p = histogram(fu16s_p);
# savefig(p, joinpath(pwd(), "Float64_UInt16_hist_petroni.pdf"))
# for l ∈ (10, 100, 1000)
#     p = histogram(fu16s_p[1:l]);
#     savefig(p, joinpath(pwd(), "Float64_UInt16_hist_petroni_$(l).pdf"))
# end
# p = histogram(n16s_p);
# savefig(p, joinpath(pwd(), "UInt16_hist_petroni_nums.pdf"))
# p = histogram(d16s_p);
# savefig(p, joinpath(pwd(), "UInt16_hist_petroni_dens.pdf"))

# ####
# for T ∈ (Int8, Int16, Int32, Int64) #(UInt32, UInt64)
#     us_p = [rand_petroni(Rational{T}) for _ = 1:10^8];
#     fus_p = float.(us_p);
#     # cs_p = tcount_rational(us_p);
#     ns_p = numerator.(us_p);
#     ds_p = denominator.(us_p);
#     # dens_p = sort!(unique(ds_p));
#     # dcs_p = asyncmap(i -> fetch(Threads.@spawn count(==(i), ds_p)), dens_p);
#     # p = bar(cs_p);
#     # savefig(p, joinpath(pwd(), "$(T)_bar_petroni.pdf"))
#     # p = bar(cs_p .- Int(10^16 / 256));
#     # vline!(p, dens_p)
#     # savefig(p, joinpath(pwd(), "$(T)_bar_delta_petroni.pdf"))
#     p = histogram(us_p);
#     savefig(p, joinpath(pwd(), "$(T)_hist_petroni.pdf"))
#     p = histogram(fus_p);
#     savefig(p, joinpath(pwd(), "Float64_$(T)_hist_petroni.pdf"))
#     for l ∈ (10, 100, 1000)
#         p = histogram(fus_p[1:l]);
#         savefig(p, joinpath(pwd(), "Float64_$(T)_hist_petroni_$(l).pdf"))
#     end
#     p = histogram(ns_p);
#     savefig(p, joinpath(pwd(), "$(T)_hist_petroni_nums.pdf"))
#     p = histogram(ds_p);
#     savefig(p, joinpath(pwd(), "$(T)_hist_petroni_dens.pdf"))
# end

