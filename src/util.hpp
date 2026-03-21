#pragma once

#include <Rcpp.h>
#include <Rinternals.h>

#include <algorithm>
#include <limits>
#include <set>

// constants --------------------------------------------------------------------------

constexpr auto INF = std::numeric_limits<double>::infinity();

template<typename Numeric>
constexpr auto relative_eps(const Numeric x) -> Numeric {
  return 8 * x * std::numeric_limits<Numeric>::epsilon();
}

// literals ---------------------------------------------------------------------------

/// Size diff literal for C++ arrays / vectors
constexpr std::ptrdiff_t operator""_z(unsigned long long n) {
  return n;
}

/// Size literal for C++ arrays / vectors
constexpr std::size_t operator""_uz(unsigned long long n) {
  return n;
}

/// Size literal for R vectors
constexpr R_xlen_t operator""_rz(unsigned long long n) {
  return n;
}

// container helpers ---------------------------------------------------------------------------

/// Signed size of a container (from C++20)
template<class C>
constexpr auto ssize_(const C& c) -> std::common_type_t<std::ptrdiff_t, std::make_signed_t<decltype(c.size())>> {
    using signed_c_size_t = std::common_type_t<std::ptrdiff_t, std::make_signed_t<decltype(c.size())>>;
    return static_cast<signed_c_size_t>(c.size());
}

/// Upper bound for a value in a container
template<class C>
inline auto upper_bound_(C& container, const double value) {
  return std::upper_bound(container.begin(), container.end(), value);
}
template<>
inline auto upper_bound_<std::multiset<double>>(std::multiset<double>& container, const double value) {
  return container.upper_bound(value);
}

/// Lower bound for a value in a container
template<class C>
inline auto lower_bound_(C& container, const double value) {
  return std::lower_bound(container.begin(), container.end(), value);
}
template<>
inline auto lower_bound_<std::multiset<double>>(std::multiset<double>& container, const double value) {
  return container.lower_bound(value);
}
