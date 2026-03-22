#pragma once

#include <Rcpp.h>
#include <Rinternals.h>

#include <algorithm>
#include <limits>
#include <type_traits>

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

// reversible sequence helpers ------------------------------------------------------

// These helpers allow us to write the core grid swarm placement methods in a way that
// is agnostic to whether we are iterating forward or backward through the candidate dots.

/// const begin iterator for forward or reverse iteration
/// @tparam reverse iterate in reverse?
/// @tparam C container type
/// @param container object to iterate over
template<bool reverse, typename C>
inline auto cbegin_(const C& container) {
  if constexpr (reverse) {
    return container.crbegin();
  } else {
    return container.cbegin();
  }
}

/// const end iterator for forward or reverse iteration
/// @tparam reverse iterate in reverse?
/// @tparam C container type
/// @param container object to iterate over
template<bool reverse, typename C>
inline auto cend_(const C& container) {
  if constexpr (reverse) {
    return container.crend();
  } else {
    return container.cend();
  }
}

/// Erase an element from a container via a (possibly reversed) iterator
/// @tparam reverse is the iterator reversed?
/// @tparam C container type
/// @tparam It iterator type
/// @param container object to erase from
/// @param it iterator pointing at element to erase
template<bool reverse, typename C, typename It>
inline auto erase_(C& container, const It& it) -> It {
  if constexpr (reverse) {
    return std::reverse_iterator(container.erase(std::next(it).base()));
  } else {
    return container.erase(it);
  }
}

/// Optionally negate a value
/// @tparam negate negate the value?
/// @param value value to negate (if `reverse` is `true`)
/// @returns `value` or `-value`
template<bool negate>
inline auto negate_if(const double value) -> double {
  if constexpr (negate) {
    return -value;
  } else {
    return value;
  }
}

/// Reversible minimum
/// @tparam reverse take maximum instead of minimum?
/// @param a first value
/// @param b second value
/// @returns minimum (or maximum) of `a` and `b`
template<bool reverse>
inline auto min_(const double a, const double b) -> double {
  if constexpr (reverse) {
    return std::max(a, b);
  } else {
    return std::min(a, b);
  }
}

/// Advance an iterator on a container to at least `min_value`
/// @tparam C container type
/// @tparam It iterator type (may be const and/or reverse)
/// @param container sorted container
/// @param it current iterator position
/// @param min_value minimum value to advance to
template<typename C, typename It>
inline auto advance_to_at_least(
  C& container, It it, const typename C::value_type min_value
) -> It {
  if constexpr (std::is_base_of_v<typename C::reverse_iterator, It>) {
    return std::reverse_iterator(std::upper_bound(container.begin(), it.base(), min_value));
  } else if constexpr (std::is_base_of_v<typename C::const_reverse_iterator, It>) {
    return std::reverse_iterator(std::upper_bound(container.cbegin(), it.base(), min_value));
  } else if constexpr (std::is_base_of_v<typename C::iterator, It>) {
    return std::lower_bound(it, container.end(), min_value);
  } else if constexpr (std::is_base_of_v<typename C::const_iterator, It>) {
    return std::lower_bound(it, container.cend(), min_value);
  } else {
    static_assert(false, "`it` must be an iterator for `container`");
  }
}
