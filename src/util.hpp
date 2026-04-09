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

// math ---------------------------------------------------------------------------

/// square a value
template<typename V>
constexpr auto sq(const V x) -> V {
  return x * x;
}

// container helpers ---------------------------------------------------------------------------

/// Signed size of a container (from C++20)
template<typename C>
constexpr auto ssize_(const C& c) -> std::common_type_t<std::ptrdiff_t, std::make_signed_t<decltype(c.size())>> {
    using signed_c_size_t = std::common_type_t<std::ptrdiff_t, std::make_signed_t<decltype(c.size())>>;
    return static_cast<signed_c_size_t>(c.size());
}

/// Signed sum of sizes of a container of containers
template<typename C>
constexpr auto sum_sizes(const C& container) -> std::ptrdiff_t {
  std::ptrdiff_t n = 0_z;
  for (const auto& x : container) n += x.size();
  return n;
}

// reversible sequence helpers ------------------------------------------------------

// These helpers allow us to write the core grid swarm placement methods in a way that
// is agnostic to whether we are iterating forward or backward through the candidate dots.

/// const begin iterator for forward or reverse iteration
/// @tparam reverse iterate in reverse?
/// @tparam C container type
/// @param container object to iterate over
template<bool reverse, typename C>
constexpr auto begin_(C& container) {
  if constexpr (reverse && std::is_const_v<C>) {
    return container.crbegin();
  } else if constexpr (reverse) {
    return container.rbegin();
  } else if constexpr (std::is_const_v<C>) {
    return container.cbegin();
  } else {
    return container.begin();
  }
}

/// const end iterator for forward or reverse iteration
/// @tparam reverse iterate in reverse?
/// @tparam C container type
/// @param container object to iterate over
template<bool reverse, typename C>
constexpr auto end_(C& container) {
  if constexpr (reverse && std::is_const_v<C>) {
    return container.crend();
  } else if constexpr (reverse) {
    return container.rend();
  } else if constexpr (std::is_const_v<C>) {
    return container.cend();
  } else {
    return container.end();
  }
}

/// Erase an element from a container via a (possibly reversed) iterator
/// @tparam reverse is the iterator reversed?
/// @tparam C container type
/// @tparam It iterator type
/// @param container object to erase from
/// @param it iterator pointing at element to erase
template<typename C, typename It>
constexpr auto erase_(C& container, const It& it) -> It {
  if constexpr (std::is_base_of_v<typename C::reverse_iterator, It> || std::is_base_of_v<typename C::const_reverse_iterator, It>) {
    return std::reverse_iterator(container.erase(std::next(it).base()));
  } else {
    return container.erase(it);
  }
}

/// Optionally negate a value
/// @tparam negate negate the value?
/// @param value value to negate (if `reverse` is `true`)
/// @returns `value` or `-value`
template<bool negate, typename V>
constexpr auto negate_if(const V value) -> V {
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
constexpr auto min_(const double a, const double b) -> double {
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
template<typename C, typename It, typename V = typename C::value_type, typename Comp = std::less<std::remove_reference_t<std::remove_cv_t<V>>>>
inline auto advance_to_at_least(
  C& container, It it, V&& min_value, Comp&& comp = {}
) -> It {
  if constexpr (std::is_base_of_v<typename C::reverse_iterator, It>) {
    return std::reverse_iterator(std::upper_bound(container.begin(), it.base(), std::forward<V>(min_value), std::forward<Comp>(comp)));
  } else if constexpr (std::is_base_of_v<typename C::const_reverse_iterator, It>) {
    return std::reverse_iterator(std::upper_bound(container.cbegin(), it.base(), std::forward<V>(min_value), std::forward<Comp>(comp)));
  } else if constexpr (std::is_base_of_v<typename C::iterator, It>) {
    return std::lower_bound(it, container.end(), std::forward<V>(min_value), std::forward<Comp>(comp));
  } else if constexpr (std::is_base_of_v<typename C::const_iterator, It>) {
    return std::lower_bound(it, container.cend(), std::forward<V>(min_value), std::forward<Comp>(comp));
  } else {
    static_assert(false, "`it` must be an iterator for `container`");
  }
}

// search ------------------------------------------------------

/// Find minimum of unimodal f(*x) for x in the sequence [lo, hi).
/// Uses Fibonnaci search to avoid re-evaluating f() on already-checked values
/// when possible.
/// @param lo iterator to lower limit of interval on a container of sorted values.
/// @param hi iterator to upper limit of interval on a container of sorted values.
/// @param f function to be minimized, taking values from the container `lo` and `hi`
/// @returns a pair containing:
///  - An iterator pointing to the location of the minimum value of `f` in the interval
///  - The corresponding minimum value of `f`
template<typename It, typename F>
inline auto unimodal_min(It lo, It hi, const F f) -> std::pair<It, decltype(f(*lo))> {
  const auto n = hi - lo;
  if (n <= 1_z) return {lo, f(*lo)};
  if (*lo == *(hi - 1_z)) return {lo, f(*lo)};

  // Find the smallest Fibonacci number <= n -> fib_k
  // We cache the Fibonacci numbers to save recalculating on repeated calls
  static auto fib = std::vector<decltype(hi - lo)>{1_z, 1_z};
  decltype(fib.cend()) fib_k;
  if (fib.back() < n) {
    do {
      fib.push_back(*(fib.cend() - 1_z) + *(fib.cend() - 2_z));
    } while (fib.back() < n);
    fib_k = fib.cend() - 1_z;
  } else {
    fib_k = std::lower_bound(fib.cbegin(), fib.cend(), n);
  }

  // initial probe points
  auto m1 = lo + *(fib_k - 2_z) * (hi - lo) / *fib_k;
  auto f1 = f(*m1);

  auto m2 = lo + *(fib_k - 1_z) * (hi - lo) / *fib_k;
  auto f2 = f(*m2);

  const auto fib_3 = fib.cbegin() + 2_z;
  while (fib_k > fib_3 && m1 < m2) {
    if (f1 < f2) {
      // minimum is in [lo, m2):
      //    [lo,     m1, m2, hi)
      //     vv      vv  vv
      // -> [lo, m1, m2, hi)
      //         ^^
      //         new
      hi = m2;

      // reuse m1 as m2
      m2 = m1;
      f2 = f1;

      // compute new m1
      --fib_k;
      m1 = std::min(m2, lo + *(fib_k - 2_z) * (hi - lo) / *fib_k);
      if (m1 != m2) f1 = f(*m1);
    } else {
      // minimum is in [m1, hi):
      //    [lo, m1, m2,     hi)
      //         vv  vv      vv
      // ->     [lo, m1, m2, hi)
      //                 ^^
      //                 new
      lo = m1;

      // reuse m2 as m1
      m1 = m2;
      f1 = f2;

      // compute new m2
      --fib_k;
      m2 = std::max(m1, lo + *(fib_k - 1_z) * (hi - lo) / *fib_k);
      if (m1 != m2) f2 = f(*m2);
    }

    // compensate for collisions caused by integer division when
    // n is not exactly a Fibonacci number
    if (m1 == m2) {
      if (hi - m2 > 1_z) {
        ++m2;
        f2 = f(*m2);
      } else if (m1 - lo > 1_z) {
        --m1;
        f1 = f(*m1);
      }
    }
  }

  // We should always converge to something like:
  //             [   lo,    m1,    m2,    hi)
  //     == lo + [    0,     1,     1,     2)
  // or:
  //             [   lo,    m1,    m2,    hi)
  //     == lo + [    0,     1,     2,     3)
  // This way we know that the minimum is in either lo, m1, or
  // m2, and that f(*m1) and f(*m2) have already been evaluated
  assert(m1 - lo <= 1_z && m2 - m1 <= 1_z && hi - m2 <= 1_z);

  auto best = lo;
  auto f_best = f(*best);
  if (f1 < f_best) {
    best = m1;
    f_best = f1;
  }
  if (f2 < f_best) {
    best = m2;
    f_best = f2;
  }

  return {best, f_best};
}
