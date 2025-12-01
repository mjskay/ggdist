#include <Rcpp.h>
#include <Rinternals.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

// constants --------------------------------------------------------------------------

constexpr auto INF = std::numeric_limits<double>::infinity();
constexpr auto EPS = std::numeric_limits<double>::epsilon();

// literals ---------------------------------------------------------------------------

//' Size literal for C++ arrays / vectors
//' @noRd
constexpr std::size_t operator""_z(unsigned long long n) {
  return n;
}

//' Size literal for R vectors
//' @noRd
constexpr R_xlen_t operator""_rz(unsigned long long n) {
  return n;
}

// wilkinson-esque methods ------------------------------------------------------------

// [[Rcpp::export(rng = false)]]
Rcpp::IntegerVector wilkinson_bin_to_right_(const Rcpp::NumericVector& x, const double width) {
  const auto n = x.size();

  auto bins = Rcpp::IntegerVector(n);
  auto current_bin = 1_rz;
  auto first_x = x[0];

  bins[0] = 1;
  for (auto i = 1_rz; i < n; ++i) {
    // This is equivalent to x[i] - first_x >= width but it accounts for machine precision.
    // If we instead used `>=` directly some things that should be symmetric will not be
    if (x[i] - first_x - width >= -EPS) {
      current_bin = current_bin + 1_rz;
      first_x = x[i];
    }
    bins[i] = current_bin;
  }

  return bins;
}

// grid_swarm ------------------------------------------------------------

//' Can we place `candidate` at this position given the last placed dot and
//' the previous rows of dots placed so far?
//' @param reverse <scalar [logical]> are we placing dots in reverse order?
//' @param candidate <scalar [numeric]> candidate x position
//' @param rows <[list] of [numeric]> list of previous rows of placed dots
//' @param n_rows_back <scalar [integer]> actual number of previous rows to consider
//' @param ygrid <scalar [integer]> max possible number of previous rows in the
//' y grid that could overlap with this candidate
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @returns <scalar [logical]> can we place candidate here?
//' @noRd
template<bool reverse>
inline auto place_candidate(
  const double candidate,
  std::vector<std::multiset<double>>& rows,
  const std::size_t n_rows_back,
  const std::size_t ygrid,
  const double xsize
) -> bool {
  const auto eps = 8 * EPS * xsize;

  auto& current_row = rows.back();
  auto insert_loc = current_row.begin();

  // for the current row + n_rows_back previous rows, check if candidate is overlapping an existing dot
  const auto n_rows = rows.size();
  for (auto i = 0_z; i <= n_rows_back; ++i) {
    auto& row = rows[n_rows - i - 1_z];
    if (row.size() == 0) continue;

    const auto y_offset = double(i) / double(ygrid);
    const auto min_x_dist = std::sqrt(1 - y_offset * y_offset) * (xsize - eps);

    auto loc = row.upper_bound(candidate);
    if (loc != row.end()) {
      const auto min_val_gt_candidate = *loc;
      if (candidate > min_val_gt_candidate - min_x_dist) return false;
    }
    if (loc != row.begin()) {
      const auto max_val_lte_candidate = *--loc;
      if (candidate < max_val_lte_candidate + min_x_dist) return false;
    }
    if (i == 0_z) insert_loc = loc;
  }

  current_row.insert(insert_loc, candidate);
  return true;
}

//' const begin iterator for forward or reverse iteration
//' @param reverse iterate in reverse?
//' @param T iterable type
//' @param vec object to iterate over
//' @noRd
template<bool reverse, typename C>
inline auto cbegin(const C& container) {
  if constexpr (reverse) {
    return container.crbegin();
  } else {
    return container.cbegin();
  }
}

//' const end iterator for forward or reverse iteration
//' @param reverse iterate in reverse?
//' @param T iterable type
//' @param vec object to iterate over
//' @noRd
template<bool reverse, typename C>
inline auto cend(const C& container) {
  if constexpr (reverse) {
    return container.crend();
  } else {
    return container.cend();
  }
}

template<bool reverse, typename C, typename V>
inline void push(C& container, V&& value) {
  if constexpr (reverse) {
    container.push_front(std::forward<V>(value));
  } else {
    container.push_back(std::forward<V>(value));
  }
}

template<typename C>
constexpr auto all_empty(const C& container) -> bool {
  for (const auto& value : container) {
    if (!value.empty()) return false;
  }
  return true;
}



//' Place dots in a single row in the grid_swarm algorithm
//' @param reverse are we placing dots in reverse order?
//' @param both is this a mirrored layout (`side == "both"`?)
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ygrid <scalar [integer]> max possible number of previous rows in the
//' y grid that  could overlap with this candidate
//' @param remaining vector of dots to be placed
//' @param rows <[list] of [numeric]> list of previous rows of placed dots
//' @param rows_bottom <[list] of [numeric]> list of previous bottom rows of placed dots
//' (when `both == true`)
//' @returns `true` if `remaining` may still have dots to place and `false` otherwise
//' @noRd
template<bool reverse>
inline auto place_row(
  const bool both,
  const double xsize,
  const std::size_t ygrid,
  std::vector<std::deque<double>>& remaining,
  std::vector<std::multiset<double>>& rows,
  std::vector<std::multiset<double>>& rows_bottom
) -> bool {
  if (all_empty(remaining)) return false;

  // must calculate n_rows_back here before adding a new row
  const auto n_rows_back = std::min(ygrid, rows.size());

  rows.emplace_back();
  if (both) rows_bottom.emplace_back();

  std::deque<double> next_remaining;

  for (auto i = 0_z; i < remaining.size(); ++i) {
    next_remaining.clear();

    for (auto it = cbegin<reverse>(remaining[i]); it != cend<reverse>(remaining[i]); ++it) {
      const auto candidate = *it;

      if (place_candidate<reverse>(candidate, rows, n_rows_back, ygrid, xsize)) {
        continue;
      } else if (both && place_candidate<reverse>(candidate, rows_bottom, n_rows_back, ygrid, xsize)) {
        continue;
      }

      push<reverse>(next_remaining, candidate);
    }

    std::swap(remaining[i], next_remaining);
  }

  return true;
}

//' Place dots in `n` rows in the grid_swarm algorithm
//' See `place_row()`
//' @returns `true` if `remaining` may still have dots to place and `false` otherwise
//' @noRd
template<bool reverse>
inline auto place_rows(
  std::size_t n,
  const bool both,
  const double xsize,
  const std::size_t ygrid,
  std::vector<std::deque<double>>& remaining,
  std::vector<std::multiset<double>>& rows,
  std::vector<std::multiset<double>>& rows_bottom
) -> bool {
  auto any_left = true;
  while (
    n-- > 0_z &&
    (any_left = place_row<reverse>(both, xsize, ygrid, remaining, rows, rows_bottom)) &&
    n-- > 0_z &&
    (any_left = place_row<!reverse>(both, xsize, ygrid, remaining, rows, rows_bottom))
  );
  return any_left;
}

//' Fractional grid swarm layout
//' @param x <[numeric]> sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP grid_swarm_(
  std::vector<std::deque<double>> xs,
  const double xsize,
  const double ysize,
  const std::size_t ygrid,
  const int side
) {
  auto n_out = 0_z;
  for (const auto& x : xs) n_out += x.size();
  const auto both = side == 0;

  auto rows = std::vector<std::multiset<double>>{};
  auto rows_bottom = std::vector<std::multiset<double>>{};

  // first row is special: when both == true, it is a "middle" row that is
  // treated as the first row (for placement purposes) on both the top and bottom sides
  // so we always treat it as both = false and just copy it to rows_bottom
  place_row<false>(false, xsize, ygrid, xs, rows, rows_bottom);
  if (both) rows_bottom.push_back(rows.back());

  // place dots in rows, alternating direction (but also ensuring every ygrid-th row alternates)
  while (
    // start with <true>(ygrid - 1, ...) instead of <false>(ygrid, ...) because
    // we already placed the first row above
    place_rows<true>(ygrid - 1, both, xsize, ygrid, xs, rows, rows_bottom) &&
    place_rows<true>(ygrid, both, xsize, ygrid, xs, rows, rows_bottom) &&
    place_row<false>(both, xsize, ygrid, xs, rows, rows_bottom)
  );

  // construct output data frame
  auto out_x_vec = Rcpp::NumericVector(n_out);
  auto out_y_vec = Rcpp::NumericVector(n_out);
  auto out_x_arr = REAL(out_x_vec);
  auto out_y_arr = REAL(out_y_vec);
  auto i = 0_z;
  const auto copy_rows_to_output = [&i, &out_x_arr, &out_y_arr, ygrid, ysize](
    const std::vector<std::multiset<double>>& rows,
    const std::size_t row_start,
    const double side
  ) {
    for (auto row_i = row_start; row_i < rows.size(); ++row_i) {
      const auto& row = rows[row_i];
      for (const auto x_val : row) {
        out_x_arr[i] = x_val;
        out_y_arr[i] = double(row_i) / double(ygrid) * ysize * side;
        ++i;
      }
    }
  };
  copy_rows_to_output(rows, 0_z, both ? 1.0 : double(side));
  if (both) copy_rows_to_output(rows_bottom, 1_z, -1.0);

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = out_x_vec,
    Rcpp::Named("y") = out_y_vec
  );
}
