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
//' @param candidate <scalar [numeric]> candidate x position
//' @param last_placed <scalar [numeric]> last placed x position in this row
//' @param rows <[list] of [numeric]> list of previous rows of placed dots
//' @param n_rows_back <scalar [integer]> actual number of previous rows to consider
//' @param ygrid <scalar [integer]> max possible number of previous rows in the
//' y grid that  could overlap with this candidate
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param reverse <scalar [logical]> are we placing dots in reverse order?
//' @returns <scalar [logical]> can we place candidate here?
//' @noRd
template<bool reverse>
inline auto can_place_candidate(
  const double candidate,
  const double last_placed,
  std::vector<std::deque<double>>& rows,
  const std::size_t n_rows_back,
  const std::size_t ygrid,
  const double xsize
) -> bool {
  const auto eps = 8 * EPS * xsize;

  if constexpr (reverse) {
    if (candidate > last_placed - xsize + eps) return false;
  } else {
    if (candidate < last_placed + xsize - eps) return false;
  }

  // for the n_rows_back previous rows, check if candidate is overlapping an existing dot
  const auto n_rows = rows.size();
  for (auto i = 1_z; i <= n_rows_back; ++i) {
    // rows[n_rows - i] is the current row being placed, so previous rows start at n_rows - i - 1
    auto& prev_row = rows[n_rows - i - 1_z];
    const auto n = prev_row.size();
    if (n == 0) continue;

    const auto y_offset = double(i) / double(ygrid);
    const auto min_x_dist = std::sqrt(1 - y_offset * y_offset) * (xsize - eps);

    if (candidate <= prev_row.front()) {
      if (candidate > prev_row.front() - min_x_dist) return false;
    } else if (candidate >= prev_row.back()) {
      if (candidate < prev_row.back() + min_x_dist) return false;
    } else {
      auto it = std::upper_bound(prev_row.begin(), prev_row.end(), candidate);
      const auto min_val_gt_candidate = *it;
      if (candidate > min_val_gt_candidate - min_x_dist) return false;
      const auto max_val_lte_candidate = *--it;
      if (candidate < max_val_lte_candidate + min_x_dist) return false;
    }
  }
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

//' Place dots in a single row in the grid_swarm algorithm
//' @param reverse are we placing dots in reverse order?
//' @param both is this a mirrored layout (`side == "both"`?)
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ygrid <scalar [integer]> max possible number of previous rows in the
//' y grid that  could overlap with this candidate
//' @param remaining vector of dots to be placed
//' @param next_remaining swap space to move next set of dots to be placed into
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
  std::deque<double>*& remaining,
  std::deque<double>*& next_remaining,
  std::vector<std::deque<double>>& rows,
  std::vector<std::deque<double>>& rows_bottom
) -> bool {
  if (remaining->empty()) return false;

  // must calculate n_rows_back here before adding a new row
  const auto n_rows_back = std::min(ygrid, rows.size());

  const auto row = &rows.emplace_back();
  const auto row_bottom = both ? &rows_bottom.emplace_back() : nullptr;

  auto last_placed = reverse ? INF : -INF;
  auto last_placed_bottom = last_placed;

  next_remaining->clear();

  for (auto it = cbegin<reverse>(*remaining); it != cend<reverse>(*remaining); ++it) {
    const auto candidate = *it;
    if (can_place_candidate<reverse>(candidate, last_placed, rows, n_rows_back, ygrid, xsize)) {
      push<reverse>(*row, candidate);
      last_placed = candidate;
    } else if (both && can_place_candidate<reverse>(candidate, last_placed_bottom, rows_bottom, n_rows_back, ygrid, xsize)) {
      push<reverse>(*row_bottom, candidate);
      last_placed_bottom = candidate;
    } else {
      push<reverse>(*next_remaining, candidate);
    }
  }

  std::swap(remaining, next_remaining);

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
  std::deque<double>*& remaining,
  std::deque<double>*& next_remaining,
  std::vector<std::deque<double>>& rows,
  std::vector<std::deque<double>>& rows_bottom
) -> bool {
  auto any_left = true;
  while (
    n-- > 0_z &&
    (any_left = place_row<reverse>(both, xsize, ygrid, remaining, next_remaining, rows, rows_bottom)) &&
    n-- > 0_z &&
    (any_left = place_row<!reverse>(both, xsize, ygrid, remaining, next_remaining, rows, rows_bottom))
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
  std::deque<double> x,
  const double xsize,
  const double ysize,
  const std::size_t ygrid,
  const int side
) {
  const auto n_out = x.size();
  const auto both = side == 0;

  // as we place dots into rows, we will put unplaced dots into next_remaining
  // and then swap with remaining at the end of each row placement
  auto remaining_swap = std::deque<double>{};
  auto remaining = &x;
  auto next_remaining = &remaining_swap;

  auto rows = std::vector<std::deque<double>>{};
  auto rows_bottom = std::vector<std::deque<double>>{};

  // first row is special: when both == true, it is a "middle" row that is
  // treated as the first row (for placement purposes) on both the top and bottom sides
  // so we always treat it as both = false and just copy it to rows_bottom
  place_row<false>(false, xsize, ygrid, remaining, next_remaining, rows, rows_bottom);
  if (both) rows_bottom.push_back(rows.back());

  // place dots in rows, alternating direction (but also ensuring every ygrid-th row alternates)
  while (
    // start with <true>(ygrid - 1, ...) instead of <false>(ygrid, ...) because
    // we already placed the first row above
    place_rows<true>(ygrid - 1, both, xsize, ygrid, remaining, next_remaining, rows, rows_bottom) &&
    place_rows<true>(ygrid, both, xsize, ygrid, remaining, next_remaining, rows, rows_bottom) &&
    place_row<false>(both, xsize, ygrid, remaining, next_remaining, rows, rows_bottom)
  );

  // construct output data frame
  auto out_x_vec = Rcpp::NumericVector(n_out);
  auto out_y_vec = Rcpp::NumericVector(n_out);
  auto out_x_arr = REAL(out_x_vec);
  auto out_y_arr = REAL(out_y_vec);
  auto i = 0_z;
  const auto copy_rows_to_output = [&i, &out_x_arr, &out_y_arr, ygrid, ysize](
    const std::vector<std::deque<double>>& rows,
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
