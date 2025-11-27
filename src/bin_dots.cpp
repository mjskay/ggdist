#include <Rcpp.h>
#include <Rinternals.h>

#include <algorithm>
#include <cfloat>
#include <cmath>
#include <limits>
#include <vector>

using namespace Rcpp;

// wilkinson-esque methods ------------------------------------------------------------

// [[Rcpp::export(rng = false)]]
IntegerVector wilkinson_bin_to_right_(const NumericVector& x, const double width) {
  using index_t = decltype(x.size());
  const auto n = x.size();

  auto bins = IntegerVector(n);
  auto current_bin = index_t{1};
  auto first_x = x[0];

  bins[0] = 1;
  for (auto i = index_t{1}; i < n; ++i) {
    // This is equivalent to x[i] - first_x >= width but it accounts for machine precision.
    // If we instead used `>=` directly some things that should be symmetric will not be
    if (x[i] - first_x - width >= -DBL_EPSILON) {
      current_bin = current_bin + 1;
      first_x = x[i];
    }
    bins[i] = current_bin;
  }

  return bins;
}

// weave_swarm ------------------------------------------------------------

//' Can we place `candidate` at this position given the last placed dot and
//' the previous rows of dots placed so far?
//' @param candidate <scalar [numeric]> candidate x position
//' @param last_placed <scalar [numeric]> last placed x position in this row
//' @param rows <[list] of [numeric]> list of previous rows of placed dots
//' @param n_rows_back <scalar [integer]> actual number of previous rows to consider
//' @param y_grid <scalar [integer]> max possible number of previous rows in the
//' y grid that  could overlap with this candidate
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param reverse <scalar [logical]> are we placing dots in reverse order?
//' @returns <scalar [logical]> can we place candidate here?
//' @noRd
template<bool reverse>
inline auto can_place_candidate(
  const double candidate,
  const double last_placed,
  std::vector<std::vector<double>>& rows,
  const int n_rows_back,
  const int y_grid,
  const double xsize
) -> bool {
  if constexpr (reverse) {
    if (candidate > last_placed - xsize) return false;
  } else {
    if (candidate < last_placed + xsize) return false;
  }

  // for the n_rows_back previous rows, check if candidate is overlapping an existing dot
  const auto n_rows = rows.size();
  for (int i = 1; i <= n_rows_back; i++) {
    // rows[n_rows - i] is the current row being placed, so previous rows start at n_rows - i - 1
    auto& prev_row_vec = rows[n_rows - i - 1];
    const auto n = prev_row_vec.size();
    if (n == 0) continue;

    const auto y_offset = double(i) / double(y_grid);
    const auto min_x_dist = std::sqrt(1 - y_offset * y_offset) * xsize;

    auto prev_row_arr = prev_row_vec.data();
    auto mflag = 0;  // -1 if < all, 0 if inside, +1 if >= all
    const auto max_val_lte_candidate_idx = findInterval(
      prev_row_arr,
      n,
      candidate,
      /*rightmost_closed=*/FALSE,
      /*all_inside=*/FALSE,
      /*ilo=*/0,
      &mflag
    );

    if (mflag >= 0) {
      const auto max_val_lte_candidate = prev_row_arr[max_val_lte_candidate_idx - 1];
      if (candidate < max_val_lte_candidate + min_x_dist) return false;
    }
    if (mflag <= 0) {
      const auto min_val_gt_candidate = prev_row_arr[max_val_lte_candidate_idx];
      if (candidate > min_val_gt_candidate - min_x_dist) return false;
    }
  }
  return true;
}

//' const begin iterator for forward or reverse iteration
//' @tparam reverse iterate in reverse?
//' @tparam T iterable type
//' @param vec object to iterate over
//' @noRd
template<bool reverse, typename T>
inline auto cbegin(const T& vec) {
  if constexpr (reverse) {
    return vec.crbegin();
  } else {
    return vec.cbegin();
  }
}

//' const end iterator for forward or reverse iteration
//' @tparam reverse iterate in reverse?
//' @tparam T iterable type
//' @param vec object to iterate over
//' @noRd
template<bool reverse, typename T>
inline auto cend(const T& vec) {
  if constexpr (reverse) {
    return vec.crend();
  } else {
    return vec.cend();
  }
}

//' Place dots in a single row in the weave_swarm algorithm
//' @tparam reverse are we placing dots in reverse order?
//' @param both is this a mirrored layout (`side == "both"`?)
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param y_grid <scalar [integer]> max possible number of previous rows in the
//' y grid that  could overlap with this candidate
//' @param remaining vector of dots to be placed
//' @param next_remaining swap space to move next set of dots to be placed into
//' @param rows <[list] of [numeric]> list of previous rows of placed dots
//' @param rows_bottom <[list] of [numeric]> list of previous bottom rows of placed dots
//' (when `both == true`)
//' @noRd
template<bool reverse>
inline void place_row(
  const bool both,
  const double xsize,
  const size_t y_grid,
  std::vector<double>*& remaining,
  std::vector<double>*& next_remaining,
  std::vector<std::vector<double>>& rows,
  std::vector<std::vector<double>>& rows_bottom
) {
  if (remaining->empty()) return;

  // must calculate n_rows_back here before adding a new row
  const auto n_rows_back = std::min(y_grid, rows.size());

  const auto row = &rows.emplace_back();
  const auto row_bottom = both ? &rows_bottom.emplace_back() : nullptr;

  auto last_placed = std::numeric_limits<double>::infinity() * (reverse ? 1.0 : -1.0);
  auto last_placed_bottom = last_placed;

  next_remaining->clear();

  for (auto it = cbegin<reverse>(*remaining); it != cend<reverse>(*remaining); ++it) {
    const auto candidate = *it;
    if (can_place_candidate<reverse>(candidate, last_placed, rows, n_rows_back, y_grid, xsize)) {
      row->push_back(candidate);
      last_placed = candidate;
    } else if (both && can_place_candidate<reverse>(candidate, last_placed_bottom, rows_bottom, n_rows_back, y_grid, xsize)) {
      row_bottom->push_back(candidate);
      last_placed_bottom = candidate;
    } else {
      next_remaining->push_back(candidate);
    }
  }

  if constexpr (reverse) {
    std::reverse(row->begin(), row->end());
    if (both) std::reverse(row_bottom->begin(), row_bottom->end());
    std::reverse(next_remaining->begin(), next_remaining->end());
  }
  std::swap(remaining, next_remaining);
}

//' Place dots `n` rows in the weave_swarm algorithm
//' See `place_row()`
//' @noRd
template<size_t n, bool reverse>
inline void place_rows(
  const bool both,
  const double xsize,
  const size_t y_grid,
  std::vector<double>*& remaining,
  std::vector<double>*& next_remaining,
  std::vector<std::vector<double>>& rows,
  std::vector<std::vector<double>>& rows_bottom
) {
  for (size_t i = 0; i < n; ++i) {
    place_row<reverse>(both, xsize, y_grid, remaining, next_remaining, rows, rows_bottom);
  }
}

//' Weave/swarm hybrid
//'
//' @param x <[numeric]> sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP weave_swarm_(
  std::vector<double> x,
  const double xsize,
  const double ysize,
  const int side
) {
  constexpr auto y_grid = size_t{4};
  const auto n_out = x.size();
  const auto both = side == 0;

  // as we place dots into rows, we will put unplaced dots into next_remaining
  // and then swap with remaining at the end of each row placement
  auto remaining_swap = std::vector<double>{};
  auto remaining = &x;
  auto next_remaining = &remaining_swap;

  auto rows = std::vector<std::vector<double>>{};
  auto rows_bottom = std::vector<std::vector<double>>{};

  // first row is special: when both == true, it is a "middle" row that is
  // treated as the first row (for placement purposes) on both the top and bottom sides
  // so we always treat it as both = false and just copy it to rows_bottom
  place_row<false>(false, xsize, y_grid, remaining, next_remaining, rows, rows_bottom);
  if (both) rows_bottom.push_back(rows.back());

  // place dots in rows, alternating direction every y_grid rows
  while (!remaining->empty()) {
    // start with y_grid - 1 because we already placed the first row above
    place_rows<y_grid - 1, false>(both, xsize, y_grid, remaining, next_remaining, rows, rows_bottom);
    place_rows<y_grid, true>(both, xsize, y_grid, remaining, next_remaining, rows, rows_bottom);
    place_row<false>(both, xsize, y_grid, remaining, next_remaining, rows, rows_bottom);
  }

  // construct output data frame
  auto out_x_vec = NumericVector(n_out);
  auto out_y_vec = NumericVector(n_out);
  auto out_x_arr = REAL(out_x_vec);
  auto out_y_arr = REAL(out_y_vec);
  auto i = std::ptrdiff_t{0};
  const auto copy_rows_to_output = [&i, &out_x_arr, &out_y_arr, y_grid, ysize](
    const std::vector<std::vector<double>>& rows,
    const std::ptrdiff_t row_start,
    const double side
  ) {
    for (size_t row_i = row_start; row_i < rows.size(); ++row_i) {
      const auto& row = rows[row_i];
      for (const auto& x_val : row) {
        out_x_arr[i] = x_val;
        out_y_arr[i] = double(row_i) / double(y_grid) * ysize * side;
        ++i;
      }
    }
  };
  copy_rows_to_output(rows, 0, both ? 1 : side);
  if (both) copy_rows_to_output(rows_bottom, 1, -1);

  return DataFrame::create(
    Named("x") = out_x_vec,
    Named("y") = out_y_vec
  );
}
