#include "util.hpp"

#include <Rcpp.h>
#include <Rinternals.h>

#include <algorithm>
#include <cmath>
#include <deque>
#include <iterator>
#include <set>
#include <vector>

// grid swarm layout --------------------------------------------------------------

/// Gridded variation of compact swarm layout
///
/// Alternative to compact swarm layout that places dots in alternating left/right sweeps along
/// grid lines, greedily placing the next-closest placeable dot (in x position) to the most
/// recently-placed dot in the same row.
class GridSwarm {
 private:
  // inputs and derived values
  const std::vector<Rcpp::NumericVector>& xs_list;
  const double xsize;
  const double ysize;
  const std::ptrdiff_t ygrid;
  const int side;
  const bool both;
  const std::ptrdiff_t n;

  /// Candidate (unplaced) dots we are currently placing
  std::deque<double> candidates = {};

  /// A single row of placed dots
  using Row = std::multiset<double>;
  /// Rows of dots
  /// Contains the x position of each placed dot in each row.
  std::deque<Row> rows = {{}};

  /// Current row.
  /// - When `both == false`, this is the index of the current row in `rows`.
  /// - When `both == true`, this is the distance from the origin (center) row.
  std::ptrdiff_t current_row = 0_z;

  /// Row origin
  /// Index of the origin row in `rows`
  /// - When `both == false`, this is 0.
  /// - When `both == true`, this is floor(rows.size() / 2).
  std::ptrdiff_t row_origin = 0_z;

 public:
  GridSwarm(
    const std::vector<Rcpp::NumericVector>& xs_list,
    const double xsize,
    const double ysize,
    const std::ptrdiff_t ygrid,
    const int side
  )
    : xs_list{xs_list},
      xsize{xsize},
      ysize{ysize},
      ygrid{ygrid},
      side{side},
      both{side == 0},
      n{sum_sizes(xs_list)}
    {};

 private:
  /// Attempt to place a dot in a target row
  /// @param x dot x position
  /// @param target_row_i index of row in `rows` to attempt to place `x` in
  /// @param min_next_x output parameter giving the minimum candidate x position
  /// that could be placed after attempting to place `x`
  /// @returns `true` if the candidate dot was placed successfully
  template<bool reverse>
  auto place_dot(
    const double x,
    const std::ptrdiff_t target_row_i,
    double& min_next_x
  ) -> bool {
    const auto eps = relative_eps(xsize);

    auto& target_row = rows[target_row_i];
    auto insert_loc = target_row.end();

    // check +/- (ygrid - 1) rows from target_row to see if the candidate is overlapping an existing dot
    const auto first_row_i = std::max(0_z, target_row_i - (ygrid - 1_z));
    const auto last_row_i = std::min(ssize_(rows), target_row_i + ygrid);
    // iterate in reverse because we will often have a quick exit by comparison to the
    // most recently placed dot
    for (auto i = last_row_i; i-- > first_row_i; ) {
      auto& row = rows[i];
      if (row.size() == 0_uz) continue;

      const auto rows_from_target = static_cast<double>(std::abs(i - target_row_i));
      const auto y_offset = rows_from_target / static_cast<double>(ygrid);
      const auto x_distance = std::sqrt(1 - sq(y_offset)) * (xsize - eps);

      auto x_loc_in_row = row.upper_bound(x);
      if (x_loc_in_row != row.end()) {
        const auto existing_dot_gt_x = *x_loc_in_row;
        if (x > existing_dot_gt_x - x_distance) {
          // overlap => can't place candidate here
          min_next_x = existing_dot_gt_x + negate_if<reverse>(x_distance);
          return false;
        }
      }
      if (x_loc_in_row != row.begin()) {
        const auto existing_dot_lte_x = *std::prev(x_loc_in_row);
        if (x < existing_dot_lte_x + x_distance) {
          // overlap => can't place candidate here
          min_next_x = existing_dot_lte_x + negate_if<reverse>(x_distance);
          return false;
        }
      }

      // if this is the target row we save insert_loc so we can give a hint to
      // speed up the call to target_row.insert() below
      if (rows_from_target == 0) insert_loc = x_loc_in_row;
    }

    target_row.insert(insert_loc, x);
    min_next_x = x + negate_if<reverse>(xsize - eps);
    return true;
  }

  /// Attempt to place dots in a specific row in the grid_swarm algorithm
  /// @tparam reverse are we placing dots in reverse order?
  /// @returns `true` if `candidates` may still have dots to place and `false` otherwise
  template<bool reverse>
  auto place_row() -> bool {
    if (candidates.empty()) return false;

    // determine row indices
    auto row_i_top = row_origin + current_row;
    auto row_i_btm = row_origin - current_row;

    // place candidates
    const bool place_both = both && current_row > 0; // center row only placed once
    auto min_next_x = reverse ? INF : -INF;
    auto min_next_x_top = min_next_x;
    auto min_next_x_btm = min_next_x;
    for (auto xi = begin_<reverse>(candidates); xi != end_<reverse>(candidates); ) {
      const auto x = *xi;

      // attempt to place candidate dot, updating min_next_x_{top,btm} so we can
      // skip candidates that are definitely not placeable (this is very important for performance,
      // especially when binwidth is large and many candidates are rejected)
      if (
        place_dot<reverse>(x, row_i_top, min_next_x_top) ||
        (place_both && place_dot<reverse>(x, row_i_btm, min_next_x_btm))
      ) {
        xi = erase_(candidates, xi);
      } else {
        ++xi;
      }

      // skip candidates that are definitely not placeable
      min_next_x = place_both ? min_<reverse>(min_next_x_top, min_next_x_btm) : min_next_x_top;
      xi = advance_to_at_least(candidates, xi, min_next_x);
    }

    // advance to next row (and ensure it exists)
    if (row_i_top + 1_z == ssize_(rows)) {
      rows.emplace_back();
      if (both) {
        rows.emplace_front();
        ++row_origin;
      }
    }
    ++current_row;

    return !candidates.empty();
  }

  /// Place dots in `n_rows` rows in the grid_swarm algorithm, alternating directions.
  /// @param n_rows Number of rows to place.
  /// @see `place_row()`
  template<bool reverse>
  auto place_rows(std::size_t n_rows) -> bool {
    while (
      n_rows-- > 0_uz &&
      place_row<reverse>() &&
      n_rows-- > 0_uz &&
      place_row<!reverse>()
    );
    return !candidates.empty();
  }

 public:
  /// Run the grid swarm algorithm.
  auto place_dots() -> SEXP {
    for (const auto& xs : xs_list) {
      current_row = 0_z;
      candidates.clear();

      // TODO: divide by xsize here so that all the distance calculations for checking
      // overlaps can be done in standardized units of 1 dot diameter, then we
      // multiply final positions by xsize and ysize before final output.
      for (const auto x : xs) candidates.emplace_back(x);

      // place dots in rows, alternating direction (but also ensuring every ygrid-th row alternates)
      while (
        place_rows<false>(ygrid) &&
        place_rows<true>(ygrid)
      ) {
        Rcpp::checkUserInterrupt();
      }
    }

    // construct output data frame
    auto out_x_vec = Rcpp::NumericVector(n);
    auto out_y_vec = Rcpp::NumericVector(n);
    auto out_x_arr = REAL(out_x_vec);
    auto out_y_arr = REAL(out_y_vec);
    if (both) {
      const auto row_height = ysize / static_cast<double>(ygrid);
      const auto row_origin = ssize_(rows) / 2_z;
      auto i = 0_z;
      for (auto row_num = 1_z; row_num <= ssize_(rows); ++row_num) {
        // row_offset is 0, 1, -1, 2, -2, ...
        const auto row_offset = (row_num / 2_z) * (1_z - (row_num % 2_z) * 2_z);
        const auto& row = rows[row_origin + row_offset];
        const auto y_val = static_cast<double>(row_offset) * row_height;
        for (const auto x_val : row) {
          out_x_arr[i] = x_val;
          out_y_arr[i] = y_val;
          ++i;
        }
      }
    } else {
      const auto row_height = static_cast<double>(side) * ysize / static_cast<double>(ygrid);
      auto i = 0_z;
      for (auto row_i = 0_z; row_i < ssize_(rows); ++row_i) {
        const auto& row = rows[row_i];
        const auto y_val = static_cast<double>(row_i) * row_height;
        for (const auto x_val : row) {
          out_x_arr[i] = x_val;
          out_y_arr[i] = y_val;
          ++i;
        }
      }
    }

    return Rcpp::DataFrame::create(
      Rcpp::Named("x") = out_x_vec,
      Rcpp::Named("y") = out_y_vec
    );
  }
};

//' Fractional grid swarm layout
//' @param xs_list <list of [numeric]> list of vectors of sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param ygrid <scalar [numeric]> size of the y grid (corresponding to 1 + the number of adjacent
//' rows above or below this row that could overlap with dots in this row).
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP grid_swarm_(
  const std::vector<Rcpp::NumericVector>& xs_list,
  const double xsize,
  const double ysize,
  const std::ptrdiff_t ygrid,
  const int side
) {
  return GridSwarm{xs_list, xsize, ysize, ygrid, side}.place_dots();
}


// swarm cluster recentering ------------------------------------------------------------

//' Re-center contiguous clusters around their mean y position so that
//' small clusters are visually centered (rather than e.g. a cluster of
//' two points having one point on the origin line and one above it)
//' @param x sorted numeric vector of dot positions
//' @param y sorted numeric vector of dot heights, same length as x
//' @returns modified `y`
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP recenter_swarm_clusters_(
  const Rcpp::NumericVector& x_vec,
  Rcpp::NumericVector& y_vec,
  const double binwidth
) {
  const auto n = ssize_(x_vec);
  const auto x = REAL(x_vec);
  auto y = REAL(y_vec);
  auto bin_sum = 0.0;
  auto bin_start = 0_z;
  for (auto bin_end = 1_z; bin_end <= n; ++bin_end) {
    bin_sum += y[bin_end - 1_z];
    if (bin_end == n || x[bin_end] - x[bin_end - 1_z] >= binwidth) {
      auto mean = bin_sum / static_cast<double>(bin_end - bin_start);
      for (auto i = bin_start; i < bin_end; ++i) y[i] -= mean;
      bin_start = bin_end;
      bin_sum = 0.0;
    }
  }
  return y_vec;
}
