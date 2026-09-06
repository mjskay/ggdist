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
  std::vector<std::deque<double>>& xs;
  const double xsize;
  const double ysize;
  const std::ptrdiff_t ygrid;
  const int side;
  const std::ptrdiff_t n;
  const bool both;

  using Row = std::multiset<double>;
  /// Rows of placed dots
  /// Contains the x position of each placed dot in each row.
  std::deque<Row> rows = {{}};

 public:
  GridSwarm(
    std::vector<std::deque<double>>& xs,
    const double xsize,
    const double ysize,
    const std::ptrdiff_t ygrid,
    const int side
  )
    : xs{xs},
      xsize{xsize},
      ysize{ysize},
      ygrid{ygrid},
      side{side},
      n{sum_sizes(xs)},
      both{side == 0}
    {};

 private:
  /// Attempt to place a candidate dot in a target row
  /// @param candidate candidate x position
  /// @param target_row_i index of row in `rows` to attempt to place `candidate` in
  /// @param min_next_candidate output parameter giving the minimum candidate x position
  /// that could be placed after attempting to place `candidate`
  /// @returns `true` if the candidate was placed successfully
  template<bool reverse>
  auto place_candidate(
    const double candidate,
    const std::ptrdiff_t target_row_i,
    double& min_next_candidate
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
      const auto min_x_dist = std::sqrt(1 - sq(y_offset)) * (xsize - eps);

      auto loc = row.upper_bound(candidate);
      if (loc != row.end()) {
        const auto min_val_gt_candidate = *loc;
        if (candidate > min_val_gt_candidate - min_x_dist) {
          // overlap => can't place candidate here
          min_next_candidate = min_val_gt_candidate + negate_if<reverse>(min_x_dist);
          return false;
        }
      }
      if (loc != row.begin()) {
        const auto max_val_lte_candidate = *std::prev(loc);
        if (candidate < max_val_lte_candidate + min_x_dist) {
          // overlap => can't place candidate here
          min_next_candidate = max_val_lte_candidate + negate_if<reverse>(min_x_dist);
          return false;
        }
      }

      // if this is the target row we save insert_loc so we can give a hint to
      // speed up the call to target_row.insert() below
      if (rows_from_target == 0) insert_loc = loc;
    }

    target_row.insert(insert_loc, candidate);
    min_next_candidate = candidate + negate_if<reverse>(xsize - eps);
    return true;
  }

  /// Attempt to place dots in a specific row in the grid_swarm algorithm
  /// @tparam reverse are we placing dots in reverse order?
  /// @param candidates dots to be placed
  /// @param row_num row number to place dots in.
  /// - When `both == false`, this is the index of the row in `rows`
  /// - When `both == true`, this is the distance from the center row.
  /// @returns `true` if `candidates` may still have dots to place and `false` otherwise
  template<bool reverse, typename Candidates>
  auto place_row(
    Candidates& candidates,
    std::ptrdiff_t& row_num
  ) -> bool {
    if (candidates.empty()) return false;

    // determine row indices and ensure target row exists
    auto row_i = row_num;
    auto row_i_bottom = row_num;
    if (both) {
      auto row_origin = ssize_(rows) / 2_z;
      if (row_num == row_origin + 1_z) {
        rows.emplace_back();
        rows.emplace_front();
        ++row_origin;
      }
      row_i = row_origin + row_num;
      row_i_bottom = row_origin - row_num;
    } else if (row_num == ssize_(rows)) {
      rows.emplace_back();
    }

    // place candidates
    const bool place_both = both && row_num > 0; // center row only placed once
    auto min_next_candidate = reverse ? INF : -INF;
    auto min_next_candidate_top = min_next_candidate;
    auto min_next_candidate_bottom = min_next_candidate;
    for (auto it = begin_<reverse>(candidates); it != end_<reverse>(candidates); ) {
      auto candidate = *it;

      // attempt to place candidate, updating min_next_candidate_{top,bottom} so we can
      // skip candidates that are definitely not placeable (this is very important for performance,
      // especially when binwidth is large and many candidates are rejected)
      if (
        place_candidate<reverse>(candidate, row_i, min_next_candidate_top) ||
        (place_both && place_candidate<reverse>(candidate, row_i_bottom, min_next_candidate_bottom))
      ) {
        it = erase_(candidates, it);
      } else {
        ++it;
      }

      // skip candidates that are definitely not placeable
      if (place_both) {
        min_next_candidate = min_<reverse>(min_next_candidate_top, min_next_candidate_bottom);
      } else {
        min_next_candidate = min_next_candidate_top;
      }
      it = advance_to_at_least(candidates, it, min_next_candidate);
    }

    ++row_num;
    return true;
  }

  /// Place dots in `n` rows in the grid_swarm algorithm, alternating directions.
  /// @returns `true` if `remaining` may still have dots to place and `false` otherwise
  /// @see `place_row()`
  template<bool reverse, typename Candidates>
  auto place_rows(
    std::size_t n_rows,
    Candidates& candidates,
    std::ptrdiff_t& row_num
  ) -> bool {
    auto any_left = true;
    while (
      n_rows-- > 0_uz &&
      (any_left = place_row<reverse>(candidates, row_num)) &&
      n_rows-- > 0_uz &&
      (any_left = place_row<!reverse>(candidates, row_num))
    );
    return any_left;
  }

 public:
  /// Run the grid swarm algorithm.
  auto place_dots() -> SEXP {
    for (auto& candidates : xs) {
      auto row_num = 0_z;
      // place dots in rows, alternating direction (but also ensuring every ygrid-th row alternates)
      while (
        place_rows<false>(ygrid, candidates, row_num) &&
        place_rows<true>(ygrid, candidates, row_num)
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
//' @param xs <list of [numeric]> list of vectors of sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param ygrid <scalar [numeric]> size of the y grid (corresponding to 1 + the number of adjacent
//' rows above or below this row that could overlap with dots in this row).
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP grid_swarm_(
  std::vector<std::deque<double>> xs,
  const double xsize,
  const double ysize,
  const std::ptrdiff_t ygrid,
  const int side
) {
  return GridSwarm{xs, xsize, ysize, ygrid, side}.place_dots();
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
  Rcpp::NumericVector& x_vec,
  Rcpp::NumericVector& y_vec,
  const double binwidth
) {
  auto n = ssize_(x_vec);
  auto x = REAL(x_vec);
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
