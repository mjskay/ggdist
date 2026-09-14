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

/// Stratified variation of compact swarm layout
///
/// Alternative to compact swarm layout that places dots in alternating left/right sweeps along
/// grid lines, greedily placing the next-closest placeable dot (in x position) to the most
/// recently-placed dot in the same row.
///
/// The distance between grid lines (`row_height`) is `ysize/strata`, where `strata` is a positive
/// integer. During placement, already-placed values in each row are stored in a set
/// to allow efficient searching for collisions within a row.
class GridSwarm {
  // TYPES ---------------------------------------------------------------------------------------
  /// Direction of iteration when placing dots in a row.
  enum Direction {
    FWD = false,
    REV = true
  };
  constexpr friend Direction operator!(const Direction direction) {
    return static_cast<Direction>(!static_cast<bool>(direction));
  };
  /// A single row of placed dots
  using Row = std::set<double>;

  // CONSTRUCTORS --------------------------------------------------------------------------------
 public:
  /// Initialize the grid swarm algorithm.
  GridSwarm(
    const std::vector<Rcpp::NumericVector>& xs_list,
    const double xsize,
    const double ysize,
    const std::ptrdiff_t strata,
    const int signed_side
  )
    : xs_list{xs_list},
      xsize{xsize},
      ysize{ysize},
      strata{strata},
      signed_side{signed_side},
      both{signed_side == 0},
      n{sum_sizes(xs_list)},
      out_x_vec(n),
      out_y_vec(n),
      out_x_arr{REAL(out_x_vec)},
      out_y_arr{REAL(out_y_vec)},
      row_height{
        (both ? 1.0 : static_cast<double>(signed_side)) * ysize / static_cast<double>(strata)
      } {};

  // FIELDS --------------------------------------------------------------------------------------
 private:
  // inputs and derived values
  /// List of unnormalized x values for in each group.
  const std::vector<Rcpp::NumericVector>& xs_list;
  /// Size of dots in the x dimension.
  const double xsize;
  /// Size of dots in the y dimension.
  const double ysize;
  /// Number of rows in the grid in a distance of 1 `ysize`.
  const std::ptrdiff_t strata;
  //// Side we are placing on (-1 = bottom, 0 = both, 1 = top).
  const int signed_side;
  /// Are we placing dots on both sides?
  const bool both;
  /// Total number of dots
  const std::ptrdiff_t n;

  // outputs
  /// Unnormalized x values.
  Rcpp::NumericVector out_x_vec;
  /// Unnormalized y values.
  Rcpp::NumericVector out_y_vec;
  /// C array backing `out_x_vec`
  double* out_x_arr;
  /// C array backing `out_y_vec`
  double* out_y_arr;

  /// Index of the next-to-be-placed value in out_x_vec / out_y_vec
  std::ptrdiff_t i = 0_z;

  /// Row height
  /// Height of a single fractional row (ysize / strata) times the direction of plotting.
  /// Positive if `signed_side == 1` or `0` ("top" or "both"), negative if `signed_side == -1`
  /// ("bottom").
  const double row_height;

  /// Normalized x positions of unplaced dots in the group we are currently placing
  /// Input x values are divded by `xsize` prior to running the placement algorithm to simplify
  /// distance calculations.
  std::deque<double> unplaced = {};

  /// Rows of dots
  /// Contains the normalized x position of each placed dot in each row.
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

  // PRIVATE METHODS -----------------------------------------------------------------------------
 private:
  /// Attempt to place a dot in a target row
  /// @param x normalized x position of dot
  /// @param target_row_i index of row in `rows` to attempt to place `x` in
  /// @param min_next_x output parameter giving the next closest x position
  /// that a dot could be placed at in this row after `x` is placed
  /// @returns `true` if the dot was placed successfully
  template<Direction reverse>
  auto place_dot(
    const double x,
    const std::ptrdiff_t target_row_i,
    double& min_next_x
  ) -> bool {
    auto& target_row = rows[target_row_i];
    auto insert_loc = target_row.end();

    // check +/- (strata - 1) rows from target_row to see if the dot is overlapping an existing dot
    const auto first_row_i = std::max(0_z, target_row_i - (strata - 1_z));
    const auto last_row_i = std::min(ssize_(rows), target_row_i + strata);
    // iterate in reverse because higher-up placed dots should be closer to this one (which
    // may lead to a quick exit)
    for (auto i = last_row_i; i-- > first_row_i;) {
      auto& row = rows[i];
      if (row.empty()) continue;

      const auto rows_from_target = static_cast<double>(std::abs(i - target_row_i));
      const auto y_offset = rows_from_target / static_cast<double>(strata);
      const auto x_distance = std::sqrt(1 - sq(y_offset));

      auto x_loc_in_row = row.upper_bound(x);
      if (x_loc_in_row != row.end()) {
        const auto existing_dot_gt_x = *x_loc_in_row;
        if (x > existing_dot_gt_x - x_distance) {
          // overlap => can't place dot here
          min_next_x = existing_dot_gt_x + negate_if<reverse>(x_distance);
          return false;
        }
      }
      if (x_loc_in_row != row.begin()) {
        const auto existing_dot_lte_x = *std::prev(x_loc_in_row);
        if (x < existing_dot_lte_x + x_distance) {
          // overlap => can't place dot here
          min_next_x = existing_dot_lte_x + negate_if<reverse>(x_distance);
          return false;
        }
      }

      // if this is the target row we save insert_loc so we can give a hint to
      // speed up the call to target_row.insert() below
      if (rows_from_target == 0.0) insert_loc = x_loc_in_row;
    }

    // Place dot
    out_x_arr[i] = x * xsize;
    out_y_arr[i] = static_cast<double>(target_row_i - row_origin) * row_height;
    ++i;
    target_row.insert(insert_loc, x);
    min_next_x = x + negate_if<reverse>(1.0);

    return true;
  }

  /// Attempt to place dots in a specific row in the grid_swarm algorithm
  /// @tparam reverse are we placing dots in reverse order?
  /// @returns `true` if `unplaced` may still have dots to place and `false` otherwise
  template<Direction reverse>
  auto place_row() -> bool {
    if (unplaced.empty()) return false;

    // determine row indices
    auto row_i_top = row_origin + current_row;
    auto row_i_btm = row_origin - current_row;

    // place dots
    const bool place_both = both && current_row > 0;  // center row only placed once
    auto min_next_x = reverse ? INF : -INF;
    auto min_next_x_top = min_next_x;
    auto min_next_x_btm = min_next_x;
    for (auto xi = begin_<reverse>(unplaced); xi != end_<reverse>(unplaced);) {
      const auto x = *xi;

      // attempt to place the dot, updating min_next_x_{top,btm} so we can skip dots that are
      // definitely not placeable (this is very important for performance, especially when binwidth
      // is large and many candidate dots are rejected)
      if (
        place_dot<reverse>(x, row_i_top, min_next_x_top) ||
        (place_both && place_dot<reverse>(x, row_i_btm, min_next_x_btm))
      ) {
        xi = erase_(unplaced, xi);
      } else {
        ++xi;
      }

      // skip dots that are definitely not placeable in this row
      min_next_x = place_both ? min_<reverse>(min_next_x_top, min_next_x_btm) : min_next_x_top;
      xi = advance_to_at_least(unplaced, xi, min_next_x);
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

    return !unplaced.empty();
  }

  /// Place dots in `n_rows` rows in the grid_swarm algorithm, alternating directions.
  /// @param n_rows Number of rows to place.
  /// @see `place_row()`
  template<Direction reverse>
  auto place_rows(std::size_t n_rows) -> bool {
    while (
      n_rows-- > 0_uz &&
      place_row<reverse>() &&
      n_rows-- > 0_uz &&
      place_row<!reverse>()
    );
    return !unplaced.empty();
  }

  // PUBLIC METHODS ------------------------------------------------------------------------------
 public:
  /// Run the grid swarm algorithm.
  auto place_dots() -> SEXP {
    for (const auto& xs : xs_list) {
      current_row = 0_z;

      // we divide by xsize here so that all the distance calculations for checking
      // overlaps can be done in standardized units of 1 dot diameter, then we
      // multiply final positions by xsize and ysize before final output.
      unplaced.clear();
      for (const auto x : xs) unplaced.emplace_back(x / xsize);

      // place dots in rows, alternating direction (but also ensuring every strata-th row alternates)
      while (
        place_rows<FWD>(strata) &&
        place_rows<REV>(strata)
      ) {
        Rcpp::checkUserInterrupt();
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
//' @param strata <scalar [numeric]> size of the y grid (corresponding to 1 + the number of adjacent
//' rows above or below this row that could overlap with dots in this row).
//' @param signed_side <scalar [integer]> which side to place dots on?
//' -  `0` = both
//' -  `1` = above
//' - `-1` = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP grid_swarm_(
  const std::vector<Rcpp::NumericVector>& xs_list,
  const double xsize,
  const double ysize,
  const std::ptrdiff_t strata,
  const int signed_side
) {
  return GridSwarm{xs_list, xsize, ysize, strata, signed_side}.place_dots();
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
