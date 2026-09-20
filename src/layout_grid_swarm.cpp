#include "util.h"

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
  enum Direction : bool {
    FWD = false,
    REV = true
  };
  constexpr friend Direction operator!(const Direction direction) {
    return static_cast<Direction>(!static_cast<bool>(direction));
  };

  /// A single row of placed dots
  using Row = std::set<double>;
  /// Rows of placed dots
  using Rows = std::deque<Row>;
  /// Iterator to a single row
  using RowIt = Rows::iterator;

  // CONSTRUCTORS --------------------------------------------------------------------------------
 public:
  /// Initialize the grid swarm algorithm.
  GridSwarm(
    const std::vector<Rcpp::NumericVector>& xs_list,
    const double xsize,
    const double ysize,
    const int signed_side,
    const std::ptrdiff_t strata
  )
    : xs_list{xs_list},
      xsize{xsize},
      ysize{ysize},
      signed_side{signed_side},
      both{signed_side == 0},
      strata{strata},
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
  //// Side we are placing on (-1 = bottom, 0 = both, 1 = top).
  const int signed_side;
  /// Are we placing dots on both sides?
  const bool both;
  /// Number of rows in the grid in a distance of 1 `ysize`.
  const std::ptrdiff_t strata;
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
  Rows rows = {{}};

  /// Current row number.
  /// Distance from `origin_row_i` to the current row.
  /// - When `both == false`, this is also the index of the current row in `rows`.
  /// - When `both == true`, there are two current rows: `origin_row_i + current_row_num` and
  ///   `origin_row_i - current_row_num`.
  std::ptrdiff_t row_num = 0_z;

  /// Origin row.
  /// Iterator to the origin row in `rows` (i.e. the row at the axis).
  /// - When `both == false`, this is the base row of the plot (`rows.begin()`).
  /// - When `both == true`, the is the middle row (`rows.begin() + floor(rows.size() / 2)`).
  RowIt origin_row_i = rows.begin();

  // PRIVATE METHODS -----------------------------------------------------------------------------
 private:
  /// Attempt to place a dot in a target row
  /// @tparam reverse are we placing dots in reverse (from end to beginning)?
  /// @param x normalized x position of dot to attempt to place
  /// @param row_i iterator to row in `rows` to attempt to place `x` in
  /// @param min_next_x output parameter giving the next closest x position
  /// that a dot could be placed at in this row after `x` is placed
  /// @returns `true` if the dot was placed successfully
  template<Direction reverse>
  auto place_dot(
    const double x,
    const RowIt row_i,
    double& min_next_x
  ) -> bool {
    auto x_insert_loc = row_i->end();

    // check +/- (strata - 1) rows from the target row to see if the dot overlaps an existing dot
    const auto first_row_i = row_i - std::min(strata - 1_z, row_i - rows.begin());
    const auto last_row_i = row_i + std::min(strata, rows.end() - row_i);
    for (auto test_row_i = first_row_i; test_row_i != last_row_i; ++test_row_i) {
      if (test_row_i->empty()) continue;

      const auto rows_from_target = static_cast<double>(std::abs(test_row_i - row_i));
      const auto y_offset = rows_from_target / static_cast<double>(strata);
      const auto x_distance = std::sqrt(1 - sq(y_offset));

      auto x_loc_in_test_row = test_row_i->upper_bound(x);
      if (x_loc_in_test_row != test_row_i->end()) {
        const auto existing_dot_gt_x = *x_loc_in_test_row;
        if (x > existing_dot_gt_x - x_distance) {
          // overlap => can't place dot here
          min_next_x = existing_dot_gt_x + negate_if<reverse>(x_distance);
          return false;
        }
      }
      if (x_loc_in_test_row != test_row_i->begin()) {
        const auto existing_dot_lte_x = *std::prev(x_loc_in_test_row);
        if (x < existing_dot_lte_x + x_distance) {
          // overlap => can't place dot here
          min_next_x = existing_dot_lte_x + negate_if<reverse>(x_distance);
          return false;
        }
      }

      // if the test row is the target row we save x_insert_loc so we can give a hint to
      // speed up the call to row_i->insert() below
      if (rows_from_target == 0.0) x_insert_loc = x_loc_in_test_row;
    }

    // Place dot
    out_x_arr[i] = x * xsize;
    out_y_arr[i] = static_cast<double>(row_i - origin_row_i) * row_height;
    ++i;
    row_i->insert(x_insert_loc, x);
    min_next_x = x + negate_if<reverse>(1.0);

    return true;
  }

  /// Attempt to place dots in the current row in the grid_swarm algorithm
  /// @tparam reverse are we placing dots in reverse (from end to beginning)?
  /// @returns `true` if `unplaced` still has dots to place and `false` otherwise.
  template<Direction reverse>
  auto place_row() -> bool {
    if (unplaced.empty()) return false;

    const bool place_both = both && row_num > 0;  // center (origin) row only placed once
    auto min_next_x = reverse ? INF : -INF;
    auto min_next_x_top = min_next_x;
    auto min_next_x_btm = min_next_x;
    for (auto xi = begin_<reverse>(unplaced); xi != end_<reverse>(unplaced);) {
      const auto x = *xi;

      // attempt to place the dot, updating min_next_x_{top,btm} so we can skip dots that are
      // definitely not placeable (this is very important for performance, especially when binwidth
      // is large and many candidate dots are rejected)
      if (
        place_dot<reverse>(x, origin_row_i + row_num, min_next_x_top) ||
        (place_both && place_dot<reverse>(x, origin_row_i - row_num, min_next_x_btm))
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
    ++row_num;
    if (origin_row_i + row_num == rows.end()) {
      rows.emplace_back();
      origin_row_i = rows.begin();
      if (both) {
        rows.emplace_front();
        origin_row_i = rows.begin() + rows.size() / 2;
      }
    }

    return !unplaced.empty();
  }

  /// Place dots in `n_rows` rows in the grid_swarm algorithm, alternating directions.
  /// @param n_rows Number of rows to place.
  /// @see `place_row()`
  template<Direction reverse>
  auto place_rows(std::ptrdiff_t n_rows) -> bool {
    while (
      n_rows-- > 0_z &&
      place_row<reverse>() &&
      n_rows-- > 0_z &&
      place_row<!reverse>()
    );
    return !unplaced.empty();
  }

  // PUBLIC METHODS ------------------------------------------------------------------------------
 public:
  /// Run the grid swarm algorithm.
  auto place_dots() -> SEXP {
    for (const auto& xs : xs_list) {
      row_num = 0_z;

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
//' @param signed_side <scalar [integer]> which side to place dots on?
//' -  `0` = both
//' -  `1` = above
//' - `-1` = below
//' @param strata <scalar [numeric]> size of the y grid (corresponding to 1 + the number of adjacent
//' rows above or below this row that could overlap with dots in this row).
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP grid_swarm_(
  const std::vector<Rcpp::NumericVector>& xs_list,
  const double xsize,
  const double ysize,
  const int signed_side,
  const std::ptrdiff_t strata
) {
  return GridSwarm{xs_list, xsize, ysize, signed_side, strata}.place_dots();
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
