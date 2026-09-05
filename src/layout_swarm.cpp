#include "util.hpp"

#include <Rcpp.h>
#include <Rinternals.h>

#include <cmath>
#include <cstddef>
#include <functional>
#include <iterator>
#include <queue>
#include <tuple>
#include <vector>

// compact swarm helpers ------------------------------------------------------

namespace {

/// A single dot in the layout
struct dot {
  double x;
  double y;

  constexpr dot(const double x, const double y) : x{x}, y{y} {};
};

constexpr auto operator==(const dot e1, const dot e2) -> bool {
  return e1.x == e2.x && e1.y == e2.y;
}

constexpr auto squared_dist(const dot e1, const dot e2) -> double {
  return sq(e1.x - e2.x) + sq(e1.y - e2.y);
}

}  // namespace


// compact swarm layout --------------------------------------------------------------

/// Divide-and-conquer approach to compact swarm layout
///
/// Does compact swarm layout by building a `frontier` of placed dots along the
/// top edge of the swarm and recursively searching contiguous regions of unplaced
/// dots for the lowest next dot.
///
/// When we place a dot, we add the contiguous region of unplaced dots just above and just below
/// that dot to a priority queue, prioritized by our current best guess at the minimum
/// position of the next dot in that region. Since unplaced regions are contiguous regions between
/// two placed dots, we can use Fibonacci search to efficiently search for the lowest dot in a
/// region without checking all dots in a region. We use the priority queue to find the unplaced
/// region with the lowest unplaced dot, then recursively add the contiguous unplaced regions
/// above and below each newly-placed dot back to the queue.
class compact_swarm {
  enum class Side {
    TOP = 0_z,
    BOTTOM = 1_z,
    BOTH = 2_z
  };

  // inputs and derived values
  const std::vector<Rcpp::NumericVector>& xs_list;
  const double xsize;
  const double ysize;
  const Side side;
  const std::ptrdiff_t n;
  Rcpp::NumericVector out_x_vec;
  Rcpp::NumericVector out_y_vec;
  double* out_x_arr;
  double* out_y_arr;
  std::ptrdiff_t i;

  /// "Frontier" of placed dots that new dots may collide with.
  /// The frontier consists of columns of dots partitioned by
  /// contiguous regions of x values and ordered by y value
  /// within each column.
  using Frontier = std::vector<std::vector<dot>>;
  Frontier frontiers[2] = {{}, {}};

  /// Minimum y value at which the next dot may be placed.
  /// This is updated as we place dots and used to remove values from
  /// the frontier that we won't need to check against again.
  double min_y = 0.0;

  /// Minimum x value (normalized to binwidth == 1)
  double min_value = INF;
  /// Maximum x value (normalized to binwidth == 1)
  double max_value = -INF;
  /// Multiplier to go from normalized x value to frontier column index
  double value_to_frontier = 1.0;

  /// Values we are currently placing, normalized so that a distance
  /// of 1 is one dot diameter (`xsize`).
  std::vector<double> values = {};
  using ValueIt = decltype(values)::const_iterator;

  /// Regions to search for values to place.
  /// <xi_1, xi_2, y> is a half-open interval [xi_1, xi_2) on `values`
  /// with the `y` position the lowest dot in the region is *likely*
  /// to be placed at (`y` is always less than the ultimate position
  /// of the lowest dot in the region, which may end up higher).
  using Region = std::tuple<ValueIt, ValueIt, double>;
  struct region_is_greater : std::greater<Region> {
    constexpr bool operator()(const Region& e1, const Region& e2) const {
      return std::get<2>(e1) > std::get<2>(e2);
    }
  };

  /// Priority queue of regions to search for the next dot to place.
  std::priority_queue<Region, std::vector<Region>, region_is_greater> queue = {};

 public:
  compact_swarm(
    const std::vector<Rcpp::NumericVector>& xs_list,
    const double xsize,
    const double ysize,
    const int side
  )
    : xs_list{xs_list},
      xsize{xsize},
      ysize{ysize},
      side{side == 1 ? Side::TOP : side == -1 ? Side::BOTTOM : Side::BOTH},
      n{sum_sizes(xs_list)},
      out_x_vec(n),
      out_y_vec(n),
      out_x_arr{REAL(out_x_vec)},
      out_y_arr{REAL(out_y_vec)},
      i{0_z}
  {};

 private:
  /// Get the index of the column in the frontier associated with this x position
  constexpr auto frontier_col(const double x) -> std::ptrdiff_t {
    return static_cast<std::ptrdiff_t>((x - min_value) * value_to_frontier) + 1_z;
  }

  /// Find the minimum y placement of value `x`
  /// Checks along the `frontier` to determine the lowest point this dot can be
  /// placed at without intersecting already-placed dots.
  /// @param `x` value of dot to attempt to place.
  /// @param `frontier` the frontier to check for collisions against.
  /// @returns the lowest `y` value `x` can be placed at without intersecting
  /// anything in the `frontier`, or `min_y` if `x` does not intersect anything
  /// in the `frontier`. Also updates the `frontier` to remove any values less
  /// than the current `min_y` as it goes.
  auto min_dot_y(const double x, Frontier& frontier) -> double {
    auto y = min_y;

    // the frontier contains columns of width at least 1, so we only need
    // to examine the columns just before and just after this dot
    const auto middle_col_i = frontier_col(x);
    auto cols = std::vector{
      std::pair{frontier[middle_col_i - 1].rbegin(), frontier[middle_col_i - 1].rend()},
      std::pair{frontier[middle_col_i].rbegin(), frontier[middle_col_i].rend()},
      std::pair{frontier[middle_col_i + 1].rbegin(), frontier[middle_col_i + 1].rend()}
    };
    while (!cols.empty()) {
      for (auto col = cols.begin(); col != cols.end();) {
        auto& [existing_dot, col_end] = *col;

        if (existing_dot == col_end || existing_dot->y < y - 1.0) {
          col = cols.erase(col);
          continue;
        }

        const auto x_distance = std::abs(x - existing_dot->x);
        if (x_distance <= 1.0) {
          const auto new_y = std::sqrt(1 - sq(x_distance)) + existing_dot->y;
          if (new_y > y) y = new_y;
        }

        ++existing_dot;
        ++col;
      }
    }

    return y;
  }

  /// Find the minimum y placement of a value in the half-open interval [xi_1, xi_2).
  /// Finds the `x` value in the region `xs` with the lowest possible `y` position.
  /// @param xi_1 lower limit of region to search
  /// @param xi_2 upper limit of region to search
  /// @returns <xi, y, s> Iterator `xi` to the lowest `x` value in `values` and the `y` position
  /// it would be placed at on Side `s`.
  auto min_region_y(const ValueIt xi_1, const ValueIt xi_2, const Side s) -> std::tuple<ValueIt, double, Side> {
    if (s == Side::BOTH) {
      const auto [xi_top, y_top, s_top] = min_region_y(xi_1, xi_2, Side::TOP);
      const auto [xi_btm, y_btm, s_btm] = min_region_y(xi_1, xi_2, Side::BOTTOM);
      if (y_btm < y_top) return {xi_btm, y_btm, Side::BOTTOM};
      else return {xi_top, y_top, Side::TOP};
    } else {
      auto& frontier = frontiers[static_cast<std::ptrdiff_t>(s)];
      const auto [xi, y] = unimodal_min(xi_1, xi_2, [this, &frontier](const double x) {
        return min_dot_y(x, frontier);
      });
      return {xi, y, s};
    }
  }


  /// Update the frontier with the given dot
  /// Adds the dot to the frontier and removes any old dots we won't need to check again.
  /// @param frontier top or bottom frontier to update
  /// @param x x position dot was placed at
  /// @param y y position dot was placed at
  void update_frontier(Frontier& frontier, const double x, const double y) {
    frontier[frontier_col(x)].emplace_back(x, y);
  }

  /// Place a dot
  /// Places a dot in `out_x_arr` and `out_y_arr` and updates the `frontier` and `min_y`
  /// accordingly.
  /// @param x x position to place dot at
  /// @param y y position to place dot at
  /// @param s Side to place dot at
  void place_dot(const double x, const double y, const Side s) {
    if (y > min_y) min_y = y;

    out_x_arr[i] = x * xsize;
    out_y_arr[i] = (s == Side::BOTTOM ? -y : y) * ysize;
    ++i;

    if (i % 1000 == 0) Rcpp::checkUserInterrupt();

    if (s == Side::BOTH) {
      update_frontier(frontiers[0], x, y);
      update_frontier(frontiers[1], x, y);
      // frontiers[0].emplace(x, y);
      // frontiers[1].emplace(x, y);
    } else {
      const auto si = static_cast<std::ptrdiff_t>(s);
      update_frontier(frontiers[si], x, y);
    }
  }

  /// Enqueue the region [xi_1, xi_2) for future search.
  /// @param xi_1 lower limit of region
  /// @param xi_2 upper limit of region
  /// @param y current best guess of `y` position of lowest dot in the region
  /// (does not have to be correct, but must be less than or equal to what
  /// ends up being the actual position of the lowest dot in this region).
  void queue_region(const ValueIt xi_1, const ValueIt xi_2, const double y) {
    if (xi_2 - xi_1 <= 0) return;
    queue.emplace(xi_1, xi_2, y);
  }
  /// Enqueue the region [xi_1, xi_2) for future search.
  /// Produces a guess for lower limit of `y` before enqueuing.
  /// @param xi_1 lower limit of region
  /// @param xi_2 upper limit of region
  void queue_region(const ValueIt xi_1, const ValueIt xi_2) {
    if (xi_2 - xi_1 <= 0) return;
    const auto [_, y, s] = min_region_y(xi_1, xi_2, side);
    queue_region(xi_1, xi_2, y);
  }

 public:
  /// Run the compact swarm algorithm.
  auto place_dots() -> SEXP {
    for (const auto& xs : xs_list) {
      for (const auto x : xs) {
        if (x < min_value) min_value = x;
        if (x > max_value) max_value = x;
      }
    }
    min_value /= xsize;
    max_value /= xsize;
    const auto value_range = std::max(max_value - min_value, 1.0);
    const auto frontier_range = std::max(std::min(value_range, static_cast<double>(n)), 1.0);
    value_to_frontier = frontier_range / value_range;
    const auto frontier_size = static_cast<std::ptrdiff_t>(frontier_range) + 3_z;
    for (auto& frontier : frontiers) frontier.resize(frontier_size);

    for (const auto& xs : xs_list) {
      values.clear();
      min_y = 0.0;

      // we divide by xsize here so that all the distance calculations for checking
      // overlaps can be done in standardized units of 1 dot diameter, then we
      // multiply final positions by xsize and ysize before final output.
      for (const auto x : xs) values.emplace_back(x / xsize);

      // place a base row of dots and set up a priority queue containing sub-regions
      // to search for the lowest dot in
      for (auto xi_1 = values.cbegin(); xi_1 != values.cend(); ) {
        place_dot(*xi_1, 0.0, side);

        auto xi_2 = advance_to_at_least(values, xi_1, *xi_1 + 1.0);
        // we use 0.0 here because the next dot hasn't been placed yet and will
        // likely change the lowest position of this region, so spending the time
        // guessing now isn't worth it as it will likely be wrong and need to immediately
        // be recalculated (which putting in 0.0 will cause to happen anyway).
        queue_region(xi_1 + 1, xi_2, 0.0);

        xi_1 = xi_2;
      }

      // repeatedly look for the region containing the lowest dot to insert and insert it
      while (!queue.empty()) {
        const auto [xi_1, xi_2, y_old] = queue.top();
        queue.pop();

        const auto [xi_new, y_new, s] = min_region_y(xi_1, xi_2, side);
        if (y_new > y_old) {
          // region is no longer the lowest region, put it back in the queue at its new position
          queue_region(xi_1, xi_2, y_new);
        } else {
          // lowest dot in region is still where we thought it was => it is the lowest region
          place_dot(*xi_new, y_new, s);

          // enqueue [xi_1, x_m) and (x_m, xi_2) for future search
          queue_region(xi_1, xi_new);
          queue_region(xi_new + 1, xi_2);
        }
      }
    }

    return Rcpp::DataFrame::create(
      Rcpp::Named("x") = out_x_vec,
      Rcpp::Named("y") = out_y_vec
    );
  }
};

//' Compact swarm layout
//' @param xs <list of [numeric]> list of vectors of sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP compact_swarm_(
  std::vector<Rcpp::NumericVector> xs, const double xsize, const double ysize, const int side
) {
  return compact_swarm{xs, xsize, ysize, side}.place_dots();
}
