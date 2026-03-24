#include "util.hpp"

#include <Rcpp.h>
#include <Rinternals.h>

#include <cmath>
#include <cstddef>
#include <functional>
#include <queue>
#include <set>
#include <vector>

// compact swarm helpers ------------------------------------------------------

namespace {

/// A single dot in the layout
struct dot {
  double x;
  double y;

  constexpr dot(const double x, const double y) : x(x), y(y) {};
};

constexpr auto operator==(const dot e1, const dot e2) -> bool {
  return e1.x == e2.x && e1.y == e2.y;
}

constexpr auto squared_dist(const dot e1, const dot e2) -> double {
  return sq(e1.x - e2.x) + sq(e1.y - e2.y);
}

struct x_is_less : std::less<dot> {
  constexpr bool operator()(const dot& e1, const dot& e2) const {
    return e1.x < e2.x || (e1.x == e2.x && e1.y < e2.y);
  }
};

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
  // inputs and derived values
  const std::vector<Rcpp::NumericVector>& xs;
  const double xsize;
  const double ysize;
  const int side;
  const std::ptrdiff_t n;
  Rcpp::NumericVector out_x_vec;
  Rcpp::NumericVector out_y_vec;
  double* out_x_arr = REAL(out_x_vec);
  double* out_y_arr = REAL(out_y_vec);
  std::ptrdiff_t i;

  /// "Frontier" of placed dots that new dots may collide with.
  std::set<dot, x_is_less> frontier = {};

  /// Minimum y value at which the next dot may be placed.
  /// This is updated as we place dots and used to remove values from
  /// the frontier that we won't need to check against again.
  double min_y = 0.0;

  /// Values we are currently placing, normalized so that a distance
  /// of 1 is one dot diameter (`xsize`).
  std::deque<double> values = {};
  using ValueIt = decltype(values)::const_iterator;

  /// Regions to search for values to place.
  /// <x_1, x_2, y> is a half-open interval [x_1, x_2) on `values`
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
    const std::vector<Rcpp::NumericVector>& xs,
    const double xsize,
    const double ysize,
    const int side
  )
    : xs{xs},
      xsize{xsize},
      ysize{ysize},
      side{side},
      n{sum_sizes(xs)},
      out_x_vec(n),
      out_y_vec(n),
      out_x_arr{REAL(out_x_vec)},
      out_y_arr{REAL(out_y_vec)},
      i{0_z}
  {};

 private:
  /// Find the minimum y placement of value `x`
  /// Checks along the `frontier` to determine the lowest point this dot can be
  /// placed at without intersecting already-placed dots.
  /// @param `x` value of dot to attempt to place.
  /// @returns the lowest `y` value `x` can be placed at without intersecting
  /// anything in the `frontier`, or `min_y` if `x` does not intersect anything
  /// in the `frontier`. Also updates the `frontier` to remove any values less
  /// then the current `min_y` as it goes.
  auto min_y_placement(const double x) -> double {
    auto y = -INF;

    // compare this candidate to any existing dots within +/- 1 diameter, since
    // only these may overlap with it
    auto existing = frontier.lower_bound({x - 1, 0});
    const auto last_existing = frontier.upper_bound({x + 1, INF});
    while (existing != last_existing) {
      if (existing->y < min_y - 1) {
        // `existing` is now out of range of any candidate dots, no
        // need to check it for overlaps again
        // Rcpp::Rcout << "  Unfrontiering\t" << existing->x << "\t" << existing->y << std::endl;
        existing = frontier.erase(existing);
        continue;
      }

      const auto new_y = std::sqrt(1 - sq(x - existing->x)) + existing->y;
      if (new_y > y) y = new_y;

      ++existing;
    }

    return y == -INF ? min_y : y;
  }

  /// Find the minimum y placement of a value in the half-open interval [x_1, x_2).
  /// Finds the `x` value in the region `xs` with the lowest possible `y` position.
  /// @param x_1 lower limit of region to search
  /// @param x_2 upper limit of region to search
  /// @returns <x_i, y> Iterator to the lowest `x` value in `values` and the `y` position
  /// it would be placed at.
  auto min_y_placement(ValueIt x_1, ValueIt x_2) -> std::pair<ValueIt, double> {
    return unimodal_min(x_1, x_2, [this](double x) {
      return min_y_placement(x);
    });
  }

  /// Place a dot
  /// Places a dot in `out_x_arr` and `out_y_arr` and updates the `frontier` and `min_y`
  /// accordingly.
  /// @param x_i x position to place dot at
  /// @param y y position to place dot at
  void place_dot(const double x, const double y) {
    // Rcpp::Rcout << "  PLACING \t[" << (x_i - values.begin()) << "] =\t" << *x_i << "\t" << y << std::endl;
    if (y > min_y) min_y = y;

    out_x_arr[i] = x * xsize;
    out_y_arr[i] = y * ysize;
    ++i;

    if (i % 1000 == 0) Rcpp::checkUserInterrupt();

    frontier.emplace(x, y);
  }

  /// Enqueue the region [x_1, x_2) for future search.
  /// @param x_1 lower limit of region
  /// @param x_2 upper limit of region
  /// @param y current best guess of `y` position of lowest dot in the region (
  /// does not have to be correct, but must be less than the ultimate position
  /// of the lowest dot).
  void queue_region(ValueIt x_1, ValueIt x_2, double y) {
    if (x_2 - x_1 <= 0) return;
    // Rcpp::Rcout << "  Queuing \t[" << (x_1 - values.begin()) << ",\t" << (x_2 - values.begin()) << ")\t" << y << std::endl;
    // Rcpp::Rcout << "          \t[" << *x_1 << "..." << std::endl;
    queue.emplace(x_1, x_2, y);
  }
  /// Enqueue the region [x_1, x_2) for future search.
  /// Produces a guess for lower limit of `y` before enqueuing.
  /// @param x_1 lower limit of region
  /// @param x_2 upper limit of region
  void queue_region(ValueIt x_1, ValueIt x_2) {
    if (x_2 - x_1 <= 0) return;
    auto [_, y] = min_y_placement(x_1, x_2);
    queue_region(x_1, x_2, y);
  }

 public:
  /// Run the compact swarm algorithm.
  auto place_dots() -> SEXP {
    for (auto& x : xs) {
      values.clear();
      decltype(queue){}.swap(queue);  // queue.clear();
      min_y = 0.0;

      // we divide by xsize here so that all the distance calculations for checking
      // overlaps can be done in standardized units of 1 dot diameter, then we
      // multiply final positions by xsize and ysize before final output.
      for (const auto x_i : x) values.emplace_back(x_i / xsize);

      // place a base row of dots and set up a priority queue containing sub-regions
      // to search for the lowest dot in
      for (auto x_1 = values.cbegin(); x_1 != values.cend(); ) {
        place_dot(*x_1, 0.0);

        auto x_2 = advance_to_at_least(values, x_1, *x_1 + 1.0);
        // we use 0.0 here because the next dot hasn't been placed yet and will
        // likely change the lowest position of this region, so spending the time
        // guessing now isn't worth it as it will likely be wrong need to immediately
        // be recalculated (which putting in 0.0 will cause to happen anyway).
        queue_region(x_1 + 1, x_2, 0.0);

        x_1 = x_2;
      }

      // repeatedly look for the region containing the lowest dot to insert and insert it
      while (!queue.empty()) {
        auto [x_1, x_2, y] = queue.top();
        queue.pop();
        // Rcpp::Rcout << "Checking  \t[" << (x_1 - values.begin()) << ",\t" << (x_2 - values.begin()) << ")" << std::endl;
        // Rcpp::Rcout << "          \t[" << *x_1 << "..." << std::endl;

        auto [x_m, y_new] = min_y_placement(x_1, x_2);
        if (y_new > y) {
          // region is no longer the lowest region, put it back in the queue at its new position
          // Rcpp::Rcout << "  Re-queuing: \t" << y << " -> \t" << y_new << std::endl;
          queue_region(x_1, x_2, y_new);
        } else {
          // region is still where we thought it was => it is the lowest region
          place_dot(*x_m, y);

          // enqueue [x_1, x_m) and (x_m, x_2) for future search
          queue_region(x_1, x_m);
          queue_region(x_m + 1, x_2);
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
//' @param x <[numeric]> sorted x values
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
