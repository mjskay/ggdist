#include "util.hpp"

#include <Rcpp.h>
#include <Rinternals.h>

#include <cmath>
#include <cstddef>
#include <functional>
#include <iterator>
#include <queue>
#include <tuple>
#include <utility>
#include <vector>

// compact swarm layout --------------------------------------------------------------

/// Divide-and-conquer approach to compact swarm layout
///
/// Does compact swarm layout by recursively searching contiguous regions of unplaced
/// dots for the lowest next dot.
///
/// When we place a dot, we add the contiguous region of unplaced dots just above and just below
/// that dot (between the just placed dot and its adjacent-in-x already-placed dots) to a priority
/// queue, prioritized by our current best guess at the minimum position of the next dot in that
/// region. Since unplaced regions are between two adjacent already-placed dots, the height of the
/// lowest dot in a region as a function of x value is generally well-behaved enough to use
/// Fibonacci search to efficiently find the lowest dot in a region without checking all dots
/// in a region. We use the priority queue to find the unplaced region with the lowest unplaced dot,
/// then recursively add the contiguous unplaced regions above and below each newly-placed dot back
/// to the queue.
///
/// Positions of placed dots are stored in a `frontier` divided into columns of dots that have a
/// minimum width of the dot `xsize` and which are sorted by dot y position. This allows us to
/// quickly find the highest/lowest already-placed dots to check for collisions when determining the
/// height a dot would be placed at.
class CompactSwarm {
  /// Candidate values we are currently placing, normalized so that a distance
  /// of 1 is one dot diameter (`xsize`).
  std::vector<double> candidates = {};
  using CandidateIt = decltype(candidates)::const_iterator;

  /// A single dot in the layout
  /// Dots may be placed, in which case they represent a single dot, or unplaced,
  /// in which case they represent a contiguous region of one or more consecutive dots
  /// in the input that may be placed here.
  struct Dot {
    CandidateIt xi_1;
    CandidateIt xi_2;
    double x;
    double y;
    bool placed = false;
    bool no_higher_after[2] = {false, false};
    Dot(const CandidateIt xi_1, const CandidateIt xi_2, const double x, const double y)
      : xi_1(xi_1), xi_2(xi_2), x(x), y(y) {};
    Dot(const CandidateIt xi_1, const CandidateIt xi_2)
      : Dot(xi_1, xi_2, *xi_1, 0.0) {};
  };

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

  // outputs
  Rcpp::NumericVector out_x_vec;
  Rcpp::NumericVector out_y_vec;
  double* out_x_arr;
  double* out_y_arr;

  /// Index of the next-to-be-placed value in out_x_vec / out_y_vec
  std::ptrdiff_t i = 0_z;

  /// Minimum y value at which the next dot may be placed.
  /// This is updated as we place dots and used to remove values from the frontier that we won't
  /// need to check against again.
  double min_y = 0.0;

  /// Minimum x value (normalized to binwidth == 1)
  double min_x = INF;
  /// Maximum x value (normalized to binwidth == 1)
  double max_x = -INF;
  /// Multiplier to go from normalized x value to frontier column index
  /// This is typically 1.0 except in cases where that would cause there
  /// to be more columns than data points.
  double x_to_col = 1.0;

  /// "Frontier" of placed dots and unplaced regions.
  using Frontier = std::list<Dot>;
  Frontier frontier = {};

  using DotIt = decltype(frontier.begin());
  struct region_is_greater : std::greater<DotIt> {
    bool operator()(const DotIt e1, const DotIt e2) const {
      return e1->y > e2->y;
    }
  };

  /// Priority queue of regions to search for the next dot to place.
  std::priority_queue<DotIt, std::vector<DotIt>, region_is_greater> queue = {};

 public:
  CompactSwarm(
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
      out_y_arr{REAL(out_y_vec)}
  {};

 private:
  /// Search the frontier for the placed dot that would collide with x at the highest point.
  /// Checks against placed dots in the open interval (r, end_<reverse>(frontier)).
  /// @param r one before the first dot to check
  /// @param x x position of the dot to check
  /// @returns <cd, y> iterator to the highest colliding dot `cd` and the y position where that dot
  /// would collide with a dot at `x`.
  template<bool reverse>
  auto highest_colliding_dot(const DotIt r, const double x) -> std::tuple<DotIt, double> {
    auto colliding_dot = frontier.end();
    auto y = min_y;

    for (auto existing_dot = next_<reverse>(r); existing_dot != end_<reverse>(frontier); ++existing_dot) {
      if (!existing_dot->placed) continue;

      const auto x_distance = std::abs(x - existing_dot->x);
      if (x_distance > 1.0) break; // all further dots must be out of range

      const auto new_y = std::sqrt(1 - sq(x_distance)) + existing_dot->y;
      if (new_y < y) continue;

      colliding_dot = as_forward_it(existing_dot);
      y = new_y;
      if (existing_dot->no_higher_after[reverse]) break; // all further dots are lower
    }

    return {colliding_dot, y};
  }

  /// Erase all placed dots in a region of the frontier
  /// Erases all placed dots in the half-open interval [begin, end) of the frontier.
  /// @param begin first dot to erase (if it is placed)
  /// @param end one past the last dot to erase
  void erase_placed_dots(const DotIt begin, const DotIt end) {
    for (auto existing_dot = begin; existing_dot != end; ) {
      if (existing_dot->placed) {
        existing_dot = erase_(frontier, existing_dot);
      } else {
        ++existing_dot;
      }
    }
  }

  /// Are there no unplaced dots within 1 unit of x in the given range?
  template<typename FrontierIt>
  auto no_unplaced_in_range(const double x, const FrontierIt begin, const FrontierIt end) -> bool {
    for (auto existing_dot = begin; existing_dot != end; ++existing_dot) {
      if (
        std::abs(*existing_dot->xi_1 - begin->x) > 1.0 &&
        std::abs(*(existing_dot->xi_2 - 1) - begin->x) > 1.0
      ) {
        break;
      }
      if (!existing_dot->placed) return false;
    }
    return true;
  }

  /// Erase noncolliding dots in a region of the frontier
  /// Noncolliding dots are those in the interior of the frontier that, after
  /// dot `d` is placed, will no longer collide with any subsequent dots (so we
  /// never have to check them again)
  /// @param placed A newly-placed dot.
  void erase_noncolliding_dots(const DotIt d) {
    // erase the noncolliding dots to the right of r
    auto [cd_r, y_r] = highest_colliding_dot<false>(d, d->x);
    if (cd_r != frontier.end()) {
      erase_placed_dots(std::next(d), cd_r);
    }
    // erase the noncolliding dots to the left of r
    auto [cd_l, y_l] = highest_colliding_dot<true>(d, d->x);
    if (cd_l != frontier.end()) {
      erase_placed_dots(std::next(cd_l), d);
    }
  }

  /// Find the lowest y placement of a dot.
  /// Checks along the `frontier` to determine the lowest point this dot can be placed at without
  /// intersecting already-placed dots.
  /// @tparam bottom search from the bottom?
  /// If `true`, searches from the bottom side up and returns `-y` so that the result is always
  /// positive (this is because we use the lowest value to pick where to place the dot, so the value
  /// is always a positive value relative to the side we are placing it on).
  /// @param d unplaced dot to attempt to place.
  /// @param x normalized x value of dot to attempt to place.
  /// @returns the lowest `y` value `x` can be placed at without intersecting anything in the
  /// `frontier`, or `min_y` if `x` does not intersect anything in the `frontier`.
  template<bool bottom>
  auto min_dot_y(const DotIt d, const double x) -> double {
    auto [cd_r, y_r] = highest_colliding_dot<false>(d, x);
    auto [cd_l, y_l] = highest_colliding_dot<true>(d, x);
    return std::max(y_r, y_l);
  }

  /// Find the minimum y placement of a dot in an unplaced region on one side of the chart.
  /// Finds the `x` value of the dot in the region [r->xi_1, r->xi_2) with the lowest possible `y`
  /// position placing on the top or bottom side of the chart.
  /// @tparam bottom search from the bottom?
  /// If `true`, searches from the bottom side up and returns `-y` so that the result is always
  /// positive (this is because we use the lowest value to pick where to place the dot, so the value
  /// is always a positive value relative to the side we are placing it on).
  /// @param r unplaced region defining the dot(s) to attempt to place.
  /// @returns <xi, y> Iterator `xi` to the lowest `x` value in `r` and the `y` position
  /// it would be placed on (negated if `bottom == true`).
  template<bool bottom>
  auto min_region_y(const DotIt r) -> std::tuple<CandidateIt, double> {
    return unimodal_min(r->xi_1, r->xi_2, [this, r](const double x) {
      return min_dot_y<bottom>(r, x);
    });
  }

  /// Find the minimum y placement of a dot in an unplaced region.
  /// Finds the `x` value of the dot in the region [r->xi_1, r->xi_2) with the lowest possible `y`
  /// position.
  /// @param r unplaced region defining the dot(s) to attempt to place.
  /// @param s side to search on. If `Side::BOTH`, both sides are searched and the value
  /// from the lowest side is returned.
  /// @returns <xi, y, s> Iterator `xi` to the lowest `x` value in `values` and the `y` position
  /// it would be placed at on Side `s` (negated if `s == Side::BOTTOM`).
  auto min_region_y(const DotIt r, const Side s) -> std::tuple<CandidateIt, double, Side> {
    switch (s) {
      case Side::BOTH: {
        const auto [xi_top, y_top] = min_region_y<false>(r);
        const auto [xi_btm, y_btm] = min_region_y<true>(r);
        if (y_btm < y_top) return {xi_btm, y_btm, Side::BOTTOM};
        else return {xi_top, y_top, Side::TOP};
      }
      case Side::TOP: {
        const auto [xi, y] = min_region_y<false>(r);
        return {xi, y, s};
      }
      default: { // Side::BOTTOM
        const auto [xi, y] = min_region_y<true>(r);
        return {xi, y, s};
      }
    }
  }

  /// Place a dot
  /// Places a dot in `out_x_arr` and `out_y_arr` and updates the `frontier` and `min_y`
  /// accordingly.
  /// @param d unplaced dot.
  /// @param x normalized x position to place dot at
  /// @param y normalized y position to place dot at (negated if `s == Side::BOTTOM`).
  /// @param s Side to place dot on
  void place_dot(const DotIt d, const Side s) {
    // update the frontier
    erase_noncolliding_dots(d);
    d->placed = true;
    d->no_higher_after[false] = no_unplaced_in_range(d->x, std::next(d), frontier.end());
    d->no_higher_after[true] = no_unplaced_in_range(d->x, std::reverse_iterator(d), frontier.rend());
    if (d->y > min_y) min_y = d->y;

    // output the non-normalized x and y positions
    out_x_arr[i] = d->x * xsize;
    out_y_arr[i] = d->y * ysize * (s == Side::BOTTOM ? -1.0 : 1.0);
    ++i;

    if (i % 1000 == 0) Rcpp::checkUserInterrupt();
  }

  /// Create and enqueue the region [xi_1, xi_2) for future search.
  /// Does not calculate an initial guess at best placement: just enters 0.0 as the "best guess".
  /// This will cause the lowest dot in this region to be recalculated later.
  /// @param before iterator to the position in the frontier to insert the new region before
  /// @param xi_1 lower limit of region
  /// @param xi_2 upper limit of region
  void queue_region_without_guess(const DotIt before, const CandidateIt xi_1, const CandidateIt xi_2) {
    if (xi_2 - xi_1 <= 0) return;
    queue.push(frontier.emplace(before, xi_1, xi_2));
  }

  /// Create and enqueue the region [xi_1, xi_2) for future search.
  /// Produces a guess for lower limit of `y` before enqueuing.
  /// @param xi_1 lower limit of region
  /// @param xi_2 upper limit of region
  void queue_region(const DotIt before, const CandidateIt xi_1, const CandidateIt xi_2) {
    if (xi_2 - xi_1 <= 0) return;
    auto r = frontier.emplace(before, xi_1, xi_2);
    const auto [xi, y, s] = min_region_y(r, side);
    r->x = *xi;
    r->y = y;
    queue.push(r);
  }

 public:
  /// Run the compact swarm algorithm.
  auto place_dots() -> SEXP {
    auto first_group = true;
    for (const auto& xs : xs_list) {
      candidates.clear();
      min_y = 0.0;

      // we divide by xsize here so that all the distance calculations for checking
      // overlaps can be done in standardized units of 1 dot diameter, then we
      // multiply final positions by xsize and ysize before final output.
      for (const auto x : xs) candidates.emplace_back(x / xsize);

      // Build initial queue of regions to search
      if (first_group) {
        // For the first group we can quickly place a row of non-overlapping dots at the
        // base of the plot and enqueue the regions between each of those dots
        for (auto xi_1 = candidates.cbegin(); xi_1 != candidates.cend();) {
          auto d = frontier.emplace(frontier.end(), xi_1, xi_1 + 1);
          place_dot(d, side);

          auto xi_2 = advance_to_at_least(candidates, xi_1, *xi_1 + 1.0);
          // we queue without guessing here because the next dot on the bottom row hasn't been
          // placed yet and will likely change the lowest position of this region, so spending the
          // time finding the lowest point now isn't worth it as it will likely be wrong and need to
          // immediately be recalculated (which putting in 0.0 will cause to happen anyway).
          queue_region_without_guess(frontier.end(), xi_1 + 1, xi_2);

          xi_1 = xi_2;
        }
        first_group = false;
      } else {
        // After the first group, we must rebuild a new priority queue of regions before
        // laying out each subsequent group. Since there are already dots laid down,
        // we use the highest points along the frontier from previously-placed groups
        // to define the boundaries of the regions.

        // Enqueue the regions between the highest points on the frontier
        auto xi_1 = candidates.cbegin();
        for (auto r = frontier.begin(); r != frontier.end(); ++r) {
          assert(r->placed);
          if (xi_1 == candidates.cend()) break;
          auto xi_2 = advance_to_at_least(candidates, xi_1, r->x);
          queue_region(r, xi_1, xi_2);
          xi_1 = xi_2;
        }
        queue_region(frontier.end(), xi_1, candidates.cend());
      }

      // repeatedly look for the region containing the lowest dot to insert and insert it
      while (!queue.empty()) {
        auto r = queue.top();
        queue.pop();

        const auto [xi_new, y_new, s_new] = min_region_y(r, side);
        if (y_new > r->y) {
          // region may no longer be the lowest region, put it back in the queue at its new position
          r->x = *xi_new;
          r->y = y_new;
          queue.push(r);
        } else {
          // lowest dot in region is still where we thought it was => it is the lowest region
          place_dot(r, s_new);

          // enqueue [xi_1, x_m) and (x_m, xi_2) for future search
          queue_region(r, r->xi_1, xi_new);
          queue_region(std::next(r), xi_new + 1, r->xi_2);
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
//' @param xs_list <list of [numeric]> list of vectors of sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP compact_swarm_(
  const std::vector<Rcpp::NumericVector>& xs_list,
  const double xsize,
  const double ysize,
  const int side
) {
  return CompactSwarm{xs_list, xsize, ysize, side}.place_dots();
}
