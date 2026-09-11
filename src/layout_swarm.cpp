#include "util.hpp"

#include <Rcpp.h>
#include <Rinternals.h>

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <functional>
#include <iterator>
#include <queue>
#include <string>
#include <tuple>
#include <utility>
#include <variant>
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
      both{side == 0},
      bottom{side == -1 ? BTM : TOP},
      n{sum_sizes(xs_list)},
      out_x_vec(n),
      out_y_vec(n),
      out_x_arr{REAL(out_x_vec)},
      out_y_arr{REAL(out_y_vec)}
  {};

 private:
  enum Direction {
    NEXT = false,
    PREV = true
  };
  enum Side {
    TOP = false,
    BTM = true
  };

  // inputs and derived values
  const std::vector<Rcpp::NumericVector>& xs_list;
  const double xsize;
  const double ysize;
  const bool both;
  const Side bottom;
  const std::ptrdiff_t n;

  // outputs
  Rcpp::NumericVector out_x_vec;
  Rcpp::NumericVector out_y_vec;
  double* out_x_arr;
  double* out_y_arr;

  /// Index of the next-to-be-placed value in out_x_vec / out_y_vec
  std::ptrdiff_t i = 0_z;

  /// Candidate x values we are currently placing, normalized so that a distance
  /// of 1 is one dot diameter (`xsize`).
  std::vector<double> candidates = {};
  using CandidateIt = decltype(candidates)::const_iterator;

  /// An unplaced region containing one or more dots.
  /// A contiguous region of one or more consecutive dots in the input that may be placed
  /// in the half-open interval [xi_1, xi_2)
  struct Unplaced {
    CandidateIt xi_1;
    CandidateIt xi_2;
    double y;
    Unplaced(const CandidateIt xi_1, const CandidateIt xi_2, const double y)
      : xi_1(xi_1), xi_2(xi_2), y(y) {};
    Unplaced(const CandidateIt xi_1, const CandidateIt xi_2)
      : Unplaced(xi_1, xi_2, 0.0) {};
  };

  /// A placed dot.
  /// A dot that has been placed in a particular location.
  struct Dot {
    double x;
    double y;
    Dot(const double x, const double y)
      : x(x), y(y) {};
  };

  struct dot_is_less : std::less<Dot> {
    bool operator()(const Dot& e1, const Dot& e2) const {
      return e1.x < e2.x || (e1.x == e2.x && e1.y < e2.y);
    }
  };
  using Frontier = std::set<Dot, dot_is_less>;
  using DotIt = Frontier::iterator;

  /// "Frontier" of placed dots
  /// Contains placed dots in increasing x order representing placed dot or unplaced regions.
  Frontier frontier[2] = {};

  /// Minimum y value at which the next dot may be placed.
  /// This is updated as we place dots and used to remove values from the frontier that we won't
  /// need to check against again.
  double min_y = 0.0;

  struct unplaced_is_higher : std::greater<Unplaced> {
    bool operator()(const Unplaced& e1, const Unplaced& e2) const {
      return e1.y > e2.y;
    }
  };
  /// Priority queue of regions to search for the lowest dot to place next.
  std::priority_queue<Unplaced, std::vector<Unplaced>, unplaced_is_higher> queue = {};

  /// Search the frontier above (or below) di for the dot that would collide with x at the highest
  /// point. Checks against placed dots in the interval [di, frontier.end())
  /// @tparam search in reverse? If true, searches [frontier.begin(), di) starting from std::prev(di)
  /// backwards
  /// @param di starting point of search.
  /// @param x x position of the dot to check
  /// @returns <cd, y> iterator to the highest colliding dot `cd` and the y position where that dot
  /// would collide with a dot at `x`.
  template<Direction reverse, bool delete_min_y>
  auto highest_colliding_dot(const DotIt di, const double x, const Side s) -> std::tuple<DotIt, double> {
    auto colliding_dot = frontier[s].end();
    auto y = min_y;

    for (auto existing_dot = reverse_if<reverse>(di); existing_dot != end_<reverse>(frontier[s]); ) {
      const auto x_distance = std::abs(x - existing_dot->x);
      if (x_distance > 1.0) break; // all further dots must be out of range

      if constexpr (delete_min_y) {
        if (existing_dot->y < min_y - 1.0) {
          // this existing dot will never be within range of any future dots
          existing_dot = erase_(frontier[s], existing_dot);
          continue;
        }
      }

      const auto new_y = std::sqrt(1 - sq(x_distance)) + existing_dot->y;
      if (new_y >= y) {
        colliding_dot = as_forward_it(existing_dot);
        y = new_y;
      }

      ++existing_dot;
    }

    return {colliding_dot, y};
  }

  /// Erase dots
  /// Erase dots in the open interval (begin, end)
  void erase_dots(DotIt begin, const DotIt end, const Side s) {
    if (++begin == end) return;
    frontier[s].erase(begin, end);
  }

  /// Erase noncolliding dots in a region of the frontier
  /// Noncolliding dots are those in the interior of the frontier that, after
  /// dot `di` is placed, will no longer collide with any subsequent dots (so we
  /// never have to check them again)
  /// @param di A newly-placed dot.
  /// @param s side the dot was placed on.
  void erase_noncolliding_dots(const DotIt di, const Side s) {
    // TODO: remove?
    // erase the noncolliding dots to the right of r
    auto [cd_r, y_r] = highest_colliding_dot<NEXT, false>(std::next(di), di->x, s);
    if (cd_r != frontier[s].end()) erase_dots(di, cd_r, s);
    auto [cd_l, y_l] = highest_colliding_dot<PREV, false>(di, di->x, s);
    if (cd_l != frontier[s].end()) erase_dots(cd_l, di, s);
  }

  /// Find the lowest height to place a dot.
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
  auto min_dot_y(const double x, const Side s) -> double {
    auto di = frontier[s].lower_bound({x, 0});
    // must do PREV before NEXT because di could be deleted in NEXT
    auto [cd_l, y_l] = highest_colliding_dot<PREV, true>(di, x, s);
    auto [cd_r, y_r] = highest_colliding_dot<NEXT, true>(di, x, s);
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
  auto min_region_y(const CandidateIt xi_1, const CandidateIt xi_2, const Side s) -> std::tuple<CandidateIt, double> {
    return unimodal_min(xi_1, xi_2, [this, s](const double x) {
      return min_dot_y(x, s);
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
  auto min_region_y(const CandidateIt xi_1, const CandidateIt xi_2) -> std::tuple<CandidateIt, double, Side> {
    if (both) {
      const auto [xi_top, y_top] = min_region_y(xi_1, xi_2, TOP);
      const auto [xi_btm, y_btm] = min_region_y(xi_1, xi_2, BTM);
      if (y_btm < y_top) {
        return {xi_btm, y_btm, BTM};
      } else {
        return {xi_top, y_top, TOP};
      }
    } else {
      const auto [xi, y] = min_region_y(xi_1, xi_2, bottom);
      return {xi, y, bottom};
    }
  }

  /// Place a dot
  /// Places a dot in `out_x_arr` and `out_y_arr` and updates the `frontier` and `min_y`
  /// accordingly.
  /// @param d unplaced dot.
  /// @param x normalized x position to place dot at
  /// @param y normalized y position to place dot at (negated if `s == Side::BOTTOM`).
  /// @param s Side to place dot on
  void place_dot(const double x, const double y, const Side s) {
    // update the frontier
    auto [di, _] = frontier[s].emplace(x, y);
    erase_noncolliding_dots(di, s);
    if (both && y < 1) {
      // when placing on both sides, the opposite frontier shares all points within 1 unit
      // of the axis since these can collide with dots on the other side.
      auto [di_rev, _] = frontier[!s].emplace(x, -y);
      erase_noncolliding_dots(di_rev, Side(!s));
    }
    if (di->y > min_y) min_y = di->y;

    // output the non-normalized x and y positions
    out_x_arr[i] = di->x * xsize;
    out_y_arr[i] = di->y * ysize * (s ? -1.0 : 1.0);
    ++i;

    if (i % 1000 == 0) Rcpp::checkUserInterrupt();
  }

  /// Create and enqueue the region [xi_1, xi_2) for future search.
  /// Does not calculate an initial guess at best placement: just enters 0.0 as the "best guess".
  /// This will cause the lowest dot in this region to be recalculated later.
  /// @param xi_1 lower limit of region
  /// @param xi_2 upper limit of region
  void queue_region_without_guess(const CandidateIt xi_1, const CandidateIt xi_2) {
    if (xi_2 - xi_1 <= 0) return;
    queue.emplace(xi_1, xi_2, 0.0);
  }

  /// Create and enqueue the region [xi_1, xi_2) for future search.
  /// Produces a guess for lower limit of `y` before enqueuing.
  /// @param xi_1 lower limit of region
  /// @param xi_2 upper limit of region
  void queue_region(const CandidateIt xi_1, const CandidateIt xi_2) {
    if (xi_2 - xi_1 <= 0) return;
    auto [xi, y, s] = min_region_y(xi_1, xi_2);
    queue.emplace(xi_1, xi_2, y);
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
          place_dot(*xi_1, 0.0, bottom);

          auto xi_2 = advance_to_at_least(candidates, xi_1, *xi_1 + 1.0);
          // we queue without guessing here because the next dot on the bottom row hasn't been
          // placed yet and will likely change the lowest position of this region, so spending the
          // time finding the lowest point now isn't worth it as it will likely be wrong and need to
          // immediately be recalculated (which putting in 0.0 will cause to happen anyway).
          queue_region_without_guess(xi_1 + 1, xi_2);

          xi_1 = xi_2;
        }
        first_group = false;
      } else {
        // After the first group, we must rebuild a new priority queue of regions before
        // laying out each subsequent group. Since there are already dots laid down,
        // we use the highest points along the frontier from previously-placed groups
        // to define the boundaries of the regions.

        // TODO: can build frontier by testing all points against just +/- one nearest point to get
        // a min y, then rebuild the frontier using only placed points >= min_y - 1

        // Enqueue the regions between the existing points on the frontier
        auto xi_1 = candidates.begin();
        for (auto d = frontier[TOP].begin(); d != frontier[TOP].end(); ++d) {
          if (xi_1 == candidates.end()) break;
          auto xi_2 = advance_to_at_least(candidates, xi_1, d->x);
          queue_region(xi_1, xi_2);
          xi_1 = xi_2;
        }
        queue_region(xi_1, candidates.end());
      }

      // repeatedly look for the region containing the lowest dot to insert and insert it
      while (!queue.empty()) {
        auto u = queue.top();
        queue.pop();

        const auto [xi_new, y_new, s_new] = min_region_y(u.xi_1, u.xi_2);
        if (y_new > u.y) {
          // unplaced region may no longer be the lowest region, put it back in the queue at its new position
          u.y = y_new;
          queue.push(u);
        } else {
          // lowest dot in the unplaced region is still where we thought it was => it is the lowest region
          place_dot(*xi_new, y_new, s_new);

          // queue regions [xi_1, x_m) and (x_m, xi_2) for future search
          queue_region(u.xi_1, xi_new);
          queue_region(xi_new + 1, u.xi_2);
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
