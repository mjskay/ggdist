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
#include <vector>


namespace {

enum Direction {
  NEXT = false,
  PREV = true
};
enum Side {
  TOP = false,
  BTM = true
};
constexpr Side operator!(const Side s) {
  return static_cast<Side>(!static_cast<bool>(s));
};

}  // namespace


// compact swarm layout --------------------------------------------------------------

/// Stackable compact swarm layout
///
/// Does compact swarm layout by recursively searching contiguous regions of unplaced dots for the
/// lowest next dot.
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
/// Positions of placed dots are stored in a `frontier` sorted by x position. As dots are placed, we
/// can trivially track the minimum y position that any subsequent dot could take (since we place
/// dots in increasing y order) and prune dots from the frontier that can no longer collide with
/// the minimum y position. This allows us to quickly check for collisions when determining the
/// height a dot would be placed at.
///
/// Dots can be grouped, and groups of dots are stacked. Stacking is achieved by applying a penalty
/// to regions in the queue based on how far up in the stacking order the lowest group in an
/// unplaced region is.
class CompactSwarm {
 public:
  CompactSwarm(
    const std::vector<Rcpp::NumericVector>& xs_list,
    const double xsize,
    const double ysize,
    const int signed_side
  )
    : xs_list{xs_list},
      xsize{xsize},
      ysize{ysize},
      both{signed_side == 0},
      side{signed_side == -1 ? BTM : TOP},
      n{sum_sizes(xs_list)},
      out_x_vec(n),
      out_y_vec(n),
      out_x_arr{REAL(out_x_vec)},
      out_y_arr{REAL(out_y_vec)},
      max_penalty{(xs_list.size() - 1) * group_penalty}
  {
    groups.reserve(xs_list.size());
    for (const auto& xs : xs_list) {
      auto& group = groups.emplace_back();
      group.reserve(xs.size());
      for (const auto x : xs) {
        // we divide by xsize here so that all the distance calculations for checking
        // overlaps can be done in standardized units of 1 dot diameter, then we
        // multiply final positions by xsize and ysize before final output.
        group.emplace_back(x / xsize);
      }
    }
  };

 private:
  // inputs and derived values
  const std::vector<Rcpp::NumericVector>& xs_list;
  decltype(xs_list.begin()) xs;
  const double xsize;
  const double ysize;
  const bool both;
  const Side side;
  const std::ptrdiff_t n;

  // outputs
  Rcpp::NumericVector out_x_vec;
  Rcpp::NumericVector out_y_vec;
  double* out_x_arr;
  double* out_y_arr;

  /// Index of the next-to-be-placed value in out_x_vec / out_y_vec
  std::ptrdiff_t i = 0_z;

  /// Groups of normalized x values we are currently placing.
  /// Groups are ordered from first placed to last. The x values in each group are normalized so
  /// that a distance of 1 is one dot diameter (`xsize`) and sorted in increasing order.
  std::vector<std::vector<double>> groups = {};
  using GroupIt = decltype(groups.cbegin());
  using XIt = decltype(groups.cbegin()->cbegin());

  /// Group penalty
  /// Placement penalty (as a proportion of a single row) of a dot from a group being placed one
  /// row too early in the stacking order. A value of 0 means no penalty, and a value of 1 ensures
  /// groups are stacked in order but often produces white gaps. 0.5 tends to be reasonable.
  static constexpr double group_penalty = 0.5;
  /// Maximum penalty = group_penalty * (numbers of groups - 1)
  const double max_penalty;

  /// An unplaced region containing one or more dots.
  /// A contiguous region of one or more consecutive dots in the input that may be placed
  /// in the half-open interval [x_1, x_2)
  struct Unplaced {
    GroupIt groupi;
    XIt xi_1;
    XIt xi_2;
    double x_1;
    double x_2;
    double y;
    double penalty;
    Unplaced(
      const CompactSwarm& outer,
      const GroupIt groupi,
      const XIt xi_1,
      const XIt xi_2,
      const double x_1,
      const double x_2,
      const double y
    )
      : groupi{groupi},
        xi_1{xi_1},
        xi_2{xi_2},
        x_1{x_1},
        x_2{x_2},
        y{y},
        penalty{(groupi - outer.groups.cbegin()) * group_penalty} {};
  };

  /// A placed dot.
  /// A dot that has been placed in a particular location.
  struct Dot {
    double x;
    double y;
    Dot(const double x, const double y)
      : x{x}, y{y} {};
  };

  struct dot_is_less : std::less<Dot> {
    bool operator()(const Dot& e1, const Dot& e2) const {
      return e1.x < e2.x || (e1.x == e2.x && e1.y < e2.y);
    }
  };
  using Frontier = std::set<Dot, dot_is_less>;
  using DotIt = Frontier::iterator;

  /// "Frontier" of placed dots
  /// Contains placed dots in increasing x order. Used to find the lowest placed point for dots.
  /// Automatically pruned to remove dots far enough below `min_y` that we will never need to check
  /// them again.
  Frontier frontier[2] = {};

  /// Minimum y value at which the next dot may be placed.
  /// This is updated as we place dots and used to remove values from the frontier that we won't
  /// need to check against again.
  double min_y = 0.0;

  struct unplaced_is_higher : std::greater<Unplaced> {
    bool operator()(const Unplaced& e1, const Unplaced& e2) const {
      return e1.y + e1.penalty > e2.y + e2.penalty;
    }
  };
  /// Priority queue of regions to search for the lowest dot to place next.
  std::priority_queue<Unplaced, std::vector<Unplaced>, unplaced_is_higher> next_unplaced = {};

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
    // TODO: remove?
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
    // return;
    // TODO: remove?
    // erase the noncolliding dots to the right of r
    auto [cd_r, y_r] = highest_colliding_dot<NEXT, false>(std::next(di), di->x, s);
    if (cd_r != frontier[s].end()) erase_dots(di, cd_r, s);
    auto [cd_l, y_l] = highest_colliding_dot<PREV, false>(di, di->x, s);
    if (cd_l != frontier[s].end()) erase_dots(cd_l, di, s);
  }

  /// Find the minimum y placement of a dot on one side of the chart.
  /// Checks along the `frontier` to determine the lowest point a dot with the given `x` value can
  /// be placed at without intersecting already-placed dots.
  /// @param x normalized x value of dot to attempt to place.
  /// @param s side to search on.
  /// @returns a value >= `min_y`: the lowest `y` value `x` can be placed at on Side `s`
  /// without intersecting anything in the `frontier`. Normalized `y` positions are always
  /// increasing positively away from the axis (i.e. they are negated if `s == BTM`).
  auto min_dot_y(const double x, const Side s) -> double {
    const auto di = frontier[s].lower_bound({x, 0});
    // must search PREV before NEXT because di could be deleted while searching NEXT
    const auto [cd_l, y_l] = highest_colliding_dot<PREV, true>(di, x, s);
    const auto [cd_r, y_r] = highest_colliding_dot<NEXT, true>(di, x, s);
    return std::max(y_r, y_l);
  }

  /// Find the minimum y placement of a dot in an unplaced region on one side of the chart.
  /// Finds the `x` value of the dot in the region [xi_1, xi_2) with the lowest possible `y`
  /// position on Side `s`.
  /// @param xi_1 lower limit of values to search
  /// @param xi_2 upper limit of values to search
  /// @param s side to search on.
  /// @returns {xi, y} Iterator `xi` to the lowest `x` value in [xi_1, xi_2) and the `y` position
  /// it would be placed at on Side `s`. `y` positions are always increasing positively away from
  /// the axis (i.e. they are negated if `s == BTM`).
  auto min_region_y(const XIt xi_1, const XIt xi_2, const Side s) -> std::tuple<XIt, double> {
    return unimodal_min(xi_1, xi_2, [this, s](const double x) {
      return min_dot_y(x, s);
    });
  }

  /// Find the minimum y placement of a dot in an unplaced region.
  /// Finds the `x` value of the dot in the region [xi_1, xi_2) with the lowest possible `y`
  /// position. If `both` is `true`, searches both sides and returns the minimum of both.
  /// @param xi_1 lower limit of values to search
  /// @param xi_2 upper limit of values to search
  /// @returns {xi, y, s} Iterator `xi` to the lowest `x` value in [xi_1, xi_2) and the `y` position
  /// it would be placed at on Side `s`. `y` positions are always increasing positively away from
  /// the axis (i.e. they are negated if `s == BTM`).
  auto min_region_y(const XIt xi_1, const XIt xi_2) -> std::tuple<XIt, double, Side> {
    if (both) {
      const auto [xi_top, y_top] = min_region_y(xi_1, xi_2, TOP);
      const auto [xi_btm, y_btm] = min_region_y(xi_1, xi_2, BTM);
      if (y_btm < y_top) {
        return {xi_btm, y_btm, BTM};
      } else {
        return {xi_top, y_top, TOP};
      }
    } else {
      const auto [xi, y] = min_region_y(xi_1, xi_2, side);
      return {xi, y, side};
    }
  }

  /// Create and enqueue the unplaced region [x_1, x_2) for future search.
  /// @tparam guess If `true` (the default), produces a guess for lower limit of `y` before
  /// enqueuing. If `false`, does not calculate an initial guess at best placement: just enters 0.0
  /// as the "best guess", which will cause the lowest dot in this region to be recalculated later.
  /// @param groupi x values in the current group
  /// @param xi_1 lower limit of region in `*groupi`
  /// @param xi_2 upper limit of region in `*groupi`
  /// @param x_1 lower limit x value, which may be <= `*xi_1` when the lower limit does not exactly
  /// coincide with a value in `*groupi`.
  /// @param x_2 upper limit x value, which may be >= `*xi_2` when the upper limit does not exactly
  /// coincide with a value in `*groupi`.
  template<bool guess = true>
  void queue_region(GroupIt groupi, XIt xi_1, XIt xi_2, const double x_1, const double x_2) {
    // If the current group does not contain any values in this region, check subsequent groups
    // until we find one that does
    while (xi_2 <= xi_1 && ++groupi != groups.cend()) {
      xi_1 = std::lower_bound(groupi->cbegin(), groupi->cend(), x_1);
      xi_2 = std::lower_bound(xi_1, groupi->cend(), x_2);
    }
    if (xi_2 <= xi_1) return;

    if constexpr (guess) {
      const auto [xi, y, s] = min_region_y(xi_1, xi_2);
      next_unplaced.emplace(*this, groupi, xi_1, xi_2, x_1, x_2, y);
    } else {
      next_unplaced.emplace(*this, groupi, xi_1, xi_2, x_1, x_2, 0.0);
    }
  }

  /// Update the frontier to account for a new dot.
  /// Add a dot to appropriate parts of the frontier so that future dots
  /// are tested against it.
  /// @param x normalized x position to place dot at
  /// @param y normalized y position to place dot at
  /// @param s Side to place dot on
  void update_frontier(const double x, const double y, const Side s) {
    const auto [di, _] = frontier[s].emplace(x, y);
    erase_noncolliding_dots(di, s);
  }

  /// Place a dot
  /// Places a dot in `out_x_arr` and `out_y_arr` and updates the `frontier` and `min_y`
  /// accordingly.
  /// @param x normalized x position to place dot at
  /// @param y normalized y position to place dot at
  /// @param s Side to place dot on
  void place_dot(const double x, const double y, const Side s, const double penalty) {
    // update the frontier
    update_frontier(x, y, s);
    // when placing on both sides, the opposite frontier shares all points within 1 unit
    // of the axis since these can collide with dots on the other side.
    if (both && y < 1) update_frontier(x, -y, !s);
    min_y = std::max(min_y, y - (max_penalty - penalty));

    // output the non-normalized x and y positions
    out_x_arr[i] = x * xsize;
    out_y_arr[i] = y * ysize * (s ? -1.0 : 1.0);
    ++i;

    if (i % 1000 == 0) Rcpp::checkUserInterrupt();
  }

 public:
  /// Run the compact swarm algorithm.
  auto place_dots() -> SEXP {
    // Build initial queue of regions to search.
    // We can quickly place a row of non-overlapping dots at the base of the plot and enqueue the
    // regions between each of those dots.
    auto x_1 = -INF;
    const auto group0 = groups.cbegin();
    for (auto xi_1 = group0->cbegin(); xi_1 != group0->cend();) {
      place_dot(*xi_1, 0.0, side, 0.0);

      const auto xi_2 = advance_to_at_least(*group0, xi_1, *xi_1 + 1.0);
      const auto x_2 = xi_2 == group0->cend() ? INF : *xi_2;
      // We queue without guessing here because the next dot on the bottom row hasn't been placed
      // yet and will likely change the lowest position of this region, so spending the time finding
      // the lowest point now isn't worth it as it will likely be wrong and need to immediately be
      // recalculated.
      queue_region<false>(group0, xi_1 + 1, xi_2, x_1, x_2);

      xi_1 = xi_2;
      x_1 = x_2;
    }

    // Repeatedly look for the unplaced region containing the lowest dot to insert and insert it
    while (!next_unplaced.empty()) {
      auto u = next_unplaced.top();
      next_unplaced.pop();

      const auto [xi_new, y_new, s_new] = min_region_y(u.xi_1, u.xi_2);
      if (y_new > u.y) {
        // unplaced region may no longer be the lowest region, put it back in the queue at its new position
        u.y = y_new;
        next_unplaced.push(u);
      } else {
        // lowest dot in the unplaced region is still where we thought it was => it is the lowest region
        place_dot(*xi_new, y_new, s_new, u.penalty);

        // Queue regions [x_1, *xi_new) and [*xi_new, x_2) for future search.
        // Note that we are careful to keep [x_1, *xi_new) U [*xi_new, x_2) contiguous even though
        // [*xi_1, *xi_new) U [*(xi_new + 1), xi_2) (which is a subset of the former two regions) is
        // not: we need the full unplaced regions to be contiguous so that if there are other groups
        // being placed the boundaries of the regions are correctly calculated for those groups.
        queue_region(u.groupi, u.xi_1, xi_new, u.x_1, *xi_new);
        queue_region(u.groupi, xi_new + 1, u.xi_2, *xi_new, u.x_2);
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
//' @param signed_side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP compact_swarm_(
  const std::vector<Rcpp::NumericVector>& xs_list,
  const double xsize,
  const double ysize,
  const int signed_side
) {
  return CompactSwarm{xs_list, xsize, ysize, signed_side}.place_dots();
}
