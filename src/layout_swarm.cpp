#include "util.hpp"

#include <Rcpp.h>
#include <Rinternals.h>

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <functional>
#include <queue>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

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
  // TYPES ---------------------------------------------------------------------------------------
  /// A group of normalized x values to place
  using Group = std::vector<double>;
  /// Vector of groups
  using Groups = std::vector<Group>;
  /// Iterator pointing to a Group
  using GroupIt = Groups::const_iterator;
  /// Iterator pointing to a normalized x value
  using XIt = Group::const_iterator;

  /// An unplaced region containing one or more dots.
  /// Represents all unplaced dots in the half-open interval [x_1, x_2).
  struct Unplaced {
    /// Lowest group still containing unplaced dots in this region.
    GroupIt groupi;
    /// First dot in groupi in this region
    XIt xi_1;
    /// First dot in groupi after this region
    XIt xi_2;
    /// Lower limit of this region
    double x_1;
    /// Upper limit of this region
    double x_2;
    /// Current guess at the y value the lowest dot in this region would be placed at.
    double y;
    /// Group-specific penalty applied to `y` when queueing it for placement.
    double penalty;

    /// A region of unplaced dots in the plot.
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
        penalty{(groupi - outer.groups.cbegin()) * outer.group_penalty} {};

    /// Order unplaced regions by their `y` value (accounting for group penalties).
    friend auto operator>(const Unplaced& u1, const Unplaced& u2) -> bool {
      return u1.y + u1.penalty > u2.y + u2.penalty;
    }
  };

  /// A placed dot.
  /// A dot that has been placed in a particular y location on one side of the plot.
  struct Dot {
    /// Normalized x position
    double x;
    /// Normalized y position
    double y;

    constexpr Dot(const double x, const double y)
      : x{x}, y{y} {};

    /// Order dots by their x value then by y value.
    friend constexpr auto operator<(const Dot& d1, const Dot& d2) -> bool {
      return std::tie(d1.x, d1.y) < std::tie(d2.x, d2.y);
    }
  };

  /// One side of the plot.
  enum Side : bool {
    TOP = false,
    BTM = true
  };

  /// Frontier of placed dots on one side of the plot.
  using Frontier = std::set<Dot>;
  /// Iterator pointing to a single Dot in a Frontier.
  using DotIt = Frontier::iterator;

  // CONSTRUCTORS ---------------------------------------------------------------------------------
 public:
  /// Initialize the compact swarm algorithm.
  CompactSwarm(
    const std::vector<Rcpp::NumericVector>& xs_list,
    const double xsize,
    const double ysize,
    const int signed_side,
    const double group_penalty
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
      group_penalty{group_penalty}
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

  // FIELDS ---------------------------------------------------------------------------------
 private:
  // inputs and derived values
  /// List of unnormalized x values for in each group.
  const std::vector<Rcpp::NumericVector>& xs_list;
  /// Size of dots in the x dimension.
  const double xsize;
  /// Size of dots in the y dimension.
  const double ysize;
  /// Are we placing dots on both sides?
  const bool both;
  /// Side we are placing on (always `TOP` if `both` is `false`).
  const Side side;
  /// Total number of dots (sum of sizes of groups in xs_list).
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

  /// Groups of normalized x values we are currently placing.
  /// Groups are ordered from first placed to last. The x values in each group are normalized so
  /// that a distance of 1 is one dot diameter (`xsize`) and sorted in increasing order.
  Groups groups = {};

  /// Group placement penalty
  /// Placement penalty (as a proportion of a single row) of a dot from a group being placed one
  /// row too early in the stacking order. A value of 0 means no penalty, and a value of 1 ensures
  /// groups are stacked in order but often produces white gaps. 0.5 tends to be reasonable.
  const double group_penalty = 0.5;

  /// "Frontier" of placed dots
  /// Contains placed dots in increasing x order. Used to find the lowest placed point for dots.
  /// Automatically pruned to remove dots far enough below `min_y` that we will never need to check
  /// them again.
  Frontier frontier[2] = {};

  /// Minimum y value at which the next dot in any group may be placed.
  /// We update this minimum as we place dots and use it to remove values from the frontier that we
  /// won't need to check against again.
  double min_y = 0.0;

  /// Priority queue of regions to search for the lowest dot to place next.
  std::priority_queue<Unplaced, std::vector<Unplaced>, std::greater<Unplaced>> next_unplaced = {};

  // PRIVATE METHODS -----------------------------------------------------------------------------
 private:
  /// Find the minimum y placement of a dot on one side of the chart.
  /// Checks along the `frontier` to determine the lowest point a dot with the given `x` value can
  /// be placed at without intersecting already-placed dots.
  /// @param x normalized x value of dot to attempt to place.
  /// @param s side to search on.
  /// @returns a value >= `min_y`: the lowest `y` value `x` can be placed at on Side `s`
  /// without intersecting anything in the `frontier`. Normalized `y` positions are always
  /// increasing positively away from the axis (i.e. they are negated if `s == BTM`).
  auto min_dot_y(const double x, const Side s) -> double {
    auto y = 0.0;

    for (
      auto existing_dot = frontier[s].lower_bound({x - 1, 0}); existing_dot != frontier[s].end();
    ) {
      const auto x_distance = std::abs(x - existing_dot->x);
      if (x_distance > 1.0) break;  // all further dots must be out of range

      if (existing_dot->y < min_y - 1.0) {
        // this existing dot will never collide with any future dots, we can remove it to make
        // future checks more efficient.
        existing_dot = frontier[s].erase(existing_dot);
      } else {
        y = std::max(y, std::sqrt(1 - sq(x_distance)) + existing_dot->y);
        ++existing_dot;
      }
    }

    return y;
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
    return unimodal_min(xi_1, xi_2, [this, s](const double x) { return min_dot_y(x, s); });
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

  /// Place a dot
  /// Places a dot in `out_x_arr` and `out_y_arr` and updates the `frontier` and `min_y`
  /// accordingly.
  /// @param x normalized x position to place dot at
  /// @param y normalized y position to place dot at
  /// @param s Side to place dot on
  void place_dot(const double x, const double y, const Side s, const GroupIt groupi) {
    // update the frontier
    frontier[s].emplace(x, y);
    // when placing on both sides, the opposite frontier shares all points within 1 unit
    // of the axis since these can collide with dots on the other side.
    if (both && y < 1) frontier[!s].emplace(x, -y);
    min_y = std::max(min_y, y - (groups.cend() - groupi));

    // output the non-normalized x and y positions
    out_x_arr[i] = x * xsize;
    out_y_arr[i] = y * ysize * (s ? -1.0 : 1.0);
    ++i;

    if (i % 1000 == 0) Rcpp::checkUserInterrupt();
  }

  // PUBLIC METHODS -----------------------------------------------------------------------------
 public:
  /// Run the compact swarm algorithm.
  auto place_dots() -> SEXP {
    // Build initial queue of regions to search.
    // We can quickly place a row of non-overlapping dots at the base of the plot and enqueue the
    // regions between each of those dots.
    auto x_1 = -INF;
    const auto group0 = groups.cbegin();
    for (auto xi_1 = group0->cbegin(); xi_1 != group0->cend();) {
      place_dot(*xi_1, 0.0, side, group0);

      const auto xi_2 = std::lower_bound(xi_1, group0->cend(), *xi_1 + 1.0);
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
        // unplaced region may no longer be the lowest region, put it back in the queue at its new
        // position
        u.y = y_new;
        next_unplaced.push(u);
      } else {
        // lowest dot in the unplaced region is still where we thought it was => it is the lowest
        // region
        place_dot(*xi_new, y_new, s_new, u.groupi);

        // Queue regions [u.x_1, *xi_new) and [*xi_new, u.x_2) for future search.
        // Note that we must specify the new regions in two ways:
        // - [*xi_1, *xi_new) and [*(xi_new + 1), *xi_2), which are the subsets of `u` in `u.groupi`
        //   that may still contain unplaced dots after `*xi_new` is placed.
        // - [u.x_1, *xi_new) and [*xi_new, u.x_2), which are two contiguous regions whose union is
        //   `u` and which each contain one of the two regions above. We need to keep track of the
        //   full, contiguous unplaced regions so that if there are dots from other groups to be
        //   placed in these regions we can correctly calculate the boundaries of the regions for
        //   those groups.
        queue_region(u.groupi,     u.xi_1, xi_new,   u.x_1, *xi_new);
        queue_region(u.groupi, xi_new + 1, u.xi_2, *xi_new,   u.x_2);
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
//' @param signed_side <scalar [integer]> which side to place dots on?
//'  -  `0` = both
//'  -  `1` = above
//'  - `-1` = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP compact_swarm_(
  const std::vector<Rcpp::NumericVector>& xs_list,
  const double xsize,
  const double ysize,
  const int signed_side,
  const double group_penalty
) {
  return CompactSwarm{xs_list, xsize, ysize, signed_side, group_penalty}.place_dots();
}
