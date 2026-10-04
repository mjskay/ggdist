#pragma once

#include "util.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <functional>
#include <queue>
#include <set>
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
/// lowest dot in a region as a function of x value is generally well-behaved enough to use golden
/// section search to efficiently find the lowest dot in a region without checking all dots in a
/// region. We use the priority queue to find the unplaced region with the lowest unplaced dot, then
/// recursively add the contiguous unplaced regions above and below each newly-placed dot back to
/// the queue.
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
/// @tparam Doubles Input and output datatype for vectors of `double`s. Must be a contiguous
/// container (in the STL sense) of `double`s and must have a constructor that takes a single
/// integer and yields a container of that size. Examples include `std::vector<double>` and
/// `Rcpp::NumericVector`.
template<typename Doubles>
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
  /// Represents all unplaced dots in the half-open interval [xi_1, xi_2).
  struct Unplaced {
    /// Lowest group still containing unplaced dots in this region.
    GroupIt groupi;
    /// First dot in groupi in this region
    XIt xi_1;
    /// First dot in groupi after this region
    XIt xi_2;
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
      const double y
    )
      : groupi{groupi},
        xi_1{xi_1},
        xi_2{xi_2},
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
  using DotIt = typename Frontier::iterator;

  // CONSTRUCTORS ---------------------------------------------------------------------------------
 public:
  /// Initialize the compact swarm algorithm.
  CompactSwarm(
    const std::vector<Doubles>& xs_list,
    const double xsize,
    const double ysize,
    const int signed_side,
    const double group_penalty,
    const std::function<void()>& check_interrupt = []() {}
  )
    : xs_list{xs_list},
      xsize{xsize},
      ysize{ysize},
      both{signed_side == 0},
      side{signed_side == -1 ? BTM : TOP},
      check_interrupt{check_interrupt},
      n{sum_sizes(xs_list)},
      out_x(n),
      out_y(n),
      out_x_arr{out_x.begin()},
      out_y_arr{out_y.begin()},
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
  const std::vector<Doubles>& xs_list;
  /// Size of dots in the x dimension.
  const double xsize;
  /// Size of dots in the y dimension.
  const double ysize;
  /// Are we placing dots on both sides?
  const bool both;
  /// Side we are placing on (always `TOP` if `both` is `false`).
  const Side side;
  /// Callback used to check for interrupts during processing
  const std::function<void()>& check_interrupt;
  /// Total number of dots (sum of sizes of groups in xs_list).
  const std::ptrdiff_t n;

  // outputs
  /// Unnormalized x values.
  Doubles out_x;
  /// Unnormalized y values.
  Doubles out_y;
  /// C array backing `out_x`
  double* out_x_arr;
  /// C array backing `out_y`
  double* out_y_arr;

  /// Index of the next-to-be-placed value in out_x and out_y
  std::ptrdiff_t i = 0_z;

  /// Groups of normalized x values we are currently placing.
  /// Groups are ordered from first placed to last. The x values in each group are normalized so
  /// that a distance of 1 is one dot diameter (`xsize`) and sorted in increasing order.
  Groups groups = {};

  /// Group placement penalty.
  /// Placement penalty (as a proportion of dot height, `ysize`) for a dot from group `i` being
  /// placed before a dot from group `i - 1`. A value of `1` means the y height of dots in group `i`
  /// will have a penalty of `group_penalty` rows more than group `i - 1` when added to the queue.
  /// Maximum value is `1` (which means a dot in group `i` cannot be placed at height `y` until all
  /// dots in group `i - 1` up to height `y + 1` have been placed).
  ///
  /// A value of 0 means no penalty, and a value of 1 ensures groups are stacked in order but often
  /// produces white gaps. A reasonable value is usually around `0.5`, which allows dots in group
  /// `i` to be placed even if some dots from group `i - 1` will overlap the area directly above it.
  const double group_penalty = 1.0;

  /// "Frontier" of placed dots
  /// Contains placed dots in increasing x order. Used to find the lowest placed point for dots.
  /// Automatically pruned to remove dots far enough below `frontier_min_y` that we will never need
  /// to check them again.
  Frontier frontier[2] = {};

  /// Minimum y value of dots in the frontier.
  /// This is the minimum y value of an already-placed dot that could intersect with any unplaced
  /// dot. We update this minimum as we place dots and use it to remove values from the frontier
  /// that we won't need to check against again.
  ///
  /// Because we place dots from lowest to highest, when there are no groups this is just the last
  /// placed y value minus 1, because if any remaining dots could have intersected with a dot at
  /// that position, it would have been placed at a lower y value than the most recently-placed dot.
  ///
  /// When there is more than 1 group, the logic is similar but must also account for the difference
  /// in group penalty terms between the most recently-placed dot and any possible remaining dots
  /// (see `place_dot()`, which is responsible for updating this value).
  double frontier_min_y = -1.0;

  /// Priority queue of regions to search for the lowest dot to place next.
  std::priority_queue<Unplaced, std::vector<Unplaced>, std::greater<Unplaced>> next_unplaced = {};

  // PRIVATE METHODS -----------------------------------------------------------------------------
 private:
  /// Find the minimum y placement of a dot on one side of the chart.
  /// Checks along the `frontier` to determine the lowest point a dot with the given `x` value can
  /// be placed at without intersecting already-placed dots.
  /// @param x normalized x value of dot to attempt to place.
  /// @param s side to search on.
  /// @returns a the lowest `y` value that `x` can be placed at on Side `s` without intersecting
  /// anything in the `frontier`. Normalized `y` positions are always non-negative values that
  /// increase positively away from the axis (i.e. they are negated if `s == BTM`).
  auto min_dot_y(const double x, const Side s) -> double {
    auto y = 0.0;

    for (
      auto existing_dot = frontier[s].upper_bound({x - 1.0, INF});
      existing_dot != frontier[s].end();
    ) {
      const auto x_distance = std::abs(x - existing_dot->x);
      if (x_distance >= 1.0) break;  // all further dots must be out of range

      if (existing_dot->y < frontier_min_y) {
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

  /// Create and enqueue the unplaced region [xi_1, xi_2) for future search.
  /// @tparam guess If `true` (the default), produces a guess for lower limit of `y` before
  /// enqueuing. If `false`, does not calculate an initial guess at best placement: just enters 0.0
  /// as the "best guess", which will cause the lowest dot in this region to be recalculated later.
  /// @param groupi x values in the current group
  /// @param xi_1 lower limit of region in `*groupi`
  /// @param xi_2 upper limit of region in `*groupi`
  template<bool guess = true>
  void queue_region(GroupIt groupi, XIt xi_1, XIt xi_2) {
    if (xi_2 <= xi_1) return;

    if constexpr (guess) {
      const auto [xi, y, s] = min_region_y(xi_1, xi_2);
      next_unplaced.emplace(*this, groupi, xi_1, xi_2, y);
    } else {
      next_unplaced.emplace(*this, groupi, xi_1, xi_2, 0.0);
    }
  }

  /// Place a dot
  /// Places a dot in `out_x_arr` and `out_y_arr` and updates the `frontier` and `frontier_min_y`
  /// accordingly.
  /// @param x normalized x position to place dot at
  /// @param y normalized y position to place dot at
  /// @param s Side to place dot on
  /// @param groupi Group this dot is from
  void place_dot(const double x, const double y, const Side s, const GroupIt groupi) {
    // update the frontier
    frontier[s].emplace(x, y);
    // when placing on both sides, the opposite frontier shares all points within 1 unit
    // of the axis since these can collide with dots on the other side.
    if (both && y < 1) frontier[!s].emplace(x, -y);

    // update the minimum y position used to prune the frontier
    // If there is only one group, this is just y - 1.0 (since a dot more than 1 unit lower than the
    // most recently-placed dot will never intersect with remaining dots). In the case of more than
    // one group we must adjust frontier_min_y to account for penalties applied to groups placed
    // after this group, otherwise we might prune dots from earlier groups too soon.
    frontier_min_y = std::max(frontier_min_y, y - 1.0 - (groups.cend() - groupi - 1.0) * group_penalty);

    // output the non-normalized x and y positions
    out_x_arr[i] = x * xsize;
    out_y_arr[i] = y * ysize * (s ? -1.0 : 1.0);
    ++i;

    if (i % 1024 == 0) check_interrupt();
  }

  // PUBLIC METHODS -----------------------------------------------------------------------------
 public:
  /// Run the compact swarm algorithm.
  /// @returns [x, y] The x and y positions of placed dots.
  auto place_dots() -> std::pair<Doubles, Doubles> {
    // Build initial queue of regions >= binwidth wide to search from each group.
    const auto group0 = groups.cbegin();
    for (auto groupi = group0; groupi != groups.cend(); ++groupi) {
      for (auto xi_1 = groupi->cbegin(); xi_1 != groupi->cend();) {
        const auto xi_2 = std::lower_bound(xi_1, groupi->cend(), *xi_1 + 1.0);

        // We can quickly place a row of non-overlapping dots from the first group at the base of
        // the plot and enqueue the regions between each of those dots. We don't place anything
        // from the other groups yet since their placements must account for the group penalties.
        if (groupi == group0) {
          place_dot(*xi_1, 0.0, side, groupi);
          ++xi_1;  // so that we queue (xi_1, xi_2) instead of [xi_1, xi_2) below.
        }

        // We queue without guessing here because:
        // - for the first group, the next dot on the bottom row hasn't been placed yet and will
        //   likely change the lowest position of this region
        // - for all other groups, depending on the penalty no dots in those groups will be placed
        //   until at least another set of dots from the first group are placed
        // so spending the time finding the lowest point now isn't worth it as it will likely be
        // wrong and need to immediately be recalculated.
        queue_region<false>(groupi, xi_1, xi_2);

        xi_1 = xi_2;
      }
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

        // Queue [xi_1, xi_2) \ {xi_new} == [xi_1, xi_new) and (xi_new, xi_2) for future search.
        queue_region(u.groupi, u.xi_1, xi_new);
        queue_region(u.groupi, xi_new + 1, u.xi_2);
      }
    }

    return {std::move(out_x), std::move(out_y)};
  }
};
