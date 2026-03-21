#include "util.hpp"

#include <Rcpp.h>
#include <Rinternals.h>

#include <cmath>
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

struct x_is_less : std::less<dot> {
  constexpr bool operator()(const dot& e1, const dot& e2) const {
    return e1.x < e2.x || (e1.x == e2.x && e1.y < e2.y);
  }
};

struct y_is_greater : std::greater<dot> {
  constexpr bool operator()(const dot& e1, const dot& e2) const {
    return e1.y > e2.y || (e1.y == e2.y && e1.x > e2.x);
  }
};

template<class T>
constexpr auto sq(const T x) {
  return x * x;
}

}  // namespace


// compact swarm layout --------------------------------------------------------------

//' Compact swarm layout
//' @param x <[numeric]> sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP compact_swarm_(
  std::vector<Rcpp::NumericVector> xs,
  const double xsize,
  const double ysize,
  const int side
) {
  auto n_out = 0_uz;
  for (const auto& x : xs) n_out += x.size();
  // const auto both = side == 0;

  auto i = 0_z;
  auto out_x_vec = Rcpp::NumericVector(n_out);
  auto out_y_vec = Rcpp::NumericVector(n_out);
  auto out_x_arr = REAL(out_x_vec);
  auto out_y_arr = REAL(out_y_vec);
  auto may_overlap = std::set<dot, x_is_less>{};

  for (auto& x : xs) {
    auto unplaced = std::priority_queue<dot, std::vector<dot>, y_is_greater>{};
    // we divide by xsize here so that all the distance calculations for checking
    // overlaps can be done in standardized units of 1 dot diameter, then we
    // multiply final positions by xsize and ysize before final output.
    for (const auto x_i : x) unplaced.emplace(x_i / xsize, 0);

    // repeatedly look for the next dot to insert: lowest unplaced dot we encounter whose
    // y value was not changed by the most recent insert
    while (!unplaced.empty()) {
      auto candidate = unplaced.top();
      unplaced.pop();

      // compare this candidate to any existing dots within +/- 1 diameter, since
      // only these may overlap with it
      auto existing = may_overlap.lower_bound({candidate.x - 1, candidate.y});
      auto last = may_overlap.upper_bound({candidate.x + 1, candidate.y});
      auto can_place = true;
      while (existing != last) {
        if (existing->y < candidate.y - 1) {
          // `existing` is now out of range of any candidate dots (because
          // `candidate` is always the unplaced dot with the lowest y value), no
          // need to check it for overlaps again
          existing = may_overlap.erase(existing);
          continue;
        }

        auto new_min_y = std::sqrt(1 - sq(candidate.x - existing->x)) + existing->y;
        if (new_min_y > candidate.y) {
          // min y at which we can insert `candidate` is above where we were planning
          // to insert it, so we no longer can guarantee it is the lowest candidate;
          // therefore put it back in the unplaced set so we can try another candidate.
          unplaced.emplace(candidate.x, new_min_y);
          can_place = false;
          break;
        }

        ++existing;
      }

      if (can_place) {
        may_overlap.insert(candidate);
        out_x_arr[i] = candidate.x * xsize;
        out_y_arr[i] = candidate.y * ysize;
        ++i;
        if (i % 1000 == 0) Rcpp::checkUserInterrupt();
      }
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = out_x_vec,
    Rcpp::Named("y") = out_y_vec
  );
}
