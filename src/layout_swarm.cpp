#include "util.hpp"

#include <Rcpp.h>
#include <Rinternals.h>

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <functional>
#include <queue>
#include <set>
#include <type_traits>
#include <vector>

// compact swarm helpers ------------------------------------------------------

namespace {

/// square a value
template<typename V>
constexpr auto sq(const V x) -> V {
  return x * x;
}

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

struct y_is_greater : std::greater<dot> {
  constexpr bool operator()(const dot& e1, const dot& e2) const {
    return e1.y > e2.y || (e1.y == e2.y && e1.x > e2.x);
  }
};

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
SEXP compact_swarm_naive_(
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
      auto min_y = candidate.y;
      while (existing != last) {
        if (existing->y < candidate.y - 1) {
          // `existing` is now out of range of any candidate dots (because
          // `candidate` is always the unplaced dot with the lowest y value), no
          // need to check it for overlaps again
          existing = may_overlap.erase(existing);
          continue;
        }

        auto new_min_y = std::sqrt(1 - sq(candidate.x - existing->x)) + existing->y;
        if (new_min_y > min_y) min_y = new_min_y;

        ++existing;
      }

      if (min_y > candidate.y) {
        // min y at which we can insert `candidate` is above where we were planning
        // to insert it, so we no longer can guarantee it is the lowest candidate;
        // therefore put it back in the unplaced set so we can try another candidate.
        unplaced.emplace(candidate.x, min_y);
      } else {
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

/// Update a candidate's min y position it can be placed at
/// and return the existing dot just touching this candidate
/// (such that it causes the candidate to be placed at its y position)
auto min_candidate_y(const dot candidate, std::set<dot, x_is_less>& may_overlap) -> double {
  // compare this candidate to any existing dots within +/- 1 diameter, since
  // only these may overlap with it
  auto existing = may_overlap.lower_bound({candidate.x - 1, candidate.y});
  const auto last_existing = may_overlap.upper_bound({candidate.x + 1, candidate.y});
  auto min_y = -INF;
  while (existing != last_existing) {
    if (existing->y < candidate.y - 2) {
      // `existing` is now out of range of any candidate dots, no
      // need to check it for overlaps again
      Rcpp::Rcout << "  Unfrontiering\t" << existing->x << "\t" << existing->y << std::endl;
      existing = may_overlap.erase(existing);
      continue;
    }

    const auto new_min_y = std::sqrt(1 - sq(candidate.x - existing->x)) + existing->y;
    if (new_min_y > min_y) {
      min_y = new_min_y;
    }

    ++existing;
  }
  return min_y == -INF ? candidate.y : min_y;
}

template<typename C>
constexpr auto sum_sizes(const C& container) -> std::ptrdiff_t {
  std::ptrdiff_t n = 0_z;
  for (const auto& x : container) n += x.size();
  return n;
}

class compact_swarm_prog {
  const std::vector<Rcpp::NumericVector>& xs;
  const double xsize;
  const double ysize;
  const int side;
  const std::ptrdiff_t n_out;
  std::ptrdiff_t i;
  Rcpp::NumericVector out_x_vec;
  Rcpp::NumericVector out_y_vec;
  double* out_x_arr = REAL(out_x_vec);
  double* out_y_arr = REAL(out_y_vec);

  std::set<dot, x_is_less> may_overlap = {};
  std::deque<double> unplaced = {};
  std::priority_queue<dot, std::vector<dot>, y_is_greater> queued = {};

 public:
  compact_swarm_prog(
    const std::vector<Rcpp::NumericVector>& xs,
    const double xsize,
    const double ysize,
    const int side
  )
    : xs{xs},
      xsize{xsize},
      ysize{ysize},
      side{side},
      n_out{sum_sizes(xs)},
      i{0_z},
      out_x_vec(n_out),
      out_y_vec(n_out),
      out_x_arr{REAL(out_x_vec)},
      out_y_arr{REAL(out_y_vec)}
  {};

 private:
  auto min_candidate_y(const dot candidate) -> double {
    // compare this candidate to any existing dots within +/- 1 diameter, since
    // only these may overlap with it
    auto existing = may_overlap.lower_bound({candidate.x - 1, candidate.y});
    const auto last_existing = may_overlap.upper_bound({candidate.x + 1, candidate.y});
    auto min_y = -INF;
    while (existing != last_existing) {
      if (existing->y < candidate.y - 2) {
        // `existing` is now out of range of any candidate dots, no
        // need to check it for overlaps again
        // Rcpp::Rcout << "  Unfrontiering\t" << existing->x << "\t" << existing->y << std::endl;
        existing = may_overlap.erase(existing);
        continue;
      }

      const auto new_min_y = std::sqrt(1 - sq(candidate.x - existing->x)) + existing->y;
      if (new_min_y > min_y) {
        min_y = new_min_y;
      }

      ++existing;
    }
    return min_y == -INF ? candidate.y : min_y;
  }

  inline void place_dot(const dot candidate) {
    // Rcpp::Rcout << "  PLACING \t" << candidate.x << "\t" << candidate.y << std::endl;
    may_overlap.insert(candidate);
    out_x_arr[i] = candidate.x * xsize;
    out_y_arr[i] = candidate.y * ysize;
    ++i;
    if (i % 1000 == 0) Rcpp::checkUserInterrupt();
  }

  inline auto position_new_candidate(const double x, const double y) -> dot {
    auto new_candidate = dot{x, y};
    new_candidate.y = min_candidate_y(new_candidate);
    return new_candidate;
  }

  inline void queue_candidate(const dot candidate) {
    // Rcpp::Rcout << "  Queuing " << candidate.x << "\t" << candidate.y << std::endl;
    queued.push(candidate);
  }

 public:
  inline auto place_dots() -> SEXP {
    for (auto& x : xs) {
      unplaced.clear();
      decltype(queued){}.swap(queued);  // queued.clear()

      // set up a queue of dots to check and a set of unplaced dots
      // we divide by xsize here so that all the distance calculations for checking
      // overlaps can be done in standardized units of 1 dot diameter, then we
      // multiply final positions by xsize and ysize before final output.
      auto next_x = -INF;
      for (const auto x_i : x) {
        if (x_i >= next_x) {
          queued.emplace(x_i / xsize, 0);
          next_x = x_i + xsize;
        } else {
          unplaced.emplace_back(x_i / xsize);
        }
      }

      // repeatedly look for the next dot to insert: lowest unplaced dot we encounter whose
      // y value was not changed by the most recent insert
      while (!queued.empty()) {
        Rcpp::checkUserInterrupt();
        auto candidate = queued.top();
        queued.pop();

        // Rcpp::Rcout << "Checking \t" << candidate.x << "\t" << candidate.y << std::endl;
        auto min_y = min_candidate_y(candidate);
        // for (const auto d : may_overlap) {
        //   Rcpp::Rcout << "         " << d.x << "\t" << d.y << std::endl;
        // }
        if (min_y > candidate.y) {
          // min y at which we can insert `candidate` is above where we were planning
          // to insert it, so we no longer can guarantee it is the lowest candidate;
          // therefore put it back in the queue so we can try another candidate.
          // Rcpp::Rcout << "  Re-queuing \t" << candidate.x << "\t" << min_y << std::endl;
          queued.emplace(candidate.x, min_y);
        } else {
          candidate.y = min_y;
          place_dot(candidate);

          // after we insert, add the first dot at least 1 diameter before/after this one to the queue
          if (unplaced.size() == 1) {
            auto new_candidate = position_new_candidate(unplaced.front(), candidate.y);
            queue_candidate(new_candidate);
            unplaced.pop_front();
          } else if (unplaced.size() >= 2) {
            auto first_after_it = advance_to_at_least(unplaced, unplaced.cbegin(), candidate.x + 1);
            if (first_after_it == unplaced.cend()) --first_after_it;
            auto first_after = position_new_candidate(*first_after_it, candidate.y);

            auto first_before_it = advance_to_at_least(unplaced, unplaced.crbegin(), candidate.x - 1);
            if (first_before_it == unplaced.crend()) --first_before_it;
            auto first_before = position_new_candidate(*first_before_it, candidate.y);

            if (first_before.y < first_after.y) {
              queue_candidate(first_before);
              erase_(unplaced, first_before_it);
            } else {
              queue_candidate(first_after);
              erase_(unplaced, first_after_it);
            }
          }
        }
      }
    }

    return Rcpp::DataFrame::create(
      Rcpp::Named("x") = out_x_vec,
      Rcpp::Named("y") = out_y_vec
    );
  }
};

class compact_swarm_prog_2 {
  const std::vector<Rcpp::NumericVector>& xs;
  const double xsize;
  const double ysize;
  const int side;
  const std::ptrdiff_t n_out;
  std::ptrdiff_t i;
  Rcpp::NumericVector out_x_vec;
  Rcpp::NumericVector out_y_vec;
  double* out_x_arr = REAL(out_x_vec);
  double* out_y_arr = REAL(out_y_vec);
  double min_y = 0.0;

  std::set<dot, x_is_less> frontier = {};

  std::deque<double> values = {};
  using ValueIt = decltype(values)::const_iterator;

  /// queued regions to next place values in as
  /// as half-open intervals [x_1, x_2) on `values`
  /// with the y position the region is likely to be
  /// placed at.
  using region = std::tuple<ValueIt, ValueIt, double>;
  struct region_is_greater : std::greater<region> {
    constexpr bool operator()(const region& e1, const region& e2) const {
      return std::get<2>(e1) > std::get<2>(e2);
    }
  };
  std::priority_queue<region, std::vector<region>, region_is_greater> queue = {};

 public:
  compact_swarm_prog_2(
    const std::vector<Rcpp::NumericVector>& xs,
    const double xsize,
    const double ysize,
    const int side
  )
    : xs{xs},
      xsize{xsize},
      ysize{ysize},
      side{side},
      n_out{sum_sizes(xs)},
      i{0_z},
      out_x_vec(n_out),
      out_y_vec(n_out),
      out_x_arr{REAL(out_x_vec)},
      out_y_arr{REAL(out_y_vec)}
  {};

 private:
  /// Find the minimum y placement of value `x`
  /// @param y suggested minimum placement
  auto min_y_placement(const double x) -> double {
    // compare this candidate to any existing dots within +/- 1 diameter, since
    // only these may overlap with it
    auto existing = frontier.lower_bound({x - 1, 0});
    const auto last_existing = frontier.upper_bound({x + 1, INF});
    auto y = -INF;
    while (existing != last_existing) {
      if (existing->y < min_y - 1) {
        // `existing` is now out of range of any candidate dots, no
        // need to check it for overlaps again
        // Rcpp::Rcout << "  Unfrontiering\t" << existing->x << "\t" << existing->y << std::endl;
        existing = frontier.erase(existing);
        continue;
      }

      const auto new_y = std::sqrt(1 - sq(x - existing->x)) + existing->y;
      if (new_y > y) {
        y = new_y;
      }

      ++existing;
    }
    return y == -INF ? min_y : y;
  }

  /// Find the minimum y placement of a value in the half-open interval [xs[0], xs[1]),
  /// returning the corresponding value location and `y` value.
  auto min_y_placement(std::pair<ValueIt, ValueIt> xs)
    -> std::pair<ValueIt, double> {
    auto [x_1, x_2] = xs;
    return unimodal_min(x_1, x_2, [this](double x) {
      return min_y_placement(x);
    });
  }

  void place_dot(ValueIt x_i, double y) {
    // Rcpp::Rcout << "  PLACING \t[" << (x_i - values.begin()) << "] =\t" << *x_i << "\t" << y << std::endl;
    if (y > min_y) min_y = y;
    out_x_arr[i] = *x_i * xsize;
    out_y_arr[i] = y * ysize;
    ++i;
    if (i % 1000 == 0) Rcpp::checkUserInterrupt();
    frontier.insert({*x_i, y});
  }

  /// Enqueue the region [x_1, x_2) for future search
  void queue_region(ValueIt x_1, ValueIt x_2, double y) {
    if (x_2 - x_1 <= 0) return;
    // Rcpp::Rcout << "  Queuing \t[" << (x_1 - values.begin()) << ",\t" << (x_2 - values.begin()) << ")\t" << y << std::endl;
    // Rcpp::Rcout << "          \t[" << *x_1 << "..." << std::endl;
    queue.emplace(x_1, x_2, y);
  }
  void queue_region(ValueIt x_1, ValueIt x_2) {
    if (x_2 - x_1 <= 0) return;
    auto [_, y] = min_y_placement({x_1, x_2});
    queue_region(x_1, x_2, y);
  }

 public:
  auto place_dots() -> SEXP {
    for (auto& x : xs) {
      values.clear();
      decltype(queue){}.swap(queue);  // queue.clear();
      min_y = 0.0;

      // we divide by xsize here so that all the distance calculations for checking
      // overlaps can be done in standardized units of 1 dot diameter, then we
      // multiply final positions by xsize and ysize before final output.
      for (const auto x_i : x) values.emplace_back(x_i / xsize);

      // place a base row of dots and set up a queue containing sub-regions to place dots in
      auto x_1 = values.cbegin();
      while (x_1 != values.cend()) {
        place_dot(x_1, 0.0);

        auto x_2 = advance_to_at_least(values, x_1, *x_1 + 1.0);
        queue_region(x_1 + 1, x_2, 0.0);
        x_1 = x_2;
      }

      // repeatedly look for the next dot to insert: lowest unplaced dot we encounter whose
      // y value was not changed by the most recent insert
      while (!queue.empty()) {
        auto [x_1, x_2, y] = queue.top();
        queue.pop();
        // Rcpp::Rcout << "Checking  \t[" << (x_1 - values.begin()) << ",\t" << (x_2 - values.begin()) << ")" << std::endl;
        // Rcpp::Rcout << "          \t[" << *x_1 << "..." << std::endl;

        auto [x_m, y_new] = min_y_placement({x_1, x_2});
        if (y_new > y) {
          // Region is no longer at the position we thought it was, put it back
          // in the queue at its new position
          // Rcpp::Rcout << "  Re-queuing: \t" << y << " -> \t" << y_new << std::endl;
          queue_region(x_1, x_2, y_new);
        } else {
          place_dot(x_m, y);
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
SEXP compact_swarm_prog_(
  std::vector<Rcpp::NumericVector> xs, const double xsize, const double ysize, const int side
) {
  return compact_swarm_prog_2{xs, xsize, ysize, side}.place_dots();
}


//' Compact swarm layout
//' @param x <[numeric]> sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP compact_swarm_prog_old_(
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
    auto unplaced = std::deque<double>{};
    // we divide by xsize here so that all the distance calculations for checking
    // overlaps can be done in standardized units of 1 dot diameter, then we
    // multiply final positions by xsize and ysize before final output.
    for (const auto x_i : x) unplaced.emplace_back(x_i / xsize);
    auto queued = std::priority_queue<dot, std::vector<dot>, y_is_greater>{};
    queued.emplace(unplaced.front(), 0.0);
    unplaced.pop_front();

    // repeatedly look for the next dot to insert: lowest unplaced dot we encounter whose
    // y value was not changed by the most recent insert
    while (!queued.empty()) {
      auto candidate = queued.top();
      queued.pop();

      // compare this candidate to any existing dots within +/- 1 diameter, since
      // only these may overlap with it
      auto existing = may_overlap.lower_bound({candidate.x - 1, candidate.y});
      auto last = may_overlap.upper_bound({candidate.x + 1, candidate.y});
      auto min_y = candidate.y;
      while (existing != last) {
        if (existing->y < candidate.y - 1) {
          // `existing` is now out of range of any candidate dots (because
          // `candidate` is always the unplaced dot with the lowest y value), no
          // need to check it for overlaps again
          existing = may_overlap.erase(existing);
          continue;
        }

        auto new_min_y = std::sqrt(1 - sq(candidate.x - existing->x)) + existing->y;
        if (new_min_y > min_y) min_y = new_min_y;

        ++existing;
      }

      if (min_y > candidate.y) {
        // min y at which we can insert `candidate` is above where we were planning
        // to insert it, so we no longer can guarantee it is the lowest candidate;
        // therefore put it back in the queue so we can try another candidate.
        // Rcpp::Rcout << "  Re-queuing " << candidate.x << "\t" << min_y << std::endl;
        queued.emplace(candidate.x, min_y);
      } else {
        // Rcpp::Rcout << "PLACING " << candidate.x << "\t" << candidate.y << std::endl;
        may_overlap.insert(candidate);
        out_x_arr[i] = candidate.x * xsize;
        out_y_arr[i] = candidate.y * ysize;
        ++i;
        if (i % 1000 == 0) Rcpp::checkUserInterrupt();

        // after we insert, add the first dot at least 1 diameter before/after this one to the queue
        if (unplaced.size() == 1) {
          // Rcpp::Rcout << "  Queuing " << unplaced.front() << "\t" << min_y << std::endl;
          queued.emplace(unplaced.front(), min_y);
          unplaced.pop_front();
        } else if (unplaced.size() >= 2) {
          auto first_after = advance_to_at_least(unplaced, unplaced.cbegin(), candidate.x + 1);
          if (first_after == unplaced.cend()) --first_after;
          // Rcpp::Rcout << "  Queuing " << *first_after << "\t" << min_y << std::endl;
          queued.emplace(*first_after, min_y);
          unplaced.erase(first_after);

          auto first_before = advance_to_at_least(unplaced, unplaced.crbegin(), candidate.x - 1);
          if (first_before == unplaced.crend()) --first_before;
          // Rcpp::Rcout << "  Queuing " << *first_before << "\t" << min_y << std::endl;
          queued.emplace(*first_before, min_y);
          erase_(unplaced, first_before);
        }
      }
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = out_x_vec,
    Rcpp::Named("y") = out_y_vec
  );
}



/// Update a candidate's min y position it can be placed at
/// and return the existing dot just touching this candidate
/// (such that it causes the candidate to be placed at its y position)
auto update_candidate_y(dot& candidate, std::set<dot, x_is_less>& may_overlap, const double overlap_min_y) {
  // compare this candidate to any existing dots within +/- 1 diameter, since
  // only these may overlap with it
  auto existing = may_overlap.lower_bound({candidate.x - 1, candidate.y});
  const auto last_existing = may_overlap.upper_bound({candidate.x + 1, candidate.y});
  auto touching = existing;
  while (existing != last_existing) {
    if (existing->y < overlap_min_y) {
      // `existing` is now out of range of any candidate dots, no
      // need to check it for overlaps again
      existing = may_overlap.erase(existing);
      continue;
    }

    const auto new_min_y = std::sqrt(1 - sq(candidate.x - existing->x)) + existing->y;
    if (new_min_y >= candidate.y) {
      candidate.y = new_min_y;
      touching = existing;
    }

    ++existing;
  }
  return touching;
}

//' Alternative compact swarm layout
//' @param x <[numeric]> sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP compact_swarm_2_(
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
    auto unplaced = std::deque<dot>{};
    // we divide by xsize here so that all the distance calculations for checking
    // overlaps can be done in standardized units of 1 dot diameter, then we
    // multiply final positions by xsize and ysize before final output.
    for (const auto x_i : x) unplaced.emplace_back(x_i / xsize, 0);

    // repeatedly look for the next dot to insert: lowest unplaced dot we encounter whose
    // y value was not changed by the most recent insert
    auto overlap_min_y = 0;  // min y value of a placed dot that could overlap an unplaced one
    while (!unplaced.empty()) {
      auto candidate = unplaced.begin();
      auto new_overlap_min_y = INF;
      auto touching = update_candidate_y(*candidate, may_overlap, overlap_min_y);
      while (candidate != unplaced.end()) {
        Rcpp::Rcout << "Cand " << (candidate - unplaced.begin()) << " at\t" << candidate->x << "\t" << candidate->y << std::endl;
        auto next_candidate = candidate + 1;
        if (touching != may_overlap.end() && touching->x > candidate->x) {
          // if `candidate`'s y placement position is because of a dot it is touching
          // that is after it, we know that the closest next possible candidate is
          // the same distance on the other side of `touching` (as anything between
          // `candidate` and that point would be placed higher than `candidate`).
          next_candidate = std::lower_bound(
            next_candidate,
            unplaced.end(),
            dot{candidate->x + 2 * (touching->x - candidate->x), INF},
            x_is_less{}
          );
          Rcpp::Rcout << "    " << "Touches, skipping " << (next_candidate - candidate) << std::endl;
        }
        auto next_touching = touching;
        auto can_place = true;
        while (next_candidate != unplaced.end()) {
          next_touching = update_candidate_y(*next_candidate, may_overlap, overlap_min_y);
          Rcpp::Rcout << "     VS " << (next_candidate - unplaced.begin()) << " at\t" << next_candidate->x << "\t" << next_candidate->y << std::endl;
          if (squared_dist(*candidate, *next_candidate) > 1) break;
          if (next_candidate->y < candidate->y) {
            // found a candidate after this one that overlaps it and which is lower than it
            can_place = false;
            break;
          }
          ++next_candidate;
        }

        if (can_place) {
          Rcpp::Rcout << "     PLACING" << std::endl;
          new_overlap_min_y = std::min(new_overlap_min_y, candidate->y);
          may_overlap.insert(*candidate);
          out_x_arr[i] = candidate->x * xsize;
          out_y_arr[i] = candidate->y * ysize;
          ++i;
          // if (i % 1000 == 0)

          const auto next_candidate_dist = next_candidate - candidate - 1_z;
          candidate = unplaced.erase(candidate) + next_candidate_dist;
        } else {
          Rcpp::Rcout << "     CANT PLACE" << std::endl;
          candidate = next_candidate;
        }
        touching = next_touching;
        Rcpp::checkUserInterrupt();
      }
      Rcpp::Rcout << "FRONTIER:" << std::endl;
      for (const auto d : may_overlap) {
        Rcpp::Rcout << "         " << d.x << "\t" << d.y << std::endl;
      }
      overlap_min_y = new_overlap_min_y;
    }
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = out_x_vec,
    Rcpp::Named("y") = out_y_vec
  );
}


namespace {

template<typename C, typename It>
inline auto index(C& container, It& it) {
  if constexpr (std::is_base_of_v<typename C::reverse_iterator, It>) {
    return it.base() - container.begin() - 1;
  } else {
    return it - container.begin();
  }
}

/// Place candidates for stratified swarm with infinite grid
template<bool reverse>
inline auto place_candidates(
  std::deque<dot>& unplaced,
  std::set<dot, x_is_less>& may_overlap,
  double& min_y,
  double*& out_x_arr,
  double*& out_y_arr,
  std::ptrdiff_t& i,
  const double xsize,
  const double ysize
) -> bool {
  if (unplaced.empty()) return false;
  auto candidate = begin_<reverse>(unplaced);
  auto touching = update_candidate_y(*candidate, may_overlap, min_y);
  auto max_y = candidate->y;
  auto new_min_y = candidate->y;
  // Rcpp::Rcout << "min(y): " << min_y << std::endl;
  // Rcpp::Rcout << (reverse ? "Search in reverse" : "Search forward") << std::endl;
  while (candidate != end_<reverse>(unplaced)) {
    // Rcpp::Rcout << "  Cand " << index(unplaced, candidate) << " at\t" << candidate->x << "\t" << candidate->y << std::endl;
    auto next_candidate_x = candidate->x + negate_if<reverse>(1); //negate_if<reverse>(std::sqrt(1 - sq(candidate->y - min_y)));
    // if (touching != may_overlap.end() && touching->x > candidate->x) {
    //   // if `candidate`'s y placement position is because of a dot it is touching
    //   // that is after it, we know that the closest next possible candidate is
    //   // the same distance on the other side of `touching` (as anything between
    //   // `candidate` and that point would be placed higher than `candidate`).
    //   const auto x_after_touching = candidate->x + 2 * (touching->x - candidate->x);
    //   if (std::isnan(next_candidate_x) || x_after_touching > next_candidate_x) {
        // Rcpp::Rcout << "    " << "TOUCHES " << next_candidate_x << " -> " << x_after_touching << std::endl;
    //     next_candidate_x = x_after_touching;
    //   }
    // }
    // Rcpp::Rcout << "     " << "Skipping to " << next_candidate_x << std::endl;
    auto next_candidate = advance_to_at_least(
      unplaced,
      candidate + 1,
      dot{next_candidate_x, 0},
      x_is_less{}
    );
    // Rcpp::Rcout << "     " << "Skipping " << (next_candidate - candidate) << std::endl;

    while (next_candidate != end_<reverse>(unplaced)) {
      touching = update_candidate_y(*next_candidate, may_overlap, min_y);
      // Rcpp::Rcout << "     Possible next: " << index(unplaced, next_candidate) << " at\t" << next_candidate->x << "\t" << next_candidate->y << std::endl;
      if (
        squared_dist(*candidate, *next_candidate) >= 1
        // && min_y <= next_candidate->y
        // && next_candidate->y <= max_y
      ) {
        break;
      }
      ++next_candidate;
    }

    // Rcpp::Rcout << "  PLACING" << std::endl;
    new_min_y = std::min(new_min_y, candidate->y);
    may_overlap.insert(*candidate);
    out_x_arr[i] = candidate->x * xsize;
    out_y_arr[i] = candidate->y * ysize;
    ++i;
    // if (i % 1000 == 0)
    Rcpp::checkUserInterrupt();

    const auto next_candidate_dist = next_candidate - candidate - 1_z;
    candidate = erase_(unplaced, candidate) + next_candidate_dist;
  }
  // Rcpp::Rcout << "FRONTIER:" << std::endl;
  // for (const auto d : may_overlap) {
    // Rcpp::Rcout << "  " << d.x << "\t" << d.y << std::endl;
  // }
  min_y = new_min_y;
  return true;
}

}

//' Alternative compact swarm layout
//' @param x <[numeric]> sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP compact_swarm_grid_(
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
    auto unplaced = std::deque<dot>{};
    // we divide by xsize here so that all the distance calculations for checking
    // overlaps can be done in standardized units of 1 dot diameter, then we
    // multiply final positions by xsize and ysize before final output.
    for (const auto x_i : x) unplaced.emplace_back(x_i / xsize, 0);

    // repeatedly look for the next dot to insert: lowest unplaced dot we encounter whose
    // y value was not changed by the most recent insert
    auto min_y = 0.0;  // min y value of next placed dot
    while (
      place_candidates<false>(unplaced, may_overlap, min_y, out_x_arr, out_y_arr, i, xsize, ysize) &&
      place_candidates<true>(unplaced, may_overlap, min_y, out_x_arr, out_y_arr, i, xsize, ysize)
    );
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = out_x_vec,
    Rcpp::Named("y") = out_y_vec
  );
}
