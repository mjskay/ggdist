#include <Rcpp.h>
#include <Rinternals.h>

#include <algorithm>
#include <cmath>
#include <deque>
#include <limits>
#include <vector>

// constants --------------------------------------------------------------------------

constexpr auto INF = std::numeric_limits<double>::infinity();

template<typename Numeric>
constexpr auto relative_eps(const Numeric x) -> Numeric {
  return 8 * x * std::numeric_limits<Numeric>::epsilon();
}

// literals ---------------------------------------------------------------------------

//' Size diff literal for C++ arrays / vectors
//' @noRd
constexpr std::ptrdiff_t operator""_z(unsigned long long n) {
  return n;
}

//' Size literal for C++ arrays / vectors
//' @noRd
constexpr std::size_t operator""_uz(unsigned long long n) {
  return n;
}

//' Size literal for R vectors
//' @noRd
constexpr R_xlen_t operator""_rz(unsigned long long n) {
  return n;
}

// helpers ---------------------------------------------------------------------------

//' Signed size of a container (from C++20)
//' @noRd
template<class C>
constexpr auto ssize(const C& c) -> std::common_type_t<std::ptrdiff_t, std::make_signed_t<decltype(c.size())>> {
    using signed_c_size_t = std::common_type_t<std::ptrdiff_t, std::make_signed_t<decltype(c.size())>>;
    return static_cast<signed_c_size_t>(c.size());
}


// wilkinson-esque methods ------------------------------------------------------------

// [[Rcpp::export(rng = false)]]
Rcpp::IntegerVector wilkinson_bin_to_right_(const Rcpp::NumericVector& x, const double width) {
  const auto n = x.size();
  const auto eps = relative_eps(width);

  auto bins = Rcpp::IntegerVector(n);
  auto current_bin = 1_rz;
  auto first_x = x[0];

  bins[0] = 1;
  for (auto i = 1_rz; i < n; ++i) {
    // This is equivalent to x[i] - first_x >= width but it accounts for machine precision.
    // If we instead used `>=` directly some things that should be symmetric will not be
    if (x[i] - first_x - width >= -eps) {
      current_bin = current_bin + 1_rz;
      first_x = x[i];
    }
    bins[i] = current_bin;
  }

  return bins;
}

// grid_swarm ------------------------------------------------------------

//' Attempt to place a candidate dot in a target row
//' @param candidate candidate x position
//' @param xsize horizontal spacing between dots
//' @param ygrid size of the y grid (corresponding to 1 + the number of adjacent rows above or
//' below this row that could overlap with dots in this row)
//' @param rows rows of already-placed dots
//' @param target_row iterator pointing at row in `rows` to attempt to place `candidate` in
//' @returns `true` if the candidate was placed successfully
//' @noRd
template<typename Row>
inline auto place_candidate(
  const double candidate,
  const double xsize,
  const std::ptrdiff_t ygrid,
  std::vector<Row>& rows,
  const std::ptrdiff_t target_row_i
) -> bool {
  const auto eps = relative_eps(xsize);

  auto& target_row = rows[target_row_i];
  auto insert_loc = target_row.end();

  // check +/- (ygrid - 1) rows from target_row to see if the candidate is overlapping an existing dot
  const auto first_row_i = std::max(0_z, target_row_i - (ygrid - 1_z));
  const auto last_row_i = std::min(ssize(rows), target_row_i + ygrid);
  // iterate in reverse because we will often have a quick exit by comparison to the
  // most recently placed dot
  for (auto i = last_row_i; i-- > first_row_i; ) {
    const auto& row = rows[i];
    if (row.size() == 0_uz) continue;

    const auto rows_from_target = static_cast<double>(std::abs(i - target_row_i));
    const auto y_offset = rows_from_target / static_cast<double>(ygrid);
    const auto min_x_dist = std::sqrt(1 - y_offset * y_offset) * (xsize - eps);

    auto loc = row.upper_bound(candidate);
    if (loc != row.end()) {
      const auto min_val_gt_candidate = *loc;
      if (candidate > min_val_gt_candidate - min_x_dist) return false;
    }
    if (loc != row.begin()) {
      const auto max_val_lte_candidate = *std::prev(loc);
      if (candidate < max_val_lte_candidate + min_x_dist) return false;
    }

    // if this is the target row we save insert_loc so we can give a hint to
    // speed up the call to target_row.insert() below
    if (rows_from_target == 0) insert_loc = loc;
  }

  target_row.insert(insert_loc, candidate);
  return true;
}

//' const begin iterator for forward or reverse s
//' @param reverse iterate in reverse?
//' @param T iterable type
//' @param vec object to iterate over
//' @noRd
template<bool reverse, typename C>
inline auto cbegin(const C& container) {
  if constexpr (reverse) {
    return container.crbegin();
  } else {
    return container.cbegin();
  }
}

//' const end iterator for forward or reverse iteration
//' @param reverse iterate in reverse?
//' @param C iterable type
//' @param container object to iterate over
//' @noRd
template<bool reverse, typename C>
inline auto cend(const C& container) {
  if constexpr (reverse) {
    return container.crend();
  } else {
    return container.cend();
  }
}

//' push onto front or back of a container
//' @param front push onto front?
//' @param C container type
//' @param container object to push onto
//' @noRd
template<bool front, typename C, typename V>
inline void push(C& container, V&& value) {
  if constexpr (front) {
    container.push_front(std::forward<V>(value));
  } else {
    container.push_back(std::forward<V>(value));
  }
}

//' Attempt to place dots in a specific row in the grid_swarm algorithm
//' @param reverse are we placing dots in reverse order?
//' @param both is this a mirrored layout (`side == "both"`?)
//' @param candidates dots to be placed
//' @param next_candidates swap space used for next `candidates`
//' @param xsize horizontal spacing between dots
//' @param ygrid size of the y grid (corresponding to 1 + the number of adjacent rows above or
//' below this row that could overlap with dots in this row)
//' @param rows rows of already-placed dots
//' @param rows_bottom bottom rows of already-placed dots (when `both == true`)
//' @param row_i index of `rows` and `rows_bottom` to place candidates in.
//' @returns `true` if `remaining` may still have dots to place and `false` otherwise
//' @noRd
template<bool reverse, typename Row>
inline auto place_row(
  const bool both,
  std::deque<double>& candidates,
  std::deque<double>& next_candidates,
  const double xsize,
  const std::ptrdiff_t ygrid,
  std::vector<Row>& rows,
  std::vector<Row>& rows_bottom,
  std::ptrdiff_t& row_i
) -> bool {
  if (candidates.empty()) return false;

  // ensure target row exists
  if (row_i == static_cast<ptrdiff_t>(rows.size())) {
    rows.emplace_back();
    if (both) rows_bottom.emplace_back();
  }

  // place candidates
  next_candidates.clear();
  for (auto it = cbegin<reverse>(candidates); it != cend<reverse>(candidates); ++it) {
    const auto candidate = *it;
    if (place_candidate(candidate, xsize, ygrid, rows, row_i)) {
      continue;
    } else if (both && place_candidate(candidate, xsize, ygrid, rows_bottom, row_i)) {
      continue;
    }
    push<reverse>(next_candidates, candidate);
  }
  std::swap(candidates, next_candidates);

  ++row_i;
  return true;
}

//' Place dots in `n` rows in the grid_swarm algorithm, alternating `reverse`
//' See `place_row()`
//' @returns `true` if `remaining` may still have dots to place and `false` otherwise
//' @noRd
template<bool reverse, typename Row>
inline auto place_rows(
  std::size_t n,
  const bool both,
  std::deque<double>& candidates,
  std::deque<double>& next_candidates,
  const double xsize,
  const std::ptrdiff_t ygrid,
  std::vector<Row>& rows,
  std::vector<Row>& rows_bottom,
  std::ptrdiff_t& row_i
) -> bool {
  auto any_left = true;
  while (
    n-- > 0_uz &&
    (any_left = place_row<reverse>(both, candidates, next_candidates, xsize, ygrid, rows, rows_bottom, row_i)) &&
    n-- > 0_uz &&
    (any_left = place_row<!reverse>(both, candidates, next_candidates, xsize, ygrid, rows, rows_bottom, row_i))
  );
  return any_left;
}

//' Fractional grid swarm layout
//' @param x <[numeric]> sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP grid_swarm_(
  std::vector<std::deque<double>> xs,
  const double xsize,
  const double ysize,
  const std::ptrdiff_t ygrid,
  const int side
) {
  auto n_out = 0_uz;
  for (const auto& x : xs) n_out += x.size();
  const auto both = side == 0;

  // swap space used for unplaced candidates
  auto next_candidates = std::deque<double>{};

  using Row = std::multiset<double>;
  auto rows = std::vector<Row>{{}};
  auto rows_bottom = std::vector<Row>{{}};

  for (auto& candidates : xs) {
    // first row is special: when both == true, it is a "middle" row that is
    // treated as the first row (for placement purposes) on both the top and bottom sides
    // so we always treat it as both = false and just copy it to rows_bottom
    auto row_i = 0_z;
    place_row<false>(false, candidates, next_candidates, xsize, ygrid, rows, rows_bottom, row_i);
    if (both) rows_bottom[0] = rows[0];

    // place dots in rows, alternating direction (but also ensuring every ygrid-th row alternates)
    while (
      // start with <true>(ygrid - 1, ...) instead of <false>(ygrid, ...) because
      // we already placed the first row above
      place_rows<true>(ygrid - 1, both, candidates, next_candidates, xsize, ygrid, rows, rows_bottom, row_i) &&
      place_rows<true>(ygrid, both, candidates, next_candidates, xsize, ygrid, rows, rows_bottom, row_i) &&
      place_row<false>(both, candidates, next_candidates, xsize, ygrid, rows, rows_bottom, row_i)
    );
  }

  // construct output data frame
  auto out_x_vec = Rcpp::NumericVector(n_out);
  auto out_y_vec = Rcpp::NumericVector(n_out);
  auto out_x_arr = REAL(out_x_vec);
  auto out_y_arr = REAL(out_y_vec);
  auto i = 0_uz;
  const auto copy_rows_to_output = [&i, &out_x_arr, &out_y_arr, ygrid, ysize](
    const std::vector<Row>& rows,
    const std::size_t row_start,
    const double side
  ) {
    for (auto row_i = row_start; row_i < rows.size(); ++row_i) {
      const auto& row = rows[row_i];
      for (const auto x_val : row) {
        out_x_arr[i] = x_val;
        out_y_arr[i] = double(row_i) / double(ygrid) * ysize * side;
        ++i;
      }
    }
  };
  copy_rows_to_output(rows, 0_uz, both ? 1.0 : double(side));
  if (both) copy_rows_to_output(rows_bottom, 1_uz, -1.0);

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = out_x_vec,
    Rcpp::Named("y") = out_y_vec
  );
}

//' Re-center contiguous clusters around their mean y position so that
//' small clusters are visually centered (rather than e.g. a cluster of
//' two points having one point on the origin line and one above it)
//' @param x sorted numeric vector of dot positions
//' @param y sorted numeric vector of dot heights, same length as x
//' @returns modified `y`
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP recenter_swarm_clusters_(
  Rcpp::NumericVector& x_vec,
  Rcpp::NumericVector& y_vec,
  const double binwidth
) {
  auto n = static_cast<std::size_t>(x_vec.size());
  auto x = REAL(x_vec);
  auto y = REAL(y_vec);
  auto bin_sum = 0.0;
  auto bin_start = 0_uz;
  for (auto bin_end = 1_uz; bin_end <= n; ++bin_end) {
    bin_sum += y[bin_end - 1_uz];
    if (bin_end == n || x[bin_end] - x[bin_end - 1_uz] >= binwidth) {
      auto mean = bin_sum / static_cast<double>(bin_end - bin_start);
      for (auto i = bin_start; i < bin_end; ++i) y[i] -= mean;
      bin_start = bin_end;
      bin_sum = 0.0;
    }
  }
  return y_vec;
}
