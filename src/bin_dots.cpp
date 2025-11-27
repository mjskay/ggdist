#include <Rcpp.h>
#include <Rinternals.h>
#include <algorithm>
#include <float.h> // for DBL_EPSILON
#include <cmath>
#include <vector>
using namespace Rcpp;

// wilkinson-esque methods ------------------------------------------------------------

// [[Rcpp::export(rng = false)]]
IntegerVector wilkinson_bin_to_right_(const NumericVector& x, double width) {
  int n = x.size();
  IntegerVector bins(n);
  bins[0] = 1;
  int current_bin = 1;
  double first_x = x[0];

  for (int i = 1; i < n; i++) {
    // This is equivalent to x[i] - first_x >= width but it accounts for machine precision.
    // If we instead used `>=` directly some things that should be symmetric will not be
    if (x[i] - first_x - width >= -DBL_EPSILON) {
      current_bin = current_bin + 1;
      first_x = x[i];
    }
    bins[i] = current_bin;
  }

  return bins;
}

// weave_swarm ------------------------------------------------------------

//' Can we place `candidate` at this position given the last placed dot and
//' the previous rows of dots placed so far?
//' @param candidate <scalar [numeric]> candidate x position
//' @param last_placed <scalar [numeric]> last placed x position in this row
//' @param rows <[list] of [numeric]> list of previous rows of placed dots
//' @param n_rows_back <scalar [integer]> actual number of previous rows to consider
//' @param y_grid <scalar [integer]> max possible number of previous rows in the
//' y grid that  could overlap with this candidate
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param reverse <scalar [logical]> are we placing dots in reverse order?
//' @returns <scalar [logical]> can we place candidate here?
//' @noRd
// [[Rcpp::export(rng = false)]]
bool can_place_candidate_(
  const double candidate,
  const double last_placed,
  std::vector<std::vector<double>>& rows,
  const int n_rows_back,
  const int y_grid,
  const double xsize,
  const bool reverse
) {
  if (reverse) {
    if (candidate > last_placed - xsize) return false;
  } else {
    if (candidate < last_placed + xsize) return false;
  }

  // for the n_rows_back previous rows, check if candidate is overlapping an existing dot
  const auto n_rows = rows.size();
  for (int i = 1; i <= n_rows_back; i++) {
    // last row is the current row being placed, so previous rows start at n_rows - i - 1
    std::vector<double>& prev_row_vec = rows[n_rows - i - 1];
    const auto n = prev_row_vec.size();
    if (n == 0) continue;
    
    const double y_offset = double(i) / double(y_grid);
    const double min_x_dist = std::sqrt(1 - y_offset * y_offset) * xsize;

    double *prev_row = prev_row_vec.data();
    int mflag = 0; // -1 if < all, 0 if inside, +1 if >= all
    const auto max_val_lte_candidate_idx = findInterval(
      prev_row,
      n,
      candidate,
      /*rightmost_closed = */ FALSE,
      /*all_inside = */ FALSE,
      /*ilo = */ 0,
      /*mflag = */ &mflag
    );
    
    if (mflag >= 0) {
      double max_val_lte_candidate = prev_row[max_val_lte_candidate_idx - 1];
      if (candidate < max_val_lte_candidate + min_x_dist) return false;
    }
    if (mflag <= 0) {
      double min_val_gt_candidate = prev_row[max_val_lte_candidate_idx];
      if (candidate > min_val_gt_candidate - min_x_dist) return false;
    }
  }
  return true;
}


//' Weave/swarm hybrid
//'
//' @param x <[numeric]> sorted x values
//' @param xsize <scalar [numeric]> horizontal spacing between dots
//' @param ysize <scalar [numeric]> vertical spacing between dots
//' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP weave_swarm_new_(
  std::vector<double> x,
  const double xsize,
  const double ysize,
  const int side
) {
  auto n_out = x.size();
  constexpr size_t y_grid = 4;

  const bool both = side == 0;
  std::vector<double> remaining_swap;
  std::vector<double>* remaining = &x;
  // as we place dots into rows, we will put unplaced dots into next_remaining
  // and then swap with remaining at the end of each row placement
  std::vector<double>* next_remaining = &remaining_swap;
  std::vector<std::vector<double>> rows;
  std::vector<std::vector<double>> rows_bottom;

  auto place_row = [y_grid, xsize, &remaining, &next_remaining, &rows, &rows_bottom](
    const bool reverse,
    const bool both
  ) {
    if (remaining->empty()) return;

    auto n_rows_back = std::min(y_grid, rows.size());

    std::vector<double>* row = &rows.emplace_back();
    std::vector<double>* row_bottom = both ? &rows_bottom.emplace_back() : nullptr;
    
    double last_placed = reverse ? INFINITY : -INFINITY;
    double last_placed_bottom = last_placed;

    next_remaining->clear();

    std::ptrdiff_t i, increment, end_index;
    if (reverse) {
      i = remaining->size() - 1;
      increment = -1;
      end_index = -1;
    } else {
      i = 0;
      increment = 1;
      end_index = remaining->size();
    }
    for (; i != end_index; i += increment) {
      double candidate = (*remaining)[i];
      if (can_place_candidate_(candidate, last_placed, rows, n_rows_back, y_grid, xsize, reverse)) {
        row->push_back(candidate);
        last_placed = candidate;
      } else if (both && can_place_candidate_(candidate, last_placed_bottom, rows_bottom, n_rows_back, y_grid, xsize, reverse)) {
        row_bottom->push_back(candidate);
        last_placed_bottom = candidate;
      } else {
        next_remaining->push_back(candidate);
      }
    }

    if (reverse) {
      std::reverse(row->begin(), row->end());
      if (both) std::reverse(row_bottom->begin(), row_bottom->end());
      std::reverse(next_remaining->begin(), next_remaining->end());
    }
    std::swap(remaining, next_remaining);
  };

  // first row is special: when both == true, it is a "middle" row that is
  // treated as the first row (for placement purposes) on both the top and bottom sides
  // so we always treat it as both = false and just copy it to rows_bottom 
  place_row(/*reverse = */ false, /*both = */ false);
  if (both) rows_bottom.push_back(rows.back());

  // place dots in rows, alternating direction every y_grid rows
  while (!remaining->empty()) {
    // start with y_grid - 1 because we already placed the first row above
    for (size_t i = 0; i < y_grid - 1; ++i) place_row(/*reverse = */ false, both);
    for (size_t i = 0; i < y_grid; ++i) place_row(/*reverse = */ true, both);
    place_row(/*reverse = */ false, both);
  }

  // construct output data frame
  NumericVector out_x_vec(n_out);
  NumericVector out_y_vec(n_out);
  double* out_x = REAL(out_x_vec);
  double* out_y = REAL(out_y_vec);
  std::ptrdiff_t i = 0;
  auto copy_rows_to_output = [&i, &out_x, &out_y, y_grid, ysize](
    const std::vector<std::vector<double>>& rows,
    std::ptrdiff_t start,
    const double side
  ) {
    for (size_t row_i = start; row_i < rows.size(); ++row_i) {
      const auto& row = rows[row_i];
      for (const auto& x_val : row) {
        out_x[i] = x_val;
        out_y[i] = double(row_i) / double(y_grid) * ysize * side;
        ++i;
      }
    }
  };
  copy_rows_to_output(rows, /*start=*/ 0, both ? 1 : side);
  if (both) copy_rows_to_output(rows_bottom, /*start=*/1, /*side=*/-1);

  return DataFrame::create(
    Named("x") = out_x_vec,
    Named("y") = out_y_vec
  );
}
