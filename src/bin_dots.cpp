#include <Rcpp.h>
#include <float.h> // for DBL_EPSILON
#include <cmath>
#include <vector>
using namespace Rcpp;

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

// [[Rcpp::export(rng = false)]]
bool can_place_candidate_(
  const double candidate,
  const double last_placed,
  std::vector<std::vector<double>> last_rows,
  const int y_grid,
  const double xsize,
  const bool reverse
) {
  if (reverse) {
    if (candidate > last_placed - xsize) return false;
  } else {
    if (candidate < last_placed + xsize) return false;
  }
  // Rcpp::Rcout << "y_grid: " << y_grid << "\n";
  for (int i = 0; i < y_grid - 1; i++) {
    std::vector<double>& last_row_vec = last_rows[i];
    const auto n = last_row_vec.size();
    if (n == 0) continue;
    
    const double y_offset = (double(i) + 1.0) / double(y_grid);
    const double min_x_dist = std::sqrt(1 - y_offset * y_offset) * xsize;

    double *last_row = last_row_vec.data();
    int mflag = 0; // -1 if < all, 0 if inside, +1 if >= all
    const auto last_val_lte_candidate_idx = findInterval(
      last_row,
      n,
      candidate,
      /*rightmost_closed = */ FALSE,
      /*all_inside = */ FALSE,
      /*ilo = */ 0,
      /*mflag = */ &mflag
    );
    
    if (mflag >= 0) {
      double last_val_lte_candidate = last_row[last_val_lte_candidate_idx - 1];
      if (candidate < last_val_lte_candidate + min_x_dist) return false;
    }
    if (mflag <= 0) {
      double first_val_gt_candidate = last_row[last_val_lte_candidate_idx];
      if (candidate > first_val_gt_candidate - min_x_dist) return false;
    }
  }
  return true;
}
