#include "util.hpp"

#include <Rcpp.h>
#include <Rinternals.h>

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
