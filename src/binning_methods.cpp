#include <Rcpp.h>
#include <float.h> // for DBL_EPSILON
using namespace Rcpp;

// [[Rcpp::export]]
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
