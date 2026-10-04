#include "util.h"
#include "layout_grid_swarm.h"

#include <Rcpp.h>

// grid swarm layout --------------------------------------------------------------

//' Fractional grid swarm layout
//' @param xs_list <list of [numeric]> list of vectors of sorted x values
//' @param xsize <positive scalar [numeric]> horizontal spacing between dots
//' @param ysize <positive scalar [numeric]> vertical spacing between dots
//' @param signed_side <scalar [integer]> which side to place dots on?
//' -  `0` = both
//' -  `1` = above
//' - `-1` = below
//' @param strata <scalar [numeric]> size of the y grid (corresponding to 1 + the number of adjacent
//' rows above or below this row that could overlap with dots in this row).
//' @returns <[data.frame]> data frame with columns x and y giving the new positions
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP grid_swarm_(
  const std::vector<Rcpp::NumericVector>& xs_list,
  const double xsize,
  const double ysize,
  const int signed_side,
  const std::ptrdiff_t strata
) {
  auto [x, y] = GridSwarm<Rcpp::NumericVector>{
    xs_list,
    xsize,
    ysize,
    signed_side,
    strata,
    Rcpp::checkUserInterrupt
  }.place_dots();

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = x,
    Rcpp::Named("y") = y
  );
}

// swarm cluster recentering ------------------------------------------------------------

//' Re-center contiguous clusters around their mean y position so that
//' small clusters are visually centered (rather than e.g. a cluster of
//' two points having one point on the origin line and one above it)
//' @param x sorted numeric vector of dot positions
//' @param y sorted numeric vector of dot heights, same length as x
//' @returns modified `y`
//' @noRd
// [[Rcpp::export(rng = false)]]
SEXP recenter_swarm_clusters_(
  const Rcpp::NumericVector x_vec,
  const Rcpp::NumericVector y_vec,
  const double binwidth
) {
  const auto n = ssize_(x_vec);

  const auto* x = x_vec.cbegin();
  auto y_out = Rcpp::clone(y_vec);
  auto* y = y_out.begin();

  auto bin_sum = 0.0;
  auto bin_start = 0_z;
  for (auto bin_end = 1_z; bin_end <= n; ++bin_end) {
    bin_sum += y[bin_end - 1_z];
    if (bin_end == n || x[bin_end] - x[bin_end - 1_z] >= binwidth) {
      const auto mean = bin_sum / static_cast<double>(bin_end - bin_start);
      for (auto i = bin_start; i < bin_end; ++i) y[i] -= mean;
      bin_start = bin_end;
      bin_sum = 0.0;
    }
  }
  return y_out;
}
