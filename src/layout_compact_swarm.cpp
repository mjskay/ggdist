#include "layout_compact_swarm.h"

#include <Rcpp.h>

//' Compact swarm layout
//' @param xs_list <list of [numeric]> list of vectors of sorted x values
//' @param xsize <positive scalar [numeric]> horizontal spacing between dots
//' @param ysize <positive scalar [numeric]> vertical spacing between dots
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
  auto [x, y] = CompactSwarm<Rcpp::NumericVector>{
    xs_list,
    xsize,
    ysize,
    signed_side,
    group_penalty,
    Rcpp::checkUserInterrupt
  }.place_dots();

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = x,
    Rcpp::Named("y") = y
  );
}
