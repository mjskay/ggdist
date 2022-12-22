# Weighted ECDF
#
# Author: mjskay
###############################################################################


#' @importFrom stats approxfun
weighted_ecdf = function(x, weights = NULL) {
  n = length(x)
  if (n < 1) stop("Need at least 1 or more values to calculate an ECDF")

  #sort x
  sort_order = order(x)
  x = x[sort_order]

  # calculate weighted cumulative probabilities
  weights = if (is.null(weights)) rep(1, n) else weights
  weights = weights[sort_order]
  p = cumsum(weights) / sum(weights)

  approxfun(x, p, yleft = 0, yright = 1, ties = "ordered", method = "constant")
}
