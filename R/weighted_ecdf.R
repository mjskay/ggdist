# Weighted ECDF
#
# Author: mjskay
###############################################################################


#' Weighted empirical cumulative distribution function
#'
#' A variation of [ecdf()] that can be applied to weighted samples.
#'
#' @param x numeric vector: sample values
#' @param weights Weights for the sample. One of:
#'  - numeric vector of same length as `x`: weights for corresponding values in `x`,
#'    which will be normalized to sum to 1.
#'  - `NULL`: indicates no weights are provided, so the unweighted empirical
#'    cumulative distribution function (equivalent to [ecdf()]) is returned.
#' @param na.rm logical: if `TRUE`, corresponding entries in `x` and `weights`
#' are removed if either is `NA`.
#'
#' @details
#' Generates a weighted empirical cumulative distribution function, \eqn{F(x)}.
#' Given \eqn{x}, a sorted vector (derived from `x`), and \eqn{w_i}, the corresponding
#' `weight` for \eqn{x_i}, \eqn{F(x)} is a step function with steps at each \eqn{x_i}
#' with \eqn{F(x_i)} equal to the sum of all weights up to and including \eqn{w_i}.
#'
#' @returns
#' `weighted_ecdf()` returns a function of class `"weighted_ecdf"`, which also
#' inherits from the [stepfun()] class. Thus, it also has `plot()` and `print()`
#' methods. Like [ecdf()], [weighted_ecdf()] also provides a [quantile()] method,
#' which dispatches to [weighted_quantile()].
#'
#' @seealso [weighted_quantile()]
#' @examples
#' weighted_ecdf(1:3, weights = 1:3)
#' plot(weighted_ecdf(1:3, weights = 1:3))
#' quantile(weighted_ecdf(1:3, weights = 1:3), 0.4)
#' @importFrom stats approxfun
#' @export
weighted_ecdf = function(x, weights = NULL, na.rm = FALSE) {
  x = check_na(x, na.rm)
  n = length(x)
  if (n < 1) cli_abort("Need at least 1 or more values to calculate an ECDF")

  weights = if (is.null(weights)) rep(1, n) else weights

  #sort only if necessary
  if (is.unsorted(x)) {
    sort_order = order(x)
    x = x[sort_order]
    weights = weights[sort_order]
  }

  # calculate weighted cumulative probabilities
  p = cumsum(weights)
  p = p/p[n]

  cdf = approxfun(x, p, yleft = 0, yright = 1, ties = "ordered", method = "constant")
  class(cdf) = c("weighted_ecdf", "stepfun", class(cdf))
  assign("weights", weights, envir = environment(cdf))
  attr(cdf, "call") = sys.call()
  cdf
}

#' @export
quantile.weighted_ecdf = function(x, ...) {
  weighted_quantile(environment(x)$x, weights = environment(x)$weights, ...)
}
