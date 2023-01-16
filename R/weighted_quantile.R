# weighted quantile estimators
#
# Author: mjskay
###############################################################################


#' Weighted sample quantiles
#'
#' A variation of [quantile()] that can be applied to weighted samples.
#'
#' @param x numeric vector: sample values
#' @param probs numeric vector: probabilities in \eqn{[0, 1]}
#' @param weights Weights for the sample. One of:
#'  - numeric vector of same length as `x`: weights for corresponding values in `x`,
#'    which will be normalized to sum to 1.
#'  - `NULL`: indicates no weights are provided, so unweighted sample quantiles
#'    (equivalent to [quantile()]) are returned.
#' @param n Presumed effective sample size. If this is greater than 1 and
#' continuous quantiles (`type >= 4`) are requested, flat regions may be added
#' to the approximation to the inverse CDF in areas where the normalized
#' weight exceeds `1/n` (i.e., regions of high density). This can be used to
#' ensure that if a sample of size `n` with duplicate `x` values is summarized
#' into a weighted sample without duplicates, the result of `weighted_quantile(..., n = n)`
#' on the weighted sample is equal to the result of `quantile()` on the original
#' sample. One of:
#'  - `NULL`: do not make a sample size adjustment.
#'  - numeric: presumed effective sample size.
#'  - function or name of function (as a string): A function applied to
#'    `weights` (prior to normalization) to determine the sample size. Some
#'    useful values may be:
#'     - `"length"`: i.e. use the number of elements in `weights` (equivalently
#'       in `x`) as the effective sample size.
#'     - `"sum"`: i.e. use the sum of the unnormalized `weights` as the sample
#'       size. Useful if the provided `weights` is unnormalized so that its
#'       sum represents the true sample size.
#' @param na.rm logical: if `TRUE`, corresponding entries in `x` and `weights`
#' are removed if either is `NA`.
#' @param type integer between 1 and 9: determines the type of quantile estimator
#' to be used. Types 1 to 3 are for discontinuous quantiles, types 4 to 9 are
#' for continuous quantiles. See **Details**.
#'
#' @details
#' Calculates weighted quantiles using a variation of the quantile types based
#' on a generalization of [quantile()].
#'
#' Type 1--3 (discontinuous) quantiles are directly a function of the inverse
#' CDF as a step function, and so can be directly translated to the weighted
#' case using the natural definition of the weighted ECDF as the cumulative
#' sum of the normalized weights.
#'
#' Type 4--9 (continuous) quantiles require some translation from the definitions
#' in [quantile()]. [quantile()] defines continuous estimators in terms of
#' \eqn{x_k}, which is the \eqn{k}th order statistic, and \eqn{p_k}, which is a function of \eqn{k}
#' and \eqn{n} (the sample size). In the weighted case, we instead take \eqn{x_k} as the \eqn{k}th
#' smallest value of \eqn{x} in the weighted sample (not necessarily an order statistic,
#' because of the weights). Then we can re-write the formulas for \eqn{p_k} in terms of
#' \eqn{F(x_k)} (the empirical CDF at \eqn{x_k}, i.e. the cumulative sum of normalized
#' weights) and \eqn{f(x_k)} (the normalized weight at \eqn{x_k}), by using the
#' fact that, in the unweighted case, \eqn{k = F(x_k) \cdot n} and \eqn{1/n = f(x_k)}:
#'
#' \describe{
#'   \item{Type 4}{\eqn{p_k = \frac{k}{n} = F(x_k)}}
#'   \item{Type 5}{\eqn{p_k = \frac{k - 0.5}{n} = F(x_k) - \frac{f(x_k)}{2}}}
#'   \item{Type 6}{\eqn{p_k = \frac{k}{n + 1} = \frac{F(x_k)}{1 + f(x_k)}}}
#'   \item{Type 7}{\eqn{p_k = \frac{k - 1}{n - 1} = \frac{F(x_k) - f(x_k)}{1 - f(x_k)}}}
#'   \item{Type 8}{\eqn{p_k = \frac{k - 1/3}{n + 1/3} = \frac{F(x_k) - f(x_k)/3}{1 + f(x_k)/3}}}
#'   \item{Type 9}{\eqn{p_k = \frac{k - 3/8}{n + 1/4} = \frac{F(x_k) - f(x_k) \cdot 3/8}{1 + f(x_k)/4}}}
#' }
#'
#' Then the quantile function (inverse CDF) is the piece-wise linear function
#' defined by the points \eqn{(p_k, x_k)}.
#'
#' @returns
#' `weighted_quantile()` returns a numeric vector of `length(probs)` with the
#' estimate of the corresponding quantile from `probs`.
#'
#' `weighted_quantile_fun()` returns a function that takes a single argument,
#' a vector of probabilities, which itself returns the corresponding quantile
#' estimates. It may be useful when `weighted_quantile()` needs to be called
#' repeatedly for the same sample, re-using some pre-computation.
#' @importFrom stats stepfun approxfun
#' @export
weighted_quantile = function(x, probs = seq(0, 1, 0.25), weights = NULL, n = NULL, na.rm = FALSE, type = 7) {
  weighted_quantile_fun(x, weights = weights, n = n, na.rm = na.rm, type = type)(probs)
}

#' @rdname weighted_quantile
#' @export
weighted_quantile_fun = function(x, weights = NULL, n = NULL, na.rm = FALSE, type = 7) {
  weights = weights %||% rep(1, length(x))
  if (!type %in% 1:9) {
    stop0("Quantile type `", deparse0(type), "` is invalid. It must be in 1:9.")
  }
  if (isTRUE(na.rm)) {
    keep = !is.na(x) & !is.na(weights)
    x = x[keep]
    weights = weights[keep]
  }

  # determine effective sample size
  if (is.character(n)) {
    n = match_function(n)
  }
  if (is.function(n)) {
    n = n(weights)
  }

  # drop 0 weights
  non_zero = weights != 0
  x = x[non_zero]
  weights = weights[non_zero]

  # if there is only 0 or 1 x values, we don't need the weighted version (and
  # we couldn't calculate it anyway as we need > 2 points for the interpolation)
  if (length(x) <= 1) {
    return(function(p) quantile(x, p, names = FALSE))
  }

  # normalize weights (must be done after calculating n, as n is calculated
  # in terms of unnormalized weights)
  weights = weights / sum(weights)

  # sort values
  x_order = order(x)
  x = x[x_order]
  weights = weights[x_order]

  if (is.null(n)) {
    f_x = weights
  } else {
    # find the (whole) number of points in a sample of size n corresponding
    # to each point in the weighted sample (rounding up). This is the number of
    # replicates of that point we will use to represent that point to create
    # flat regions in the approximate inverse CDF.
    n_rep = ceiling(weights * n)
    x = rep.int(x, n_rep)
    f_x = rep.int(weights / n_rep, n_rep)
  }

  # calculate the weighted CDF
  F_x = cumsum(f_x)

  # generate the function for the approximate inverse CDF
  if (1 <= type && type <= 3) {
    # discontinuous quantiles
    switch(type,
      # type 1
      stepfun(F_x, c(x, x[length(x)]), right = TRUE),
      # type 2
      {
        x_over_2 = c(x, x[length(x)])/2
        inverse_cdf_type2_left = stepfun(F_x, x_over_2, right = FALSE)
        inverse_cdf_type2_right = stepfun(F_x, x_over_2, right = TRUE)
        function(x) inverse_cdf_type2_left(x) + inverse_cdf_type2_right(x)
      },
      # type 3
      stepfun(F_x - f_x/2, c(x[[1]], x), right = TRUE)
    )
  } else {
    # Continuous quantiles. These are based on the definition of p_k as described
    # in the documentation of `quantile()`. The trick to re-writing those formulas
    # (which use `n` and `k`) for the weighted case is that `k` = `F_x * n` and
    # `1/n` = `f_x`. Using these two facts, we can express the formulas for
    # `p_k` without using `n` or `k`, which don't really apply in the weighted case.
    p_k = switch(type - 3,
      # type 4
      F_x,
      # type 5
      F_x - f_x/2,
      # type 6
      F_x / (1 + f_x),
      # type 7
      (F_x - f_x) / (1 - f_x),
      # type 8
      (F_x - f_x/3) / (1 + f_x/3),
      # type 9
      (F_x - f_x*3/8) / (1 + f_x/4)
    )
    approxfun(p_k, x, rule = 2, ties = "ordered")
  }
}
