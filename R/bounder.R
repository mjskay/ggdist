# distribution bounds estimators
#
# Author: mjskay
###############################################################################



#' Estimate bounds of a distribution using the CDF of its order statistics
#'
#' Estimate the bounds of the distribution a sample came from using the CDF of
#' the order statistics of the sample. Use with the `bounder` argument to [density_bounded()].
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @param x numeric vector containing a sample to estimate the bounds of.
#' @param p scalar in \eqn{[0,1]}: percentile of the order statistic distribution to use
#'   as the estimate. `p = 1` will return `range(x)`; `p = 0.5` will give the median
#'   estimate, `p = 0` will give a very wide estimate (effectively treating the
#'   distribution as unbounded when used with [density_bounded()]).
#'
#' @details
#' [bounder_cdf()] uses the distribution of the order statistics of
#' \eqn{X} to estimate where the first and last order statistics (i.e. the
#' min and max) of this distribution would be, assuming the sample `x` is the
#' distribution. Then, it adjusts the boundary outwards from `min(x)` (or `max(x)`)
#' by the distance between `min(x)` (or `max(x)`) and the nearest estimated
#' order statistic.
#'
#' Taking \eqn{X} = `x`, the distributions of the first and last order statistics are:
#'
#' \deqn{\begin{array}{rcl}
#' F_{X_{(1)}}(x) &=& 1 - \left[1 - F_X(x)\right]^n\\
#' F_{X_{(n)}}(x) &=& F_X(x)^n
#' \end{array}}
#'
#' Re-arranging, we can get the inverse CDFs (quantile functions) of each
#' order statistic in terms of the quantile function of \eqn{X} (which we
#' can estimate from the data), giving us an estimate for the minimum
#' and maximum order statistic:
#'
#' \deqn{\begin{array}{rcrcl}
#' \hat{x_1} &=& F_{X_{(1)}}^{-1}(p) &=& F_X^{-1}\left[1 - (1 - p)^{1/n}\right]\\
#' \hat{x_n} &=& F_{X_{(n)}}^{-1}(p) &=& F_X^{-1}\left[p^{1/n}\right]
#' \end{array}}
#'
#' Then the estimated bounds are:
#'
#' \deqn{\left[2\min(x) - \hat{x_1}, 2\max(x) - \hat{x_n} \right]}
#'
#' These bounds depend on \eqn{p}, the percentile of the distribution of the order
#' statistic used to form the estimate. While \eqn{p = 0.5} (the median) might be
#' a reasonable choice (and gives results similar to [bounder_cooke()]), this tends
#' to be a bit too aggressive in "detecting" bounded distributions, especially in
#' small sample sizes. Thus, we use a default of \eqn{p = 0.01}, which tends to
#' be very conservative in small samples (in that it usually gives results
#' roughly equivalent to an unbounded distribution), but which still performs
#' well on bounded distributions when sample sizes are larger (in the thousands).
#'
#' @template returns-bounder
#' @seealso The `bounder` argument to [density_bounded()].
#' @family bounds estimators
#' @export
bounder_cdf = auto_partial(name = "bounder_cdf", function(x, p = 0.01) {
  # we use the distribution of the order statistic of a sample to estimate
  # where the first and last order statistics (i.e. the min and max) of this
  # distribution would be assuming the sample `x` is the distribution, then
  # we adjust the boundary outwards from min(x) (or max(x)) by the distance
  # between min(x) (or max(x)) and the nearest estimated order statistic.
  # We can use the fact that given a CDF of distribution F_X(x), the
  # distribution of its first and last order statistics are:
  #  F_X_1(x) = 1 - (1 - F_X(x))^n
  #  F_X_n(x) = F_X(x)^n
  # Re-arranging, we can get the inverse CDFs (quantile functions) of each
  # in terms of the quantile function of X:
  #  F_X_1^-1(p) = F_X^-1(1 - (1 - p)^(1/n))
  #  F_X_n^-1(p) = F_X^-1(p^(1/n))
  # e.g. by default (when p = 0.5) we can use the median of these distributions
  # as our estimates by
  #  X_1_hat = F_X^-1(1 - p^(1/n)) = F_X^-1(1 - p_sample)
  #  X_n_hat = F_X^-1(p^(1/n)) = F_X^-1(p_sample)
  # Where p_sample = p^(1/n)
  # Then the estimated bounds are:
  #  2 * min(x) - X_1_hat
  #  2 * max(x) - X_n_hat
  p_sample = p^(1/length(x))
  `x_1_hat,x_n_hat` = quantile(x, c(1 - p_sample, p_sample), names = FALSE)
  2 * range(x) - `x_1_hat,x_n_hat`
})

#' Estimate bounds of a distribution using Cooke's method
#'
#' Estimate the bounds of the distribution a sample came from using Cooke's method.
#' Use with the `bounder` argument to [density_bounded()].
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @inheritParams bounder_cdf
#'
#' @details
#' Estimate the bounds of a distribution using the method from Cooke (1979);
#' i.e. method 2.3 from Loh (1984). These bounds are:
#'
#' \deqn{\left[\begin{array}{l}
#' 2X_{(1)} - \sum_{i = 1}^n \left[\left(1 - \frac{i - 1}{n}\right)^n -
#'   \left(1 - \frac{i}{n}\right)^n \right] X_{(i)}\\
#' 2X_{(n)} - \sum_{i = 1}^n \left[\left(1 - \frac{n - i}{n}\right)^n -
#'   \left(1 - \frac{n + 1 - i}{n} \right)^n\right] X_{(i)}
#' \end{array}\right]}
#'
#' Where \eqn{X_{(i)}} is the \eqn{i}th order statistic of `x` (i.e. its
#' \eqn{i}th-smallest value).
#'
#' @template references-bounds-estimators
#' @template returns-bounder
#' @seealso The `bounder` argument to [density_bounded()].
#' @family bounds estimators
#' @export
bounder_cooke = auto_partial(name = "bounder_cooke", function(x) {
  x = sort(x)
  n = length(x)

  # the sequence in Cooke's method below reaches a point beyond which (due to
  # floating point round off) it is always zero. This point happens pretty
  # quickly on large samples (e.g. at 720 for a sample of size 10,000), so we
  # can save a lot of computation on very large samples by not computing
  # anything beyond that point (= n_nonzero). The reciprocal of the square root
  # of this point becomes roughly linear in 1/n at large n. The coefficients
  # below for estimating n_nonzero come from a linear model of
  # 1/sqrt(n_nonzero) ~ 1/n
  n_nonzero = if (n < 200) {
    n
  } else {
    ceiling(1 / (0.03659 + 6.89991 / n)^2)
  }

  # Method from Cooke (1979) Statistical Inference for Bounds of Random Variables,
  # re-written so i is 1 to n (instead of 0 to n - 1) and we multiply by X_(i)
  # instead of X_(n - i) (to avoid having to reverse x):
  #   i = seq_along(x)
  #   c(
  #     2 * x[1] - sum(((1 - (i - 1)/n)^n - (1 - i/n)^n) * x),
  #     2 * x[n] - sum(((1 - (n - i)/n)^n - (1 - (n + 1 - i)/n)^n) * x)
  #   )
  # Then, we re-write that to use logs (to make computation faster and more
  # stable), to only compute the non-zero coefficients, to use dot products
  # instead of sum(coef[i] * x[i]), and to take advantage of the fact that
  # the coefs for x[n] are just the reverse of the coefs for x[1] (so we
  # don't have to compute them twice):
  i = seq.int(1, length.out = n_nonzero)
  rev_i = seq.int(n, by = -1, length.out = n_nonzero)
  x_1_coefs = exp(log1p((1 - i)/n) * n) - exp(log1p(-i/n) * n)
  c(
    # need min and max here in case floating point error produces bounds
    # that are just slightly inside range(x)
    min(2 * x[1] - x_1_coefs %*% x[i], x[1]),
    max(2 * x[n] - x_1_coefs %*% x[rev_i], x[n])
  )
})

#' Estimate bounds of a distribution using the range of the sample
#'
#' Estimate the bounds of the distribution a sample came from using the range of the sample.
#' Use with the `bounder` argument to [density_bounded()].
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @inheritParams bounder_cdf
#'
#' @details
#' Estimate the bounds of a distribution using `range(x)`.
#'
#' @template returns-bounder
#' @seealso The `bounder` argument to [density_bounded()].
#' @family bounds estimators
#' @export
bounder_range = function(x) {
  range(x)
}
