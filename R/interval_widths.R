# interval widths
#
# Author: mjskay
###############################################################################



#' Nicely-spaced sets of interval widths
#'
#' @description
#' Create nicely-spaced sets of nested interval widths for use with (e.g.)
#' the `.width` parameter of [point_interval()], [stat_slabinterval()], or
#' [stat_lineribbon()].
#'
#' Creates a sequence of $n$ interval widths \eqn{p_i} such that
#' \eqn{0 < p_i \le \textrm{max} < 1} representing the masses of nested intervals
#' that are evenly-spaced on a reference distribution (by default a
#' normal distribution). `interval_widths()` generalizes the idea behind
#' the default use of the ~66% and 95% intervals in [stat_slabinterval()] and
#' 50%, 80%, and 95% intervals in [stat_lineribbon()]: applied to a Normal
#' distribution, those intervals are roughly evenly-spaced and allow one to
#' see deviations from the Normal distribution (such as excess kurtosis) when
#' the resulting intervals are *not* evenly spaced. `interval_widths()`
#' creates intervals with this property on any distribution (though if the
#' distribution is not symmetric, the intervals will only appear evenly-spaced
#' on the upper half of the distribution).
#'
#' `pretty_widths()` is a variant of `interval_widths()` with defaults for
#' `max` and `precision` that make the resulting intervals more human-readable
#' (for labelling purposes).
#' @param n <[numeric]> in \eqn{[0, \infty)}: Number of intervals to generate.
#' @param dist <[distribution][distributional::distributional]>: Reference
#' distribution.
#' @param max <[numeric]> in \eqn{(0, 1)}: Maximum interval width.
#' @param precision <[numeric] | [NULL]>: If not `NULL`, a value in \eqn{(0, 1)}
#' giving the precision to round resulting intervals to. Intervals will only be
#' rounded if the result does not create collisions (in order to guarantee `n`
#' unique intervals are returned).
#' @details
#' Given the cumulative distribution function (CDF) \eqn{F_\textrm{dist}(q)}
#' and the inverse CDF (quantile function) \eqn{F^{-1}_\textrm{dist}(p)}
#' of `dist`, the following is a sequence of \eqn{n + 1} evenly-spaced
#' quantiles of `dist` that could represent upper limits of nested intervals,
#' where \eqn{q_i = q_0 + i\frac{q_n - q_0}{n}}:
#'
#' \deqn{\begin{array}{rcl}
#' q_0, \ldots, q_n &=& F^{-1}_\textrm{dist}(0.5), \ldots, F^{-1}_\textrm{dist}(0.5 + \frac{\textrm{max}}{2})
#' \end{array}}
#'
#' `interval_widths(n)` returns the `n` interval widths corresponding to the
#' upper endpoints \eqn{q_1, \ldots q_n}:
#'
#' \deqn{
#' 2\cdot\left[F_\textrm{dist}(q_1) - 0.5\right], \ldots, 2\cdot\left[F_\textrm{dist}(q_n) - 0.5\right]
#' }
#'
#' @returns A length-`n` numeric vector of interval widths (masses) between
#' `0` and `1` (exclusive) in increasing order.
#' @seealso The `.width` argument to [point_interval()], [stat_slabinterval()],
#' [stat_lineribbon()], etc.
#' @examples
#' interval_widths(1)   # 0.9
#' # this is roughly +/- 1 SD and +/- 2 SD
#' interval_widths(2)   # 0.672..., 0.95
#' interval_widths(3)   # 0.521..., 0.844..., 0.966...
#'
#' # "pretty" widths may be useful for legends with a small number of widths
#' pretty_widths(1)     # 0.95
#' pretty_widths(2)     # 0.65, 0.95
#' pretty_widths(3)     # 0.50, 0.80, 0.95
#'
#' # larger numbers of intervals can be useful for plots
#' data.frame(x = 1:20/20) |>
#'   ggplot(aes(x, ydist = distributional::dist_normal((x * 5) ^2, 1 + x * 5))) +
#'   stat_lineribbon(.width = pretty_widths(10))
#'
#' # large numbers of intervals can be used to create gradients -- particularly
#' # useful if you shade ribbons according to density (not interval width)
#' # (this is currently experimental)
#' withr::with_options(list(ggdist.experimental.slab_data_in_intervals = TRUE), print(
#'   data.frame(x = 1:20/20) |>
#'     ggplot(aes(x, ydist = distributional::dist_normal((x * 5) ^2, 1 + x * 5))) +
#'     stat_lineribbon(
#'       aes(fill_ramp = after_stat(ave(pdf_min, level))),
#'       .width = interval_widths(40),
#'       fill = "gray50"
#'     ) +
#'     theme_ggdist()
#' ))
#' @export
interval_widths = function(
  n,
  dist = distributional::dist_normal(),
  max = 1 - 0.1/n,
  precision = NULL
) {
  stopifnot(
    length(n) == 1L, n >= 0,
    length(max) == 1L, max > 0, max < 1
  )
  if (n == 0) return(numeric())

  p_bounds = c(0.5, max / 2 + 0.5)
  q_bounds = quantile(dist, p_bounds)[[1]]
  q = seq(q_bounds[[1]], q_bounds[[2]], length.out = n + 1)[-1L]
  p = (cdf(dist, q)[[1]] - 0.5) * 2

  if (is.null(precision)) {
    p
  } else {
    stopifnot(length(precision) == 1L, precision > 0, precision < 1)
    p_rounded = unique(
      pmin(pmax(round(p / precision) * precision, precision/2), 1 - precision/2)
    )
    if (length(p_rounded) != n) p else p_rounded
  }
}

#' @rdname interval_widths
#' @export
pretty_widths = function(
  n,
  dist = distributional::dist_normal(),
  max = if (n <= 4) 0.95 else 1 - 0.1/n,
  precision = if (n <= 4) 0.05 else 0.01
) {
  interval_widths(n, dist = dist, max = max, precision = precision)
}
