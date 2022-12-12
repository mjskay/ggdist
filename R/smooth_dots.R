# dot smoothing for dotplots
#
# Author: mjskay
###############################################################################


#' Smooth dot positions in a dotplot
#'
#' Smooths `x` values using a density estimator, returning new `x` of the same
#' length. Intended for use with a dotplot (e.g. [geom_dots()]) to create
#' "density dotplots".
#' @param x a numeric vector
#' @param trim if `TRUE`, density is trimmed to `range(x)`, and
#' `range(smooth_dots(x))` is guaranteed to equal `range(x)`.
#' @param ... additional parameters passed to [density()]
#'
#' @details
#' Applies a kernel density estimator (KDE) to `x`, then uses weighted quantiles
#' of the KDE to generate a new set of `x` values with smoothed values. Plotted
#' using a dotplot (e.g. `geom_dots()`), these values create a variation on
#' a "density dotplot" (Zvinca 2018). Such plots are recommended only in very
#' large sample sizes where precise positions of individual values are not
#' particularly meaningful. In small samples, normal dotplots should generally
#' be used.
#'
#' @returns
#' A numeric vector of `length(x)`, where each entry is a smoothed version of
#' the corresponding entry in `x`.
#'
#' @references
#' Zvinca, Daniel. "In the pursuit of diversity in data visualization. Jittering data to access details."
#' \url{https://www.linkedin.com/pulse/pursuit-diversity-data-visualization-jittering-access-daniel-zvinca/}.
#'
#' @examples
#'
#' library(ggplot2)
#'
#' set.seed(1234)
#' x = rnorm(5000)
#'
#' # basic dotplot is noisy
#' ggplot() +
#'   geom_dots(aes(x))
#'
#' # density dotplot is smoother, but does move points (most noticeable
#' # in areas of low density)
#' ggplot() +
#'   geom_dots(aes(smooth_dots(x)))
#'
#' @export
smooth_dots = function(x, trim = TRUE, ...) {
  if (length(x) < 2) return(x)

  cut = if (trim) 0 else 3
  d = density(x, cut = cut, ...)
  n = length(x)

  # take quantiles from the KDE
  x_dens = weighted_quantile(d$x, ppoints(n, a = 1/2), d$y)

  if (trim) {
    # when using trim = TRUE, we want the first and last points in the smoothed
    # estimate to exactly equal the original first and last point --- they
    # may not quite do that if the density was particularly low at the edges,
    # so we'll pull those two points out if necessary
    x_dens[[1]] = min(x)
    x_dens[[n]] = max(x)
  }

  # match up each smoothed value to a close value from `x` using the order of x
  x_dens[rank(x, ties.method = "first")]
}
