# dot smoothing for dotplots
#
# Author: mjskay
###############################################################################


#' Smooth dot positions in a dotplot using a kernel density estimator ("density dotplots")
#'
#' Smooths `x` values using a density estimator, returning new `x` of the same
#' length. Can be used with a dotplot (e.g. [`geom_dots`]`(smooth = ...)`) to create
#' "density dotplots".
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @param x a numeric vector
#' @param density Density estimator to use for smoothing. One of:
#'  - A function which takes a numeric vector and returns a list with elements
#'    `x` (giving grid points for the density estimator) and `y` (the
#'    corresponding densities). \pkg{ggdist} provides a family of functions
#'    following this format, including [density_unbounded()] and
#'    [density_bounded()].
#'  - A string giving the suffix of a function name that starts with `"density_"`;
#'    e.g. `"bounded"` for `[density_bounded()]`.
#' @param ... Arguments passed to the density estimator specified by `density`.
#' @inheritParams density_bounded
#'
#' @details
#' Applies a kernel density estimator (KDE) to `x`, then uses weighted quantiles
#' of the KDE to generate a new set of `x` values with smoothed values. Plotted
#' using a dotplot (e.g. `geom_dots(smooth = "bounded")` or
#' `geom_dots(smooth = smooth_bounded(...)`), these values create a variation on
#' a "density dotplot" (Zvinca 2018).
#'
#' Such plots are recommended **only** in very
#' large sample sizes where precise positions of individual values are not
#' particularly meaningful. In small samples, normal dotplots should generally
#' be used.
#'
#' Two variants are supplied by default:
#'
#' - `smooth_bounded()`, which uses [density_bounded()].
#'   Passes the `bounds` arguments to the estimator.
#' - `smooth_unbounded()`, which uses [density_unbounded()].
#'
#' It is generally recommended to pick the smooth based on the known bounds of
#' your data, e.g. by using `smooth_bounded()` with the `bounds` parameter if
#' there are finite bounds, or `smooth_unbounded()` if both bounds are infinite.
#'
#' @returns
#' A numeric vector of `length(x)`, where each entry is a smoothed version of
#' the corresponding entry in `x`.
#'
#' If `x` is missing, returns a partial application of itself. See [automatic-partial-functions].
#'
#' @references
#' Zvinca, Daniel. "In the pursuit of diversity in data visualization. Jittering data to access details."
#' \url{https://www.linkedin.com/pulse/pursuit-diversity-data-visualization-jittering-access-daniel-zvinca/}.
#'
#' @family dotplot smooths
#' @examples
#'
#' library(ggplot2)
#'
#' set.seed(1234)
#' x = rnorm(1000)
#'
#' # basic dotplot is noisy
#' ggplot(data.frame(x), aes(x)) +
#'   geom_dots()
#'
#' # density dotplot is smoother, but does move points (most noticeable
#' # in areas of low density)
#' ggplot(data.frame(x), aes(x)) +
#'   geom_dots(smooth = "unbounded")
#'
#' # you can adjust the kernel and bandwidth...
#' ggplot(data.frame(x), aes(x)) +
#'   geom_dots(smooth = smooth_unbounded(kernel = "triangular", adjust = 0.5))
#'
#' # for bounded data, you should use the bounded smoother
#' x_beta = rbeta(1000, 0.5, 0.5)
#'
#' ggplot(data.frame(x_beta), aes(x_beta)) +
#'   geom_dots(smooth = smooth_bounded(bounds = c(0, 1)))
#'
#' @name smooth_density
NULL

# currently not exporting .smooth_density as people should really be forced to
# think about if they want a bounded or unbounded smoother
.smooth_density = function(x, density = "bounded", trim = FALSE, ...) {
  if (length(x) < 2) return(x)

  density = match_function(density, prefix = "density_")

  d = density(x, trim = trim, ...)
  n = length(x)

  # when trim = TRUE, set a = 1 so ppoints goes from 0 to 1 inclusive, which
  # ensures the first and last point coincide with the first and last point
  # from the data
  a = if (trim) 1 else 0.5

  # take quantiles from the KDE
  x_dens = weighted_quantile(d$x, ppoints(n, a = a), d$y, type = 5, names = FALSE)

  # match up each smoothed value to a close value from `x` using the order of x
  x_dens[rank(x, ties.method = "first")]
}

#' @rdname smooth_density
#' @export
smooth_bounded = auto_partial(name = "smooth_bounded", function(
  x, density = "bounded", bounds = c(NA, NA), bounder = "cooke", trim = FALSE, ...
) {
  .smooth_density(x, density = density, bounds = bounds, bounder = bounder, trim = trim, ...)
})

#' @rdname smooth_density
#' @export
smooth_unbounded = auto_partial(name = "smooth_unbounded", function(
  x, density = "unbounded", trim = FALSE, ...
) {
  .smooth_density(x, density = density, trim = trim, ...)
})


# discrete smooths --------------------------------------------------------

#' Smooth dot positions in a dotplot of discrete values ("bar dotplots")
#'
#' @description
#' **Note:** Better-looking bar dotplots are typically easier to achieve using
#' `layout = "bar"` with the [geom_dotsinterval()] family instead of
#' `smooth = "bar"` or `smooth = "discrete"`.
#'
#' Smooths `x` values where `x` is presumed to be discrete, returning a new `x`
#' of the same length. Both `smooth_discrete()` and `smooth_bar()` use the
#' [resolution()] of the data to apply smoothing around unique values in the
#' dataset; `smooth_discrete()` uses a kernel density estimator and `smooth_bar()`
#' places values in an evenly-spaced grid. Can be used with a dotplot
#' (e.g. [`geom_dots`]`(smooth = ...)`) to create "bar dotplots".
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @param x a numeric vector
#' @param width approximate width of the bars as a fraction of data [resolution()].
#' @param ... additional parameters; [smooth_discrete()] passes these to
#' [smooth_unbounded()] and thereby to [density_unbounded()]; [smooth_bar()]
#' ignores them.
#' @inheritParams density_unbounded
#'
#' @details
#' `smooth_discrete()` applies a kernel density estimator (default: rectangular)
#' to `x`. It automatically sets the bandwidth to be such that the kernel's
#' width (for each kernel type) is approximately `width` times the [resolution()]
#' of the data. This means it essentially creates smoothed bins around each
#' unique value. It calls down to [smooth_unbounded()].
#'
#' `smooth_bar()` generates an evenly-spaced grid of values spanning `+/- width/2`
#' around each unique value in `x`.
#'
#' @returns
#' A numeric vector of `length(x)`, where each entry is a smoothed version of
#' the corresponding entry in `x`.
#'
#' If `x` is missing, returns a partial application of itself. See [automatic-partial-functions].
#'
#' @family dotplot smooths
#' @examples
#'
#' library(ggplot2)
#'
#' set.seed(1234)
#' x = rpois(1000, 2)
#'
#' # automatic binwidth in basic dotplot on large counts in discrete
#' # distributions is very small
#' ggplot(data.frame(x), aes(x)) +
#'   geom_dots()
#'
#' # NOTE: It is now recommended to use layout = "bar" instead of
#' # smooth = "discrete" or smooth = "bar"; the latter are retained because
#' # they can sometimes be useful in combination with other layouts for
#' # more specialized (but finicky) applications.
#' ggplot(data.frame(x), aes(x)) +
#'   geom_dots(layout = "bar")
#'
#' # smooth_discrete() constructs wider bins of dots
#' ggplot(data.frame(x), aes(x)) +
#'   geom_dots(smooth = "discrete")
#'
#' # smooth_bar() is an alternative approach to rectangular layouts
#' ggplot(data.frame(x), aes(x)) +
#'   geom_dots(smooth = "bar")
#'
#' # adjust the shape by changing the kernel or the width. epanechnikov
#' # works well with side = "both"
#' ggplot(data.frame(x), aes(x)) +
#'   geom_dots(smooth = smooth_discrete(kernel = "epanechnikov", width = 0.8), side = "both")
#'
#'
#' @export
smooth_discrete = auto_partial(name = "smooth_discrete", function(
  x,
  kernel = c("rectangular", "gaussian", "epanechnikov", "triangular", "biweight", "cosine", "optcosine"),
  width = 0.7,
  ...
) {
  if (length(x) < 2) return(x)

  # magic numbers below ensure that the range of the kernel with bandwidth = 1 is a
  # little less than 1 (i.e. +/- 0.5 around a bin)
  kernel = match.arg(kernel)
  bw_mult = switch(kernel,
    gaussian = 0.16,
    epanechnikov = 0.21,
    rectangular = 0.26,
    triangular = 0.20,
    biweight = 0.19,
    cosine = 0.18,
    optcosine = 0.20
  )
  bandwidth = resolution(x, zero = FALSE) * bw_mult * width
  smooth_unbounded(x, kernel = kernel, bandwidth = bandwidth, ...)
})

#' @rdname smooth_discrete
#' @export
smooth_bar = auto_partial(name = "smooth_bar", function(x, width = 0.7, ...) {
  if (length(x) < 2) return(x)

  x_width = resolution(x, zero = FALSE) * width
  split(x, x) = lapply(split(x, x), function(x) {
    (ppoints(length(x), 0.5) - 0.5) * x_width + x[[1]]
  })
  x
})

#' Apply no smooth to a dotplot
#'
#' Default smooth for dotplots: no smooth. Simply returns the input values.
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @param x a numeric vector
#' @param ... ignored
#'
#' @details
#' This is the default value for the `smooth` argument of `geom_dotsinterval()`.
#'
#' @returns
#' `x`
#'
#' If `x` is missing, returns a partial application of itself. See [automatic-partial-functions].
#'
#' @family dotplot smooths
#' @export
smooth_none = auto_partial(name = "smooth_none", function(x, ...) {
  x
})
