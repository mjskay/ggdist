# density estimators
#
# Author: mjskay
###############################################################################



#' Automatic density estimator
#'
#' Density estimator that picks [density_bounded()] or [density_unbounded()]
#' depending on `trim`.
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @inheritParams density_unbounded
#' @param trim Should the density estimate be trimmed to the bounds of the data?
#' If `TRUE`, uses [density_bounded()], if `FALSE`, uses [density_unbounded()].
#' @param ... Additional arguments passed to [density_bounded()] or [density_unbounded()].
#' @template returns-density
#' @family density estimators
#' @examples
#' library(distributional)
#' library(dplyr)
#' library(ggplot2)
#'
#' set.seed(123)
#' x = rbeta(5000, 1, 3)
#'
#' # here we'll use the same data as above, but pick either density_bounded()
#' # or density_unbounded() (which is equivalent to stats::density()). Notice
#' # how the bounded density (green) is biased near the boundary of the support,
#' # while the unbounded density is not.
#' data.frame(x) %>%
#'   ggplot() +
#'   stat_slab(
#'     aes(xdist = dist), data = data.frame(dist = dist_beta(1, 3)),
#'     alpha = 0.25
#'   ) +
#'   stat_slab(aes(x), density = "auto", trim = TRUE, fill = NA, color = "#d95f02", alpha = 0.5) +
#'   stat_slab(aes(x), density = "auto", trim = FALSE, fill = NA, color = "#1b9e77", alpha = 0.5) +
#'   scale_thickness_shared() +
#'   theme_ggdist()
#' @importFrom rlang as_label enexpr get_expr
#' @export
density_auto = function(
  x, weights = NULL,
  n = 512, bandwidth = "nrd0", adjust = 1, kernel = "gaussian",
  trim = FALSE,
  ...
) {
  if (missing(x)) return(partial_self("density_auto"))

  x_label = as_label(enexpr(x))

  density = if (trim) density_bounded else density_unbounded
  d = density(
    x, weights = weights,
    n = n, bandwidth = bandwidth, adjust = adjust, kernel = kernel,
    trim = trim,
    ...
  )

  d$data.name = x_label
  d$call = as.call(lapply(match.call(), get_expr))
  d
}


#' Unbounded density estimator
#'
#' Unbounded density estimator using [stats::density()].
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @param x numeric vector containing a sample to compute a density estimate for.
#' @param weights optional numeric vector of weights to apply to `x`.
#' @param n numeric: the number of grid points to evaluate the density estimator at.
#' @param bandwidth bandwidth of the density estimator. One of:
#'   - a numeric: the bandwidth, as the standard deviation of the kernel
#'   - a function: a function taking `x` (the sample) and returning the bandwidth
#'   - a string: the suffix of the name of a function starting with `"bw."` that
#'     will be used to determine the bandwidth. See [bw.nrd0()] for a list.
#' @param adjust numeric: the bandwidth for the density estimator is multiplied
#' by this value. See [stats::density()].
#' @param kernel string: the smoothing kernel to be used. This must partially
#' match one of `"gaussian"`, `"rectangular"`, `"triangular"`, `"epanechnikov"`,
#' `"biweight"`, `"cosine"`, or `"optcosine"`. See [stats::density()].
#' @param trim Should the density estimate be trimmed to the bounds of the data?
#' @template returns-density
#' @family density estimators
#' @examples
#' library(distributional)
#' library(dplyr)
#' library(ggplot2)
#'
#' # For compatibility with existing code, the return type of density_unbounded()
#' # is the same as stats::density(), ...
#' set.seed(123)
#' x = rbeta(5000, 1, 3)
#' d = density_unbounded(x)
#' d
#'
#' # ... thus, while designed for use with the `density` argument of
#' # stat_slabinterval(), output from density_unbounded() can also be used with
#' # base::plot():
#' plot(d)
#'
#' # here we'll use the same data as above, but pick either density_bounded()
#' # or density_unbounded() (which is equivalent to stats::density()). Notice
#' # how the bounded density (green) is biased near the boundary of the support,
#' # while the unbounded density is not.
#' data.frame(x) %>%
#'   ggplot() +
#'   stat_slab(
#'     aes(xdist = dist), data = data.frame(dist = dist_beta(1, 3)),
#'     alpha = 0.25
#'   ) +
#'   stat_slab(aes(x), density = "bounded", fill = NA, color = "#d95f02", alpha = 0.5) +
#'   stat_slab(aes(x), density = "unbounded", fill = NA, color = "#1b9e77", alpha = 0.5) +
#'   scale_thickness_shared() +
#'   theme_ggdist()
#' @importFrom rlang as_label enexpr get_expr
#' @export
density_unbounded = function(
  x, weights = NULL,
  n = 512, bandwidth = "nrd0", adjust = 1, kernel = "gaussian",
  trim = FALSE
) {
  if (missing(x)) return(partial_self("density_unbounded"))

  x_label = as_label(enexpr(x))

  bw = get_bandwidth(x, bandwidth)
  cut = if (trim) 0 else 3
  d = density(
    x, weights = weights,
    n = n, bw = bw, adjust = adjust, kernel = kernel,
    cut = cut
  )

  d$data.name = x_label
  # need to apply get_expr over match.call() instead of just using match.call()
  # to remove tildes from the call created by partial application
  d$call = as.call(lapply(match.call(), get_expr))
  d
}


#' Bounded density estimator using the reflection method
#'
#' Bounded density estimator using the reflection method.
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @inheritParams density_unbounded
#' @param bounds length-2 vector of min and max bounds. If a bound is `NA`, then
#' that bound is replaced with `min(x)` or `max(x)`. Thus, the default,
#' `c(NA, NA)`, means that the bounds used are `range(x)`.
#' @param trim ignored; the unbounded density estimator always uses `trim = FALSE`
#' internally before trimming to `bounds`.
#' @template returns-density
#' @family density estimators
#' @examples
#' library(distributional)
#' library(dplyr)
#' library(ggplot2)
#'
#' # For compatibility with existing code, the return type of density_bounded()
#' # is the same as stats::density(), ...
#' set.seed(123)
#' x = rbeta(5000, 1, 3)
#' d = density_bounded(x)
#' d
#'
#' # ... thus, while designed for use with the `density` argument of
#' # stat_slabinterval(), output from density_bounded() can also be used with
#' # base::plot():
#' plot(d)
#'
#' # here we'll use the same data as above, but pick either density_bounded()
#' # or density_unbounded() (which is equivalent to stats::density()). Notice
#' # how the bounded density (green) is biased near the boundary of the support,
#' # while the unbounded density is not.
#' data.frame(x) %>%
#'   ggplot() +
#'   stat_slab(
#'     aes(xdist = dist), data = data.frame(dist = dist_beta(1, 3)),
#'     alpha = 0.25
#'   ) +
#'   stat_slab(aes(x), density = "bounded", fill = NA, color = "#d95f02", alpha = 0.5) +
#'   stat_slab(aes(x), density = "unbounded", fill = NA, color = "#1b9e77", alpha = 0.5) +
#'   scale_thickness_shared() +
#'   theme_ggdist()
#'
#' # We can also supply arguments to the density estimators by using their
#' # full function names instead of the string suffix; e.g. we can supply
#' # the exact bounds of c(0,1) rather than using the bounds of the data.
#' data.frame(x) %>%
#'   ggplot() +
#'   stat_slab(
#'     aes(xdist = dist), data = data.frame(dist = dist_beta(1, 3)),
#'     alpha = 0.25
#'   ) +
#'   stat_slab(
#'     aes(x), fill = NA, color = "#d95f02", alpha = 0.5,
#'     density = density_bounded(bounds = c(0,1))
#'   ) +
#'   scale_thickness_shared() +
#'   theme_ggdist()
#' @importFrom rlang as_label enexpr get_expr
#' @export
density_bounded = function(
  x, weights = NULL,
  n = 512, bandwidth = "nrd0", adjust = 1, kernel = "gaussian",
  trim = TRUE, bounds = c(NA, NA)
) {
  if (missing(x)) return(partial_self("density_bounded"))

  if (n < 1) stop0("density_bounded() must have an n of at least 1")

  # determine bounds
  min_x = min(x)
  max_x = max(x)
  if (is.na(bounds[[1]])) bounds[[1]] = min_x
  if (is.na(bounds[[2]])) bounds[[2]] = max_x
  left_bounded = is.finite(bounds[[1]])
  right_bounded = is.finite(bounds[[2]])

  if (min_x < bounds[[1]] || max_x > bounds[[2]]) {
    stop0("All `x` must be inside `bounds` in density_bounded()")
  }

  # to get final n = requested n, if a bound is supplied, must add n - 1 values
  # beyond that bound, which will be reflected back
  n_unbounded = n + (left_bounded + right_bounded) * (n - 1)

  # determine limits of underlying unbounded density estimator
  bw = get_bandwidth(x, bandwidth) * adjust
  from = if (left_bounded) bounds[[1]] else min_x - 3 * bw
  to = if (right_bounded) bounds[[2]] else max_x + 3 * bw
  width = to - from
  if (left_bounded) from = from - width
  if (right_bounded) to = to + width

  # get unbounded estimate
  d = density(
    x, weights = weights,
    n = n_unbounded, bw = bw, adjust = 1, kernel = kernel,
    from = from, to = to
  )

  # reflect tails back into middle, if needed
  mid = seq(1 + left_bounded * (n - 1), length.out = n)
  x = d$x[mid]
  y = d$y[mid]
  if (left_bounded) y = y + d$y[n:1]
  if (right_bounded) y = y + d$y[seq(length(d$y), by = -1, length.out = n)]

  d$x = x
  d$y = y
  d$call = as.call(lapply(match.call(), get_expr))
  d
}


# helpers -----------------------------------------------------------------

get_bandwidth = function(x, bandwidth) {
  if (!is.numeric(bandwidth)) {
    bandwidth = match_function(bandwidth, prefix = "bw.")(x)
  }
  bandwidth
}
