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
#' @param x numeric vector containing a sample to compute a density estimate for.
#' @param n numeric: the number of grid points to evaluate the density estimator at.
#' @param adjust numeric: the bandwidth for the density estimator is multiplied
#' by this value. See [stats::density()].
#' @param trim Should the density estimate be trimmed to the bounds of the data?
#' If `TRUE`, uses [density_bounded()], if `FALSE`, uses [density_unbounded()].
#' @param ... Additional arguments passed to [stats::density()].
#'
#' @returns
#' An object of class `"density"`, mimicking the output format of
#' `stats:density()`, with the following components:
#'
#'   - `x`: The grid of points at which the density was estimated.
#'   - `y`: The estimated density values.
#'   - `bw`: The bandwidth.
#'   - `n`: The sample size of the `x` input argument.
#'   - `call`: The call used to produce the result, as a quoted expression.
#'   - `data.name`: The deparsed name of the `x` input argument.
#'   - `has.na`: Always `FALSE` (for compatibility).
#'
#' This allows existing methods (like `print()` and `plot()`) to work if desired.
#' This output format (and in particular, the `x` and `y` components) is also
#' the format expected by the `density` argument of the [stat_slabinterval()]
#' and the `smooth_` family of functions.
#'
#' @family density estimators
#' @examples
#'
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
#'
#' @importFrom rlang as_label enexpr get_expr
#' @export
density_auto = function(x, n = 512, adjust = 1, trim = TRUE, ...) {
  if (missing(x)) return(partial_self("density_auto"))

  x_label = as_label(enexpr(x))

  density = if (trim) density_bounded else density_unbounded
  d = density(x, n = n, adjust = adjust, trim = trim, ...)

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
#' @param n numeric: the number of grid points to evaluate the density estimator at.
#' @param adjust numeric: the bandwidth for the density estimator is multiplied
#' by this value. See [stats::density()].
#' @param trim Should the density estimate be trimmed to the bounds of the data?
#' @param ... Additional arguments passed to [stats::density()].
#'
#' @returns
#' An object of class `"density"`, mimicking the output format of
#' `stats:density()`, with the following components:
#'
#'   - `x`: The grid of points at which the density was estimated.
#'   - `y`: The estimated density values.
#'   - `bw`: The bandwidth.
#'   - `n`: The sample size of the `x` input argument.
#'   - `call`: The call used to produce the result, as a quoted expression.
#'   - `data.name`: The deparsed name of the `x` input argument.
#'   - `has.na`: Always `FALSE` (for compatibility).
#'
#' This allows existing methods (like `print()` and `plot()`) to work if desired.
#' This output format (and in particular, the `x` and `y` components) is also
#' the format expected by the `density` argument of the [stat_slabinterval()]
#' and the `smooth_` family of functions.
#'
#' @family density estimators
#' @examples
#'
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
#'
#' @importFrom rlang as_label enexpr get_expr
#' @export
density_unbounded = function(x, n = 512, adjust = 1, trim = FALSE, ...) {
  if (missing(x)) return(partial_self("density_unbounded"))

  x_label = as_label(enexpr(x))

  cut = if (trim) 0 else 3
  d = density(x, n = n, adjust = adjust, cut = cut, ...)

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
#' @param x numeric vector containing a sample to compute a density estimate for.
#' @param bounds length-2 vector of min and max bounds. If a bound is `NA`, then
#' that bound is replaced with `min(x)` or `max(x)`. Thus, the default,
#' `c(NA, NA)`, means that the bounds used are `range(x)`.
#' @param trim ignored; the unbounded density estimator always uses `trim = FALSE`
#' internally before trimming to `bounds`.
#'
#' @returns
#' An object of class `"density"`, mimicking the output format of
#' `stats:density()`, with the following components:
#'
#'   - `x`: The grid of points at which the density was estimated.
#'   - `y`: The estimated density values.
#'   - `bw`: The bandwidth.
#'   - `n`: The sample size of the `x` input argument.
#'   - `call`: The call used to produce the result, as a quoted expression.
#'   - `data.name`: The deparsed name of the `x` input argument.
#'   - `has.na`: Always `FALSE` (for compatibility).
#'
#' This allows existing methods (like `print()` and `plot()`) to work if desired.
#' This output format (and in particular, the `x` and `y` components) is also
#' the format expected by the `density` argument of the [stat_slabinterval()]
#' and the `smooth_` family of functions.
#'
#' @family density estimators
#' @examples
#'
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
#'   stat_slab(aes(x), density = density_bounded(bounds = c(0,1)), fill = NA, color = "#d95f02", alpha = 0.5) +
#'   scale_thickness_shared() +
#'   theme_ggdist()
#'
#'
#' @importFrom rlang as_label enexpr get_expr
#' @export
density_bounded = function(x, n = 512, adjust = 1, trim = FALSE, bounds = c(NA, NA), ...) {
  if (missing(x)) return(partial_self("density_bounded"))

  if (is.na(bounds[[1]])) bounds[[1]] = min(x)
  if (is.na(bounds[[2]])) bounds[[2]] = max(x)

  d = density_unbounded(x, n = n, adjust = adjust, trim = FALSE, ...)

  x = d$x
  y = d$y

  left = min(bounds)
  is_left = x < left
  y_left = y[is_left]

  right = max(bounds)
  is_right = x > right
  y_right = y[is_right]

  is_mid = !is_left & !is_right
  x = x[!is_left & !is_right]
  y = y[!is_left & !is_right]

  left_len = min(length(y), length(y_left))
  left_i = seq_len(left_len)
  y[left_i] = y[left_i] + rev(y_left)[left_i]

  right_len = min(length(y), length(y_right))
  right_i = seq_len(right_len)
  right_i_y = length(y) + 1 - right_i
  y[right_i_y] = y[right_i_y] + y_right[right_i]

  d$x = x
  d$y = y
  d$call = as.call(lapply(match.call(), get_expr))
  d
}

density_histogram = function(x, breaks = "Sturges", ...) {
  h = hist(x, breaks = breaks, plot = FALSE)

  x_1 = h$breaks[-length(h$breaks)]
  x_mid = h$mids
  x_2 = h$breaks[-1]

  cdf = cumsum(h$density) / sum(h$density)

  list(
    x = c(x_1[[1]],  rbind(x_1, x_mid, x_2), x_2[[length(x_2)]]),
    y = c(       0, rep(h$density, each = 3),                0),
    F_x = c(x_1[[1]], rbind(x_1, x_mid, x_2), x_2[[length(x_2)]])
  )
}
