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
#' @param trim Should the density estimate be trimmed?
#' If `TRUE`, uses [density_bounded()], estimating the bounds by default (see the
#' `bounds` argument to [density_bounded()]). If `FALSE`, uses [density_unbounded()],
#' setting the bounds at `3 * bandwidth` from the endpoints of the data.
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
  na.rm = FALSE,
  ...
) {
  if (missing(x)) return(partial_self("density_auto"))

  x_label = as_label(enexpr(x))
  x = check_na(x, na.rm)

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
#' @param na.rm Should missing (`NA`) values in `x` be removed?
#' @param ... Additional arguments (ignored).
#' @param range_only If `TRUE`, the range of the output of this density estimator
#' is computed and is returned in the `$x` element of the result, and `c(NA, NA)`
#' is returned in `$y`. This gives a faster way to determine the range of the output
#' than `density_XXX(n = 2)`.
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
  trim = FALSE,
  na.rm = FALSE,
  ...,
  range_only = FALSE
) {
  if (missing(x)) return(partial_self("density_unbounded"))

  x_label = as_label(enexpr(x))
  x = check_na(x, na.rm)

  bw = get_bandwidth(x, bandwidth) * adjust
  cut = if (trim) 0 else 3

  if (isTRUE(range_only)) {
    return(list(
      x = c(min(x) - cut * bw, max(x) + cut * bw),
      y = c(NA_real_, NA_real_)
    ))
  }

  d = density(
    x, weights = weights,
    n = n, bw = bw, adjust = 1, kernel = kernel,
    cut = cut
  )

  d$data.name = x_label
  # need to apply get_expr over match.call() instead of just using match.call()
  # to remove tildes from the call created by partial application
  d$call = as.call(lapply(match.call(), get_expr))
  d$cdf = weighted_ecdf(x, weights)(d$x)
  d
}


#' Bounded density estimator using the reflection method
#'
#' Bounded density estimator using the reflection method.
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @inheritParams density_unbounded
#' @param bounds length-2 vector of min and max bounds. If a bound is `NA`, then
#' that bound is estimated from the data using the method specified by `find_bounds`.
#' @param find_bounds How to find missing (`NA`) `bounds`. One of:
#'  - `"cooke"`: Use the method from Cooke (1979); i.e. method 2.3 from Loh (1984).
#'  - `"range"`: Use the range of `x` (i.e the `min` or `max`).
#' @param trim ignored; the unbounded density estimator always uses `trim = FALSE`
#' internally before trimming to `bounds`.
#' @template returns-density
#' @template references-bounds-estimators
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
  trim = TRUE, bounds = c(NA, NA), find_bounds = c("cooke", "range"),
  na.rm = FALSE,
  ...,
  range_only = FALSE
) {
  if (missing(x)) return(partial_self("density_bounded"))

  if (n < 1) stop0("density_bounded() must have an n of at least 1")

  x = check_na(x, na.rm)

  # determine bandwidth
  bw = get_bandwidth(x, bandwidth) * adjust

  # determine bounds
  bounds_to_find = is.na(bounds)
  if (any(bounds_to_find)) {
    find_bounds = switch(match.arg(find_bounds),
      cooke = find_bounds_cooke,
      range = range
    )
    bounds[bounds_to_find] = find_bounds(x)[bounds_to_find]
  }

  min_x = min(x)
  max_x = max(x)
  if (min_x < bounds[[1]] || max_x > bounds[[2]]) {
    stop0("All `x` must be inside `bounds` in density_bounded()")
  }

  # cap bounds at 3*bw beyond the range (and consider them unbounded beyond
  # that, since past that the density essentially goes to 0)
  min_bound = min_x - 3 * bw
  max_bound = max_x + 3 * bw
  left_bounded = min_bound <= bounds[[1]]
  right_bounded = bounds[[2]] <= max_bound
  if (!left_bounded) bounds[[1]] = min_bound
  if (!right_bounded) bounds[[2]] = max_bound

  if (isTRUE(range_only)) {
    return(list(x = bounds, y = c(NA_real_, NA_real_)))
  }

  # to get final n = requested n, if a bound is supplied, must add n - 1 values
  # beyond that bound, which will be reflected back
  n_unbounded = n + (left_bounded + right_bounded) * (n - 1)

  # determine limits of underlying unbounded density estimator
  from = bounds[[1]]
  to = bounds[[2]]
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
  d$x = d$x[mid]
  f = d$y[mid]
  if (left_bounded) f = f + d$y[n:1]
  if (right_bounded) f = f + d$y[seq(length(d$y), by = -1, length.out = n)]

  d$y = f
  d$call = as.call(lapply(match.call(), get_expr))
  d$cdf = weighted_ecdf(x, weights)(d$x)
  d
}


#' Histogram density estimator
#'
#' Histogram density estimator.
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @param x numeric vector containing a sample to compute a density estimate for.
#' @param weights optional numeric vector of weights to apply to `x`.
#' @param breaks Determines the breakpoints defining bins. Similar to the `breaks`
#' argument to [graphics::hist()]. One of:
#'   - A scalar (length-1) numeric giving the number of bins
#'   - A vector numeric giving the breakpoints between histogram bins
#'   - A function taking `x` and `weights` and returning either the
#'     number of bins or a vector of breakpoints
#'   - A string giving the suffix of a function that starts with
#'     `"breaks_"`. \pkg{ggdist} provides weighted implementations of the
#'     `"Sturges"`, `"scott"`, and `"FD"` break-finding algorithms from
#'     [graphics::hist()].
#' @param outline_bars Should outlines in between the bars (i.e. density values of
#' 0) be included?
#' @param na.rm Should missing (`NA`) values in `x` be removed?
#' @param ... Additional arguments (ignored).
#' @param range_only If `TRUE`, the range of the output of this density estimator
#' is computed and is returned in the `$x` element of the result, and `c(NA, NA)`
#' is returned in `$y`. This gives a faster way to determine the range of the output
#' than `density_XXX(n = 2)`.
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
#' d = density_histogram(x)
#' d
#'
#' # ... thus, while designed for use with the `density` argument of
#' # stat_slabinterval(), output from density_histogram() can also be used with
#' # base::plot():
#' plot(d)
#'
#' # here we'll use the same data as above with stat_slab():
#' data.frame(x) %>%
#'   ggplot() +
#'   stat_slab(
#'     aes(xdist = dist), data = data.frame(dist = dist_beta(1, 3)),
#'     alpha = 0.25
#'   ) +
#'   stat_slab(aes(x), density = "histogram", fill = NA, color = "#d95f02", alpha = 0.5) +
#'   scale_thickness_shared() +
#'   theme_ggdist()
#' @importFrom rlang as_label enexpr get_expr
#' @export
density_histogram = function(
  x, weights = NULL,
  breaks = "Sturges",
  outline_bars = FALSE,
  na.rm = FALSE,
  ...,
  range_only = FALSE
) {
  if (missing(x)) return(partial_self("density_histogram"))

  x_label = as_label(enexpr(x))
  x = check_na(x, na.rm)

  # TODO: use weighted_hist
  h = hist(x, breaks = breaks, plot = FALSE)
  input_1 = h$breaks[-length(h$breaks)]  # first edge of bin
  input_2 = h$breaks[-1]                 # second edge of bin
  input_ = (input_1 + input_2)/2   # center of bin

  cdf_fun = weighted_ecdf(x, weights)
  cdf_1 = cdf_fun(input_1)
  cdf_2 = cdf_fun(input_2)
  cdf_ = cdf_fun(input_)

  if (!outline_bars) {
    # as.vector(rbind(x, y)) interleaves vectors input_1 and input_2, giving
    # us the bin endpoints --- then just need to repeat the same value of density
    # for both endpoints of the same bin
    input = as.vector(rbind(input_1, input_, input_, input_2))
    pdf = as.vector(rep(h$density, each = 4))
    cdf = as.vector(rbind(cdf_1, cdf_1, cdf_, cdf_2))
  } else {
    # have to return to 0 in between each bar so that bar outlines are drawn
    input = as.vector(rbind(input_1, input_1, input_, input_, input_2, input_2))
    pdf = as.vector(rbind(0, h$density, h$density, h$density, h$density, 0))
    cdf = as.vector(rbind(cdf_1, cdf_1, cdf_1, cdf_, cdf_2, cdf_2))
  }

  structure(list(
    x = input,
    y = pdf,
    bw = if (h$equidist) diff(h$breaks[1:2]) else mean(diff(h$breaks)),
    n = length(x),
    # need to apply get_expr over match.call() instead of just using match.call()
    # to remove tildes from the call created by partial application
    call = as.call(lapply(match.call(), get_expr)),
    data.name = x_label,
    has.na = FALSE,
    cdf = cdf
  ), class = "density")
}


# helpers -----------------------------------------------------------------

get_bandwidth = function(x, bandwidth) {
  if (!is.numeric(bandwidth)) {
    bandwidth = match_function(bandwidth, prefix = "bw.")(x)
  }
  bandwidth
}

find_bounds_cdf = function(x) {
  # TODO: currently unused

  # we use the distribution of the order statistic of a sample to estimate
  # where the first and last order statistics (i.e. the min and max) of this
  # distribution would be assuming the sample `x` is the distribution, then
  # we adjust the boundary outwards from min(x) (or max(x)) by the distance
  # between min(x) (or max(x)) and the nearest estimated order statistic
  # (scaled by s, the ratio of the range of x and the range between the two
  # estimated order statistics).
  # We can use the fact that given a CDF of distribution F_X(x), the
  # distribution of its first and last order statistics are:
  #  F_X_1(x) = 1 - (1 - F_X(x))^n
  #  F_X_n(x) = F_X(x)^n
  # Re-arranging, we can get the inverse CDFs (quantile functions) of each
  # in terms of the quantile function of X:
  #  F_X_1^-1(x) = F_X^-1(1 - (1 - p)^(1/n))
  #  F_X_n^-1(x) = F_X^-1(p^(1/n))
  # Then we can use the median of these distributions as our estimates by
  # calculating them at p = 0.5:
  #  median(X_1) = F_X^-1(1 - 0.5^(1/n)) = F_X^-1(1 - p_median)
  #  median(X_n) = F_X^-1(0.5^(1/n)) = F_X^-1(p_median)
  # Where p_median = 0.5^(1/n)
  # Then the estimated bounds are:
  #  2 * min(x) - median(X_1)
  #  2 * max(x) - median(X_n)
  p_median = 0.5^(1/length(x))
  median_x_1_n = quantile(x, c(1 - p_median, p_median), names = FALSE)
  2 * range(x) - median_x_1_n
}

find_bounds_cooke = function(x) {
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
}
