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
#' If `TRUE`, uses `density_bounded(trim = TRUE)`, estimating the bounds by default (see the
#' `bounds` argument to [density_bounded()]). If `FALSE`, uses `density_unbounded(trim = FALSE)`,
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
#' @param clusters (experimental) Positive integer. If `clusters > 1`, uses
#' an adaptive approach to calculate the density. First, uses the
#' adaptive bandwidth algorithm of Abramson (1982) to determine local (pointwise)
#' bandwidths, then clusters these bandwidths into `clusters` clusters with
#' similar bandwidths, then calculates and sums the densities from each cluster.
#' You can set this to a very large number (e.g. `Inf`) for a fully adaptive
#' approach, but this will be very slow; typically something around 5--10 yields
#' nearly identical results.
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
  clusters = 1,
  na.rm = FALSE,
  ...,
  range_only = FALSE
) {
  if (missing(x)) return(partial_self("density_unbounded"))

  x_label = as_label(enexpr(x))
  x = check_na(x, na.rm)
  if (isTRUE(range_only) && isTRUE(trim)) {
    return(list(x = range(x), y = c(NA_real_, NA_real_)))
  }

  bw = get_bandwidth(x, bandwidth) * adjust
  cut = if (trim) 0 else 3

  if (isTRUE(range_only)) {
    return(list(
      x = c(min(x) - cut * bw, max(x) + cut * bw),
      y = c(NA_real_, NA_real_)
    ))
  }

  d = .density_adaptive(
    x, weights = weights,
    n = n, bw = bw, kernel = kernel,
    cut = cut,
    clusters = clusters
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
#' that bound is estimated from the data using the method specified by `bounder`.
#' @param bounder Method to use to find missing (`NA`) `bounds`. A function that
#' takes a numeric vector of values and returns a length-2 vector of the estimated
#' lower and upper bound of the distribution. Can also be a string giving the
#' suffix of the name of such a function that starts with `"bounder_"`. Useful
#' values include:
#'  - `"cdf"`: Use the CDF of the the minimum and maximum order statistics of the
#'    sample to estimate the bounds. See [bounder_cdf()].
#'  - `"cooke"`: Use the method from Cooke (1979); i.e. method 2.3 from Loh (1984).
#'    See [bounder_cooke()].
#'  - `"range"`: Use the range of `x` (i.e the `min` or `max`). See [bounder_range()].
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
  trim = TRUE, bounds = c(NA, NA), bounder = "cdf",
  clusters = 1,
  na.rm = FALSE,
  ...,
  range_only = FALSE
) {
  if (missing(x)) return(partial_self("density_bounded"))

  if (n < 1) stop0("density_bounded() must have an n of at least 1")

  x = check_na(x, na.rm)
  if (isTRUE(range_only) && isTRUE(trim)) {
    return(list(x = range(x), y = c(NA_real_, NA_real_)))
  }

  # determine bandwidth
  bw = get_bandwidth(x, bandwidth) * adjust

  # determine bounds
  bounds_to_find = is.na(bounds)
  if (any(bounds_to_find)) {
    bounder = match_function(bounder, "bounder_")
    bounds[bounds_to_find] = bounder(x)[bounds_to_find]
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
  d = .density_adaptive(
    x, weights = weights,
    n = n_unbounded, bw = bw, kernel = kernel,
    from = from, to = to,
    clusters = clusters
  )

  # reflect tails back into middle, if needed
  mid = seq(1 + left_bounded * (n - 1), length.out = n)
  d$x = d$x[mid]
  f = d$y[mid]
  if (left_bounded) f = f + d$y[n:1]
  if (right_bounded) f = f + d$y[seq(length(d$y), by = -1, length.out = n)]

  # trim to data range, if needed
  if (isTRUE(trim) && (bounds[[1]] < min_x || bounds[[2]] > max_x)) {
    x_trimmed = seq.int(min_x, max_x, length.out = n)
    f = approx(d$x, f, x_trimmed)$y
    d$x = x_trimmed
  }

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
#' @param breaks Determines the breakpoints defining bins. Similar to (but not
#' exactly the same as) the `breaks` argument to [graphics::hist()]. One of:
#'   - A scalar (length-1) numeric giving the number of bins
#'   - A vector numeric giving the breakpoints between histogram bins
#'   - A function taking `x` and `weights` and returning either the
#'     number of bins or a vector of breakpoints
#'   - A string giving the suffix of a function that starts with
#'     `"breaks_"`. \pkg{ggdist} provides weighted implementations of the
#'     `"Sturges"`, `"Scott"`, and `"FD"` break-finding algorithms from
#'     [graphics::hist()], as well as [breaks_fixed()] for manually setting
#'     the bin width. See [breaks].
#'
#' For example, `breaks = "Sturges"` will use the [breaks_Sturges()] algorithm,
#' `breaks = 9` will create 9 bins, and `breaks = breaks_fixed(width = 1)` will
#' set the bin width to `1`.
#' @param align Determines how to align the breakpoints defining bins. One of:
#'   - A scalar (length-1) numeric giving an offset that is subtracted from the breaks.
#'     The offset must be between `0` and the bin width.
#'   - A function taking a sorted vector of `breaks` (bin edges) and returning
#'     an offset to subtract from the breaks.
#'   - A string giving the suffix of a function that starts with
#'     `"align_"` used to determine the alignment, such as [align_none()],
#'     [align_boundary()], or [align_center()].
#'
#' For example, `align = "none"` will provide no alignment, `align = align_center(at = 0)`
#' will center a bin on `0`, and `align = align_boundary(at = 0)` will align a bin
#' edge on `0`.
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
  align = "none",
  outline_bars = FALSE,
  na.rm = FALSE,
  ...,
  range_only = FALSE
) {
  if (missing(x)) return(partial_self("density_histogram"))

  x_label = as_label(enexpr(x))
  x = check_na(x, na.rm)

  h = weighted_hist(x, breaks = breaks, align = align)
  input_1 = h$breaks[-length(h$breaks)]  # first edge of bin
  input_2 = h$breaks[-1]                 # second edge of bin
  input_ = (input_1 + input_2)/2   # center of bin

  cdf_fun = weighted_ecdf(x, weights)
  cdf_1 = cdf_fun(input_1)
  cdf_2 = cdf_fun(input_2)
  cdf_ = cdf_fun(input_)

  # need an epsilon value we can use to create ghost values "just above" and
  # "just below" the bin edges, so that logical conditions on fills like `x > 1`
  # work as expected if 1 is a bin edge.
  eps = min(diff(h$breaks)/4, 2*.Machine$double.eps)

  if (!outline_bars) {
    # as.vector(rbind(x, y)) interleaves vectors x and y, giving
    # us the bin endpoints --- then just need to repeat the same value of density
    # for both endpoints of the same bin
    input = as.vector(rbind(input_1, input_1 + eps, input_, input_, input_2 - eps, input_2))
    pdf = rep(h$density, each = 6)
    cdf = as.vector(rbind(cdf_1, cdf_1, cdf_1, cdf_, cdf_2, cdf_2))
  } else {
    # have to return to 0 in between each bar so that bar outlines are drawn
    input = as.vector(rbind(input_1, input_1, input_1 + eps, input_, input_, input_2 - eps, input_2, input_2))
    pdf = as.vector(rbind(0, h$density, h$density, h$density, h$density, h$density, h$density, 0))
    cdf = as.vector(rbind(cdf_1, cdf_1, cdf_1, cdf_1, cdf_, cdf_2, cdf_2, cdf_2))
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


# adaptive density estimator ----------------------------------------------

#' Internal function for calculating adaptive densities
#' Intended as a replacement for stats::density()
#' @noRd
.density_adaptive = function(
  x, weights = NULL,
  n = 512,
  bw = bw.nrd0(x),
  clusters = 5,
  kernel = "gaussian",
  cut = 3,
  from = min(x) - cut*bw,
  to = max(x) + cut*bw
) {
  if (clusters == 1) {
    # quick exit: just return the non-adaptive density
    return(density(
      x, weights = weights,
      n = n, bw = bw,
      kernel = kernel,
      from = from, to = to
    ))
  }

  # determine local adaptive bandwidth
  bw_local = get_local_bandwidth(x, bw, kernel, n)

  # cluster points by their bandwidths
  if (clusters >= length(x)) {
    # one point per group
    bw_group = seq_along(x)
    bws = bw_local
  } else {
    # use k-means clustering to create bandwidth groups
    bw_clusters = kmeans(bw_local, quantile(bw_local, ppoints(clusters)))
    bw_group = bw_clusters$cluster
    bws = bw_clusters$centers
  }

  # calculate densities in each cluster
  n_x = length(x)
  weights = weights %||% rep(1/n_x, length(x))
  densities = lapply(split(data.frame(x, weights, bw_group), bw_group), function(cluster) {
    cluster_weight = sum(cluster$weights)
    d = density(
      cluster$x, weights = cluster$weights/cluster_weight,
      bw = bws[cluster$bw_group[[1]]],
      n = n, from = from, to = to,
      kernel = kernel
    )
    d$y = d$y * cluster_weight
    d
  })
  f = rowSums(vapply(densities, `[[`, "y", FUN.VALUE = numeric(n)))

  structure(list(
    x = densities[[1]]$x,
    y = f,
    bw = bw,
    n = n_x,
    call = match.call(),
    data.name = "x",
    has.na = FALSE
  ), class = "density")
}


get_local_bandwidth = function(x, bandwidth, kernel, n) {
  # evaluate pilot density at each x value
  d_pilot = density(x, bw = bandwidth, kernel = kernel, n = n)
  d_pilot = approx(d_pilot$x, d_pilot$y, xout = x)$y

  # use Abramson's method to calculate local bandwidths
  mean_bandwidth = exp(mean(log(d_pilot[d_pilot > 0])))
  sqrt(mean_bandwidth / d_pilot) * bandwidth
}


# helpers -----------------------------------------------------------------

get_bandwidth = function(x, bandwidth) {
  if (!is.numeric(bandwidth)) {
    bandwidth = match_function(bandwidth, prefix = "bw.")(x)
  }
  bandwidth
}
