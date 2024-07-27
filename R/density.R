# density estimators
#
# Author: mjskay
###############################################################################


# density estimators ------------------------------------------------------

## density_unbounded -------------------------------------------------------
#' Unbounded density estimator
#'
#' Unbounded density estimator using [stats::density()].
#' @template description-auto-partial-waivable
#'
#' @param x <[numeric]> Sample to compute a density estimate for.
#' @param weights <[numeric] | [NULL]> Optional weights to apply to `x`.
#' @param n <scalar [numeric]> The number of grid points to evaluate the density estimator at.
#' @param bandwidth <scalar [numeric] | [function] | [string][character]>
#' Bandwidth of the density estimator. One of:
#'   - a numeric: the bandwidth, as the standard deviation of the kernel
#'   - a function: a function taking `x` (the sample) and returning the bandwidth
#'   - a string: the suffix of the name of a function starting with `"bandwidth_"` that
#'     will be used to determine the bandwidth. See [bandwidth] for a list.
#' @eval rd_param_density_adjust()
#' @param kernel <[string][character]> The smoothing kernel to be used. This must partially
#' match one of `"gaussian"`, `"rectangular"`, `"triangular"`, `"epanechnikov"`,
#' `"biweight"`, `"cosine"`, or `"optcosine"`. See [stats::density()].
#' @eval rd_param_density_trim()
#' @param adapt <positive [integer]> (**very experimental**) The name and interpretation of this argument
#' are subject to change without notice. If `adapt > 1`, uses
#' an adaptive approach to calculate the density. First, uses the
#' adaptive bandwidth algorithm of Abramson (1982) to determine local (pointwise)
#' bandwidths, then groups these bandwidths into `adapt` groups, then calculates
#' and sums the densities from each group. You can set this to a very large number
#' (e.g. `Inf`) for a fully adaptive approach, but this will be very slow; typically
#' something around 100 yields nearly identical results.
#' @param na.rm <scalar [logical]> Should missing (`NA`) values in `x` be removed?
#' @param ... Additional arguments (ignored).
#' @param range_only <scalar [logical]> If `TRUE`, the range of the output of this density estimator
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
density_unbounded = auto_partial(name = "density_unbounded", function(
  x, weights = NULL,
  n = 501, bandwidth = "dpi", adjust = 1, kernel = "gaussian",
  trim = TRUE,
  adapt = 1,
  na.rm = FALSE,
  ...,
  range_only = FALSE
) {
  if (n < 1) cli_abort("{.fun ggdist::density_unbounded} must have an {.arg n} of at least 1")

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
    adapt = adapt
  )

  d$data.name = x_label
  # need to apply get_expr over match.call() instead of just using match.call()
  # to remove tildes from the call created by partial application
  d$call = as.call(lapply(match.call(), get_expr))
  d$cdf = weighted_ecdf(x, weights)(d$x)
  d
})


## density_bounded -------------------------------------------------------
#' Bounded density estimator using the reflection method
#'
#' Bounded density estimator using the reflection method.
#' @template description-auto-partial-waivable
#'
#' @inheritParams density_unbounded
#' @param bounds <length-2 [numeric]> Min and max bounds. If a bound is `NA`, then
#' that bound is estimated from the data using the method specified by `bounder`.
#' @param bounder <[function] | [string][character]> Method to use to find missing
#' (`NA`) `bounds`. A function that
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
density_bounded = auto_partial(name = "density_bounded", function(
  x, weights = NULL,
  n = 501, bandwidth = "dpi", adjust = 1, kernel = "gaussian",
  trim = TRUE, bounds = c(NA, NA), bounder = "cdf",
  adapt = 1,
  na.rm = FALSE,
  ...,
  range_only = FALSE
) {
  if (n < 1) cli_abort("{.fun ggdist::density_bounded} must have an {.arg n} of at least 1")

  x_label = as_label(enexpr(x))
  x = check_na(x, na.rm)
  if (isTRUE(range_only) && isTRUE(trim)) {
    return(list(x = range(x), y = c(NA_real_, NA_real_)))
  }

  # determine bandwidth and bounds
  bw = get_bandwidth(x, bandwidth) * adjust
  left_bounded = right_bounded = NULL
  c(bounds, left_bounded, right_bounded) %<-% get_bounds(x, bw, bounds, bounder)

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
    adapt = adapt
  )

  # reflect tails back into middle, if needed
  mid = seq(1 + left_bounded * (n - 1), length.out = n)
  d$x = d$x[mid]
  f = d$y[mid]
  if (left_bounded) f = f + d$y[n:1]
  if (right_bounded) f = f + d$y[seq(length(d$y), by = -1, length.out = n)]

  # trim to data range, if needed
  range_x = range(x)
  if (isTRUE(trim) && (bounds[[1]] < range_x[[1]] || bounds[[2]] > range_x[[2]])) {
    x_trimmed = seq.int(range_x[[1]], range_x[[2]], length.out = n)
    f = approx(d$x, f, x_trimmed)$y
    d$x = x_trimmed
  }

  d$data.name = x_label
  d$y = f
  d$call = as.call(lapply(match.call(), get_expr))
  d$cdf = weighted_ecdf(x, weights)(d$x)
  d
})


## density_histogram -------------------------------------------------------
#' Histogram density estimator
#'
#' Histogram density estimator.
#' @template description-auto-partial-waivable
#'
#' @inheritParams density_unbounded
#' @eval rd_param_density_breaks()
#' @eval rd_param_density_align()
#' @param outline_bars <scalar [logical]> Should outlines in between the bars (i.e. density values of
#' 0) be included?
#' @param right_closed <scalar [logical]> Should the right edge of each bin be closed? For
#' a bin with endpoints \eqn{L} and \eqn{U}:
#'  - if `TRUE`, use \eqn{(L, U]}: the interval containing all \eqn{x} such that \eqn{L < x \le U}.
#'  - if `FALSE`, use \eqn{[L, U)}: the interval containing all \eqn{x} such that \eqn{L \le x < U}.
#'
#' Equivalent to the `right` argument of [hist()] or the `left.open` argument of [findInterval()].
#' @param outermost_closed <scalar [logical]> Should values on the edges of the outermost (first
#' or last) bins always be included in those bins? If `TRUE`, the first edge (when `right_closed = TRUE`)
#' or the last edge (when `right_closed = FALSE`) is treated as closed.
#'
#' Equivalent to the `include.lowest` argument of [hist()] or the `rightmost.closed` argument of [findInterval()].
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
density_histogram = auto_partial(name = "density_histogram", function(
  x, weights = NULL,
  breaks = "Scott",
  align = "none",
  outline_bars = FALSE,
  right_closed = TRUE,
  outermost_closed = TRUE,
  na.rm = FALSE,
  ...,
  range_only = FALSE
) {
  x_label = as_label(enexpr(x))
  x = check_na(x, na.rm)

  h = weighted_hist(
    x, weights = weights,
    breaks = breaks, align = align,
    right_closed = right_closed, outermost_closed = outermost_closed
  )
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

  if (isTRUE(outline_bars)) {
    # have to return to 0 in between each bar so that bar outlines are drawn
    input = as.vector(rbind(input_1, input_1, input_1 + eps, input_, input_, input_2 - eps, input_2, input_2))
    pdf = as.vector(rbind(0, h$density, h$density, h$density, h$density, h$density, h$density, 0))
    cdf = as.vector(rbind(cdf_1, cdf_1, cdf_1, cdf_1, cdf_, cdf_2, cdf_2, cdf_2))
  } else {
    # as.vector(rbind(x, y)) interleaves vectors x and y, giving
    # us the bin endpoints --- then just need to repeat the same value of density
    # for both endpoints of the same bin
    input = as.vector(rbind(input_1, input_1 + eps, input_, input_, input_2 - eps, input_2))
    pdf = rep(h$density, each = 6)
    cdf = as.vector(rbind(cdf_1, cdf_1, cdf_1, cdf_, cdf_2, cdf_2))
  }

  structure(
    list(
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
    ),
    class = c("ggdist_density", "density")
  )
})


# density object methods --------------------------------------------------

#' @export
plot.ggdist_density = function(x, ..., ylim = c(0, NA)) {
  if (is.null(ylim)) {
    ylim = range(x$y)
  } else {
    missing_limit = is.na(ylim)
    ylim[missing_limit] = range(x$y)[missing_limit]
  }
  NextMethod(ylim = ylim)
}


# bandwidth estimators ----------------------------------------------------

## bandwidth_nrd0 ----------------------------------------------------------
#' Bandwidth estimators
#'
#' Bandwidth estimators for densities, used in the `bandwidth` argument
#' to density functions (e.g. [density_bounded()], [density_unbounded()]).
#' @template description-auto-partial-waivable
#'
#' @inheritDotParams stats::bw.SJ
#' @param x <[numeric]> Vector containing a sample.
#' @details
#' These are loose wrappers around the corresponding `bw.`-prefixed functions
#' in \pkg{stats}. See, for example, [bw.SJ()].
#'
#' [bandwidth_dpi()], which is the default bandwidth estimator in \pkg{ggdist},
#' is the Sheather-Jones direct plug-in estimator, i.e. `bw.SJ(..., method = "dpi")`.
#'
#' With the exception of [bandwidth_nrd0()], these estimators may fail in some
#' cases, often when a sample contains many duplicates. If they do they will
#' automatically fall back to [bandwidth_nrd0()] with a warning. However, these
#' failures are typically symptomatic of situations where you should not want to
#' use a kernel density estimator in the first place (e.g. data with duplicates
#' and/or discrete data). In these cases consider using a dotplot ([geom_dots()])
#' or histogram ([density_histogram()]) instead.
#'
#' @returns A single number giving the bandwidth
#' @seealso [density_bounded()], [density_unbounded()].
#' @name bandwidth
#' @importFrom stats bw.nrd0
#' @export
bandwidth_nrd0 = auto_partial(name = "bandwidth_nrd0", function(x, ...) {
  bw.nrd0(x)
})

## bandwidth_nrd ----------------------------------------------------------
#' @rdname bandwidth
#' @importFrom stats bw.nrd
#' @export
bandwidth_nrd = auto_partial(name = "bandwidth_nrd", function(x, ...) {
  bw_fallback(bw.nrd, x, ..., call = call("bandwidth_nrd"))
})

## bandwidth_ucv ----------------------------------------------------------
#' @rdname bandwidth
#' @importFrom stats bw.ucv
#' @export
bandwidth_ucv = auto_partial(name = "bandwidth_ucv", function(x, ...) {
  bw_fallback(bw.ucv, x, ..., call = call("bandwidth_ucv"))
})

## bandwidth_bcv ----------------------------------------------------------
#' @rdname bandwidth
#' @importFrom stats bw.bcv
#' @export
bandwidth_bcv = auto_partial(name = "bandwidth_bcv", function(x, ...) {
  bw_fallback(bw.bcv, x, ..., call = call("bandwidth_bcv"))
})

## bandwidth_SJ ----------------------------------------------------------
#' @rdname bandwidth
#' @importFrom stats bw.SJ
#' @export
bandwidth_SJ = auto_partial(name = "bandwidth_SJ", function(x, ...) {
  bw_fallback(bw.SJ, x, ..., call = call("bandwidth_SJ"))
})

## bandwidth_dpi ----------------------------------------------------------
#' @rdname bandwidth
#' @export
bandwidth_dpi = auto_partial(name = "bandwidth_dpi", function(x, ...) {
  bw_fallback(bw.SJ, x, method = "dpi", ..., call = call("bandwidth_dpi"))
})


# adaptive density estimator ----------------------------------------------

#' Internal function for calculating adaptive densities
#' Intended as a replacement for stats::density()
#' @noRd
#' @importFrom stats bw.nrd0 kmeans
.density_adaptive = function(
  x, weights = NULL,
  n = 501,
  bw = bw.nrd0(x),
  adapt = 1,
  kernel = "gaussian",
  cut = 3,
  from = min(x) - cut*bw,
  to = max(x) + cut*bw
) {
  if (!is.null(weights)) {
    weights = weights / sum(weights)
  }

  if (adapt == 1) {
    # quick exit: just return the non-adaptive density
    d = density(
      x, weights = weights,
      n = n, bw = bw,
      kernel = kernel,
      from = from, to = to
    )
    class(d) = c("ggdist_density", "density")
    return(d)
  }

  # determine local adaptive bandwidth
  bw_local = get_local_bandwidth(x, bw, kernel, n)

  # cluster points by their bandwidths
  if (adapt >= length(x)) {
    # one point per group
    bw_group = seq_along(x)
    bws = bw_local
  } else {
    ### TODO: figure out a good method here

    ### one possibility:
    # # use k-means clustering to create bandwidth groups
    # > bw_clusters = Ckmeans.1d.dp::Ckmeans.1d.dp(bw_local, adapt)
    # > bw_group = bw_clusters$cluster
    # > bws = bw_clusters$centers

    ### simpler, cut the range of local bandwidths into equally-sized pieces
    ### doesn't work as well though...
    quantiles = quantile(bw_local, seq.int(0, 1, length.out = adapt + 1), names = FALSE)
    bw_group = as.numeric(factor(cut(bw_local, quantiles, labels = FALSE, include.lowest = TRUE)))
    bws = tapply(bw_local, bw_group, mean)
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

  structure(
    list(
      x = densities[[1]]$x,
      y = f,
      bw = bw,
      n = n_x,
      call = match.call(),
      data.name = "x",
      has.na = FALSE
    ),
    class = c("ggdist_density", "density")
  )
}

get_local_bandwidth = function(x, bandwidth, kernel, n) {
  # TODO: allow this to be done using a bounded KDE for density_bounded

  # evaluate pilot density at each x value
  d_pilot = density(x, bw = bandwidth, kernel = kernel, n = n)
  d_pilot_times_n = approx(d_pilot$x, d_pilot$y, xout = x)$y * length(x)

  # use Abramson's method to calculate local bandwidths
  mean_bandwidth = exp(mean(log(d_pilot_times_n[d_pilot_times_n > 0])))
  sqrt(mean_bandwidth / d_pilot_times_n) * bandwidth
}


# helpers -----------------------------------------------------------------

get_bandwidth = function(x, bandwidth) {
  if (!is.numeric(bandwidth)) {
    bandwidth = match_function(bandwidth, prefix = "bandwidth_")(x)
  }
  bandwidth
}

#' Get the bounds for a bounded density estimator
#' @param x data
#' @param bw bandwidth
#' @param bounds user-specified 2-vector of bounds
#' @param bounder bounder function as passed to density_bounded (e.g. function or name)
#' @returns list with:
#'   - `bounds`: 2-vector of bounds (guaranteed finite and non-`NA`)
#'   - `left_bounded`: `TRUE` if bounded below
#'   - `right_bounded`: `TRUE` if bounded above
#' @noRd
get_bounds = function(x, bw, bounds, bounder, call = caller_env()) {
  stopifnot(length(bounds) == 2, is.numeric(bounds) || is.logical(bounds))
  bounds_to_find = is.na(bounds)
  if (any(bounds_to_find)) {
    bounder = match_function(bounder, "bounder_")
    bounds[bounds_to_find] = bounder(x)[bounds_to_find]
  }

  range_x = range(x)
  if (range_x[[1]] < bounds[[1]] || range_x[[2]] > bounds[[2]]) {
    cli_abort("All {.arg x} must be inside {.arg bounds}", call = call)
  }

  # cap bounds at 3*bw beyond the range (and consider them unbounded beyond
  # that, since past that the density essentially goes to 0)
  min_bound = range_x[[1]] - 3 * bw
  max_bound = range_x[[2]] + 3 * bw
  left_bounded = min_bound <= bounds[[1]]
  right_bounded = bounds[[2]] <= max_bound
  if (!left_bounded) bounds[[1]] = min_bound
  if (!right_bounded) bounds[[2]] = max_bound

  list(bounds = bounds, left_bounded = left_bounded, right_bounded = right_bounded)
}

#' run a bandwidth calculation, catching errors and providing a fallback
#' @param bw a function used to calculate bandwidth
#' @param x data to calculate bandwidth of
#' @param ... additional arguments passed to bw
#' @importFrom rlang caller_env eval_tidy expr enquo
#' @noRd
bw_fallback = function(f, x, ..., call = caller_env()) {
  tryCatch({
    # use tidy eval here instead of bw = f(x, ...) to improve error messages
    bw = eval_tidy(expr((!!enquo(f))(x, ...)))
    if (bw <= 0) stop0("bandwidth is not positive")
    bw
  }, error = function(e) {
    cli_warn(
      c(
        "Bandwidth calculation failed.",
        ">" = "Falling back to {.fun bandwidth_nrd0}.",
        "i" = "This often occurs when a sample contains many duplicates, which
               suggests that a dotplot (e.g., {.fun geom_dots}) or histogram
               (e.g., {.fun density_histogram}, {.code stat_slab(density = 'histogram')},
               or {.fun stat_histinterval}) may better represent the data."
      ),
      class = "ggdist_bandwidth_fallback_warning",
      call = call,
      parent = e
    )
    bandwidth_nrd0(x)
  })
}
