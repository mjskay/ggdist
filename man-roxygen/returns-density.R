#' @returns
#' An object of class `"density"`, mimicking the output format of
#' [stats::density()], with the following components:
#'
#'   - `x`: The grid of points at which the density was estimated.
#'   - `y`: The estimated density values.
#'   - `bw`: The bandwidth.
#'   - `n`: The sample size of the `x` input argument.
#'   - `call`: The call used to produce the result, as a quoted expression.
#'   - `data.name`: The deparsed name of the `x` input argument.
#'   - `has.na`: Always `FALSE` (for compatibility).
#'   - `cdf`: Values of the (possibly weighted) empirical cumulative distribution
#'     function at `x`. See [weighted_ecdf()].
#'
#' This allows existing methods for density objects, like [print()] and [plot()], to work if desired.
#' This output format (and in particular, the `x` and `y` components) is also
#' the format expected by the `density` argument of the [stat_slabinterval()]
#' and the [`smooth_`][smooth_density] family of functions.
