# Distribution + interval stat for samples
#
# Author: mjskay
###############################################################################


# slab function for samples -------------------------

#' @importFrom stats approxfun
weighted_ecdf = function(x, weights = NULL) {
  n = length(x)
  if (n < 1) stop("Need at least 1 or more values to calculate an ECDF")

  #sort x
  sort_order = order(x)
  x = x[sort_order]

  # calculate weighted cumulative probabilities
  weights = if (is.null(weights)) rep(1, n) else weights
  weights = weights[sort_order]
  p = cumsum(weights) / sum(weights)

  approxfun(x, p, yleft = 0, yright = 1, ties = list("ordered", max))
}

#' @importFrom rlang missing_arg
#' @importFrom stats ecdf
sample_slab_function = function(
  df, input, slab_type = "pdf", limits = NULL, n = 501, orientation = "vertical",
  adjust = 1, trim = TRUE, breaks = "Sturges", trans = scales::identity_trans(), ...
) {
  x = switch(orientation,
    horizontal = "x",
    vertical = "y"
  )

  switch(slab_type,
    pdf = {
      cut = if (trim) 0 else 3
      # calculate on the transformed scale to ensure density is correct
      d = density(df[[x]], n = n, adjust = adjust, cut = cut)
      data.frame(
        .input = trans$inverse(d$x),
        .value = d$y,
        n = nrow(df)
      )
    },
    cdf = {
      data.frame(
        .input = input,
        .value = weighted_ecdf(df[[x]])(trans$transform(input)),
        n = nrow(df)
      )
    },
    ccdf = {
      data.frame(
        .input = input,
        .value = 1 - weighted_ecdf(df[[x]])(trans$transform(input)),
        n = nrow(df)
      )
    },
    histogram = {
      h = hist(df[[x]], breaks = breaks, plot = FALSE)
      input_1 = h$breaks[-length(h$breaks)]  # first edge of bin
      input_2 = h$breaks[-1]                 # second edge of bin
      data.frame(
        # as.vector(rbind(x, y)) interleaves vectors input_1 and input_2, giving
        # us the bin endpoints --- then just need to repeat the same value of density
        # for both endpoints of the same bin
        .input = trans$inverse(as.vector(rbind(input_1, input_2))),
        .value = rep(h$density, each = 2),
        n = nrow(df)
      )
    }
  )
}


# stat_sample_slabinterval ------------------------------------------------

#' Distribution + interval plots (eye plots, half-eye plots, CCDF barplots, etc) for samples (ggplot stat)
#'
#' Stats for computing densities and CDFs + intervals from samples for use with
#' [geom_slabinterval()]. Useful for creating eye plots, half-eye plots,
#' CCDF bar plots etc.
#'
#' A highly configurable stat for generating a variety of plots that combine a "slab"
#' that summarizes a sample plus an interval. Several "shortcut" stats are provided
#' which combine multiple options to create useful geoms, particularly *eye* plots
#' (a combination of a violin plot and interval), *half-eye* plots (a density plus interval),
#' and *CCDF bar plots* (a complementary CDF plus interval). These can be
#' handy for visualizing posterior distributions in Bayesian inference, amongst other things.
#'
#' The shortcut stat names follow the pattern `stat_[name][h|]`, where the trailing
#' `h` (if present) indicates the horizontal version of the stat.
#'
#' Stats include:
#'
#' \itemize{
#'   \item `stat_eye` / `stat_eyeh`: Eye plots (violin + interval)
#'   \item `stat_halfeye` / `stat_halfeyeh`: Half-eye plots (density + interval)
#'   \item `stat_ccdfinterval` / `stat_ccdfintervalh`: CCDF bar plots (CCDF + interval)
#'   \item `stat_cdfinterval` / `stat_cdfintervalh`: CDF bar plots (CDF + interval)
#'   \item `stat_gradientinterval` / `stat_gradientintervalh`: Density gradient + interval plots
#'   \item `stat_histinterval` / `stat_histintervalh`: Histogram + interval plots
#'   \item `stat_pointinterval` / `stat_pointintervalh`: Point + interval plots
#'   \item `stat_interval` / `stat_intervalh`: Interval plots
#' }
#'
#' @eval rd_slabinterval_aesthetics(stat = StatSampleSlabinterval)
#' @section Computed Variables:
#' \itemize{
#'   \item `x` or `y`: For slabs, the input values to the slab function.
#'     For intervals, the point summary from the interval function. Whether it is `x` or `y` depends on `orientation`
#'   \item `xmin` or `ymin`: For intervals, the lower end of the interval from the interval function.
#'   \item `xmax` or `ymax`: For intervals, the upper end of the interval from the interval function.
#'   \item `f`: For slabs, the output values from the slab function.
#'   \item `n`: For slabs, the number of data points summarized into that slab.
#' }
#'
#' @inheritParams stat_slabinterval
#' @inheritParams geom_slabinterval
#' @inheritParams stat_dist_slabinterval
#' @param slab_type The type of slab function to calculate: probability density (or mass) function (`"pdf"`),
#' cumulative distribution function (`"cdf"`), complementary CDF (`"ccdf"`), or histogram (`"histogram"`.
#' @param adjust If `slab_type` is `"pdf"`, bandwidth for the density estimator is adjusted by multiplying it
#' by this value. See [density()] for more information.
#' @param trim If `slab_type` is `"pdf"`, should the density estimate be trimmed to the range of the
#' input data? Default `TRUE`.
#' @param breaks If `slab_type` is `"histogram"`, the `breaks` parameter that is passed to
#' [hist()] to determine where to put breaks in the histogram.
#' @seealso See [geom_slabinterval()] for more information on the geom these stats
#' use by default and some of the options they have. See [stat_dist_slabinterval()]
#' for the versions of these stats that can be used on analytical distributions.
#' See `vignette("slabinterval")` for a variety of examples of use.
#' @examples
#'
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' # consider the following example data:
#' set.seed(1234)
#' df = tribble(
#'   ~group, ~subgroup, ~value,
#'   "a",          "h", rnorm(500, mean = 5),
#'   "b",          "h", rnorm(500, mean = 7, sd = 1.5),
#'   "c",          "h", rnorm(500, mean = 8),
#'   "c",          "i", rnorm(500, mean = 9),
#'   "c",          "j", rnorm(500, mean = 7)
#' ) %>%
#'   unnest(value)
#'
#' # here are vertical eyes:
#' df %>%
#'   ggplot(aes(x = group, y = value)) +
#'   stat_eye()
#'
#' # note the sample size is not automatically incorporated into the
#' # area of the densities in case one wishes to plot densities against
#' # a reference (e.g. a prior generated by a stat_dist_... function).
#' # But you may wish to account for sample size if using these geoms
#' # for something other than visualizing posteriors; in which case
#' # you can use stat(f*n):
#' df %>%
#'   ggplot(aes(x = group, y = value)) +
#'   stat_eye(aes(thickness = stat(f*n)))
#'
#' # see vignette("slabinterval") for many more examples.
#'
#' @export
stat_sample_slabinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

  slab_type = c("pdf", "cdf", "ccdf", "histogram"),
  adjust = 1,
  trim = TRUE,
  breaks = "Sturges",

  orientation = c("vertical", "horizontal"),
  limits = NULL,
  n = 501,
  interval_function = NULL,
  interval_args = list(),
  point_interval = median_qi,
  .width = c(.66, .95),

  na.rm = FALSE,

  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
  orientation = match.arg(orientation)
  slab_type = match.arg(slab_type)

  layer(
    data = data,
    mapping = mapping,
    stat = StatSampleSlabinterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      slab_type = slab_type,
      adjust = adjust,
      trim = trim,
      breaks = breaks,

      orientation = orientation,

      limits_function = NULL,
      limits_args = list(),
      limits = limits,

      slab_function = sample_slab_function,
      slab_args = list(),
      n = n,

      interval_function = interval_function,
      interval_args = interval_args,
      point_interval = point_interval,
      .width = .width,

      na.rm = na.rm,
      ...
    )
  )
}

StatSampleSlabinterval = ggproto("StatSampleSlabinterval", StatSlabinterval,
  extra_params = c(
    StatSlabinterval$extra_params,
    "slab_type",
    "adjust",
    "trim",
    "breaks"
  ),

  default_params = defaults(list(
    slab_type = "pdf",
    adjust = 1,
    trim = TRUE,
    breaks = "Sturges",

    slab_function = sample_slab_function,
    point_interval = median_qi
  ), StatSlabinterval$default_params),

  setup_params = function(self, data, params) {
    params = ggproto_parent(StatSlabinterval, self)$setup_params(data, params)

    params$slab_args = list(
      slab_type = params$slab_type %||% self$default_params$slab_type,
      adjust = params$adjust %||% self$default_params$adjust,
      trim = params$trim %||% self$default_params$trim,
      breaks = params$breaks %||% self$default_params$breaks
    )

    params
  }
)


# shortcut stats ----------------------------------------------------------

#' @export
#' @rdname stat_sample_slabinterval
stat_halfeye = function(...) stat_sample_slabinterval(...)
#' @export
#' @rdname stat_sample_slabinterval
stat_halfeyeh = function(..., orientation = "horizontal") stat_sample_slabinterval(..., orientation = orientation)

#' @export
#' @rdname stat_sample_slabinterval
stat_eye = function(..., side = "both") stat_sample_slabinterval(..., side = side)
#' @export
#' @rdname stat_sample_slabinterval
stat_eyeh = function(..., side = "both", orientation = "horizontal")
  stat_sample_slabinterval(..., side = side, orientation = orientation)

#' @export
#' @rdname stat_sample_slabinterval
stat_ccdfinterval = function(...,
  slab_type = "ccdf", justification = 0.5, side = "left", normalize = "none"
) {
  stat_sample_slabinterval(..., slab_type = slab_type, justification = justification, side = side, normalize = normalize)
}
#' @export
#' @rdname stat_sample_slabinterval
stat_ccdfintervalh = function(...,
  slab_type = "ccdf", justification = 0.5, side = "top", orientation = "horizontal", normalize = "none"
) {
  stat_sample_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @export
#' @rdname stat_sample_slabinterval
stat_cdfinterval = function(...,
  slab_type = "cdf", justification = 0.5, side = "left", normalize = "none"
) {
  stat_sample_slabinterval(..., slab_type = slab_type, justification = justification, side = side, normalize = normalize)
}
#' @export
#' @rdname stat_sample_slabinterval
stat_cdfintervalh = function(...,
  slab_type = "cdf", justification = 0.5, side = "top", orientation = "horizontal", normalize = "none"
) {
  stat_sample_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @export
#' @rdname stat_sample_slabinterval
stat_gradientinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

  justification = 0.5,
  thickness = 1,

  show.legend = c(size = FALSE, slab_alpha = FALSE),
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatGradientinterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      justification = justification,
      thickness = thickness,
      ...
    )
  )
}
StatGradientinterval = ggproto("StatGradientinterval", StatSampleSlabinterval,
  default_aes = defaults(aes(
    thickness = 1,
    slab_alpha = stat(f)
  ), StatSampleSlabinterval$default_aes),

  default_params = defaults(list(
    justification = 0.5
  ), StatSampleSlabinterval$default_params)
)
#' @export
#' @rdname stat_sample_slabinterval
stat_gradientintervalh = function(..., orientation = "horizontal") {
  stat_gradientinterval(..., orientation = orientation)
}

#' @export
#' @rdname stat_sample_slabinterval
stat_histinterval = function(..., slab_type = "histogram") stat_sample_slabinterval(..., slab_type = slab_type)
#' @export
#' @rdname stat_sample_slabinterval
stat_histintervalh = function(..., slab_type = "histogram" , orientation = "horizontal")
  stat_sample_slabinterval(..., slab_type = slab_type, orientation = orientation)

#' @export
#' @rdname stat_sample_slabinterval
stat_slab = function(
  mapping = NULL,
  data = NULL,
  geom = "slab",
  position = "identity",
  ...,

  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSlab,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      show_point = FALSE,
      show_interval = FALSE,
      ...
    )
  )
}
StatSlab = ggproto("StatSlab", StatSampleSlabinterval,
  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), StatSampleSlabinterval$default_params)
)
StatSlab$default_aes$size = NULL
#' @export
#' @rdname stat_sample_slabinterval
stat_slabh = function(..., orientation = "horizontal") {
  stat_slab(..., orientation = orientation)
}
