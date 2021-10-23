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

  # need to manually do tie removal before passing to approxfun, otherwise it
  # will fail when all x values are equal
  unique_x = unique(x)
  if (length(unique_x) < length(x)) {
    # if x[i] ... x[i + k] are all equal ("tied"), collapse to a single x
    # value and let corresponding value in p = max(p[i] ... p[i + k])
    p = as.vector(tapply(p, match(x, x), max))
    x = unique_x
    stopifnot(length(p) == length(x))
  }

  method = "constant"

  approxfun(x, p, yleft = 0, yright = 1, ties = "ordered", method = method)
}

sample_density = function(x, ...) {
  if (length(unique(x)) == 1) {
    list(x = x[[1]], y = Inf)
  } else {
    density(x, ...)
  }
}

#' @importFrom rlang missing_arg
#' @importFrom stats ecdf density
#' @importFrom graphics hist
compute_slab_sample = function(
  data, input, trans,
  slab_type = "pdf", limits = NULL, n = 501, orientation = NA,
  adjust = 1, trim = TRUE, breaks = "Sturges", outline_bars = FALSE,
  ...
) {
  x = switch(orientation,
    y = ,
    horizontal = "x",
    x = ,
    vertical = "y"
  )

  # calculate the density first, since we'll use the x values from it
  # to calculate the cdf
  slab_df = if (slab_type == "histogram") {
    # when using a histogram slab, that becomes the density function value
    # TODO: this is a hack for now until we make it so that density estimators
    # can be swapped out (which would be a better solution)
    h = hist(data[[x]], breaks = breaks, plot = FALSE)
    input_1 = h$breaks[-length(h$breaks)]  # first edge of bin
    input_2 = h$breaks[-1]                 # second edge of bin

    if (!outline_bars) {
      # as.vector(rbind(x, y)) interleaves vectors input_1 and input_2, giving
      # us the bin endpoints --- then just need to repeat the same value of density
      # for both endpoints of the same bin
      .input = trans$inverse(as.vector(rbind(input_1, input_2)))
      .value = rep(h$density, each = 2)
    } else {
      # have to return to 0 in between each bar so that bar outlines are drawn
      .input = trans$inverse(as.vector(rbind(input_1, input_1, input_2, input_2)))
      .value = as.vector(rbind(0, h$density, h$density, 0))
    }
    data.frame(
      .input = .input,
      pdf = .value
    )
  } else {
    # all other slab types use the density function as the pdf
    cut = if (trim) 0 else 3
    # calculate on the transformed scale to ensure density is correct
    d = sample_density(data[[x]], n = n, adjust = adjust, cut = cut)
    data.frame(
      .input = trans$inverse(d$x),
      pdf = d$y
    )
  }

  # calculate cdf
  trans_input = trans$transform(slab_df$.input)
  cdf_fun = weighted_ecdf(data[[x]])
  slab_df[["cdf"]] = cdf_fun(trans_input)

  if (slab_type == "cdf" || slab_type == "ccdf") {
    # for CDF and CCDF slabs we have to make sure the x values extend to the
    # range of the plot. To do that we have to include x values requested
    # from the original `input` if they are outside the range of the slab
    input_below_slab = input[input < min(slab_df$.input)]
    if (length(input_below_slab) > 0) {
      slab_df = rbind(data.frame(
        .input = input_below_slab,
        pdf = 0,
        cdf = 0
      ), slab_df)
    }

    input_above_slab = input[input > max(slab_df$.input)]
    if (length(input_above_slab) > 0) {
      slab_df = rbind(slab_df, data.frame(
        .input = input_above_slab,
        pdf = 0,
        cdf = 1
      ))
    }
  }

  slab_df[[".value"]] = switch(slab_type,
    histogram = ,
    pdf = slab_df$pdf,
    cdf = slab_df$cdf,
    ccdf = 1 - slab_df$cdf
  )

  slab_df$n = nrow(data)
  slab_df
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
#' The shortcut stat names follow the pattern `stat_[name]`.
#'
#' Stats include:
#'
#' \itemize{
#'   \item `stat_eye`: Eye plots (violin + interval)
#'   \item `stat_halfeye`: Half-eye plots (density + interval)
#'   \item `stat_ccdfinterval`: CCDF bar plots (CCDF + interval)
#'   \item `stat_cdfinterval`: CDF bar plots (CDF + interval)
#'   \item `stat_gradientinterval`: Density gradient + interval plots
#'   \item `stat_histinterval`: Histogram + interval plots
#'   \item `stat_pointinterval`: Point + interval plots
#'   \item `stat_interval`: Interval plots
#' }
#'
#' @eval rd_slabinterval_computed_variables(stat_sample = TRUE)
#' @eval rd_slabinterval_aesthetics(stat = StatSampleSlabinterval)
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
#' @param outline_bars If `slab_type` is `"histogram"`, `outline_bars` determines if outlines in between
#' the bars are drawn when the `slab_color` aesthetic is used. If `FALSE` (the default), the outline
#' is drawn only along the tops of the bars; if `TRUE`, outlines in between bars are also drawn.
#' @return A [ggplot2::Stat] representing a slab or combined slab+interval geometry which can
#' be added to a [ggplot()] object.
#' @seealso See [geom_slabinterval()] for more information on the geom these stats
#' use by default and some of the options they have. See [stat_dist_slabinterval()]
#' for the versions of these stats that can be used on analytical distributions.
#' See `vignette("slabinterval")` for a variety of examples of use.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' # consider the following example data:
#' set.seed(1234)
#' df = data.frame(
#'   group = c("a", "b", "c", "c", "c"),
#'   value = rnorm(2500, mean = c(5, 7, 9, 9, 9), sd = c(1, 1.5, 1, 1, 1))
#' )
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
#'   stat_eye(aes(thickness = stat(pdf*n)))
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
  outline_bars = FALSE,

  orientation = NA,
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
      outline_bars = outline_bars,

      orientation = orientation,
      limits = limits,
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

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSampleSlabinterval = ggproto("StatSampleSlabinterval", StatSlabinterval,
  extra_params = c(
    StatSlabinterval$extra_params,
    "slab_type",
    "adjust",
    "trim",
    "breaks",
    "outline_bars"
  ),

  default_params = defaults(list(
    slab_type = "pdf",
    adjust = 1,
    trim = TRUE,
    breaks = "Sturges",
    outline_bars = FALSE,

    point_interval = median_qi
  ), StatSlabinterval$default_params),

  compute_slab = compute_slab_sample
)


# shortcut stats ----------------------------------------------------------

#' @export
#' @rdname stat_sample_slabinterval
stat_halfeye = function(...) stat_sample_slabinterval(...)

#' @export
#' @rdname stat_sample_slabinterval
stat_eye = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatEye,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      ...
    )
  )
}
StatEye = ggproto("StatEye", StatSampleSlabinterval,
  default_aes = defaults(aes(
    side = stat("both"),
  ), StatSampleSlabinterval$default_aes)
)


#' @export
#' @rdname stat_sample_slabinterval
stat_ccdfinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

  slab_type = "ccdf",
  normalize = "none",

  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatCcdfinterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      slab_type = slab_type,
      normalize = normalize,
      ...
    )
  )
}
StatCcdfinterval = ggproto("StatCcdfinterval", StatSampleSlabinterval,
  default_aes = defaults(aes(
    justification = stat(0.5),
    side = stat("topleft"),
  ), StatSampleSlabinterval$default_aes),

  default_params = defaults(list(
    slab_type = "ccdf",
    normalize = "none"
  ), StatSampleSlabinterval$default_params)
)

#' @export
#' @rdname stat_sample_slabinterval
stat_cdfinterval = function(...,
  slab_type = "cdf", normalize = "none"
) {
  stat_ccdfinterval(..., slab_type = slab_type, normalize = normalize)
}

#' @export
#' @rdname stat_sample_slabinterval
stat_gradientinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

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
      ...
    )
  )
}
StatGradientinterval = ggproto("StatGradientinterval", StatSampleSlabinterval,
  default_aes = defaults(aes(
    justification = stat(0.5),
    thickness = stat(1),
    slab_alpha = stat(f)
  ), StatSampleSlabinterval$default_aes)
)

#' @export
#' @rdname stat_sample_slabinterval
stat_histinterval = function(..., slab_type = "histogram") {
  stat_sample_slabinterval(..., slab_type = slab_type)
}

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
