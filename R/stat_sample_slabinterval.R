# Distribution + interval stat for samples
#
# Author: mjskay
###############################################################################


# slab function for samples -------------------------

#' @importFrom rlang missing_arg
#' @importFrom stats ecdf
sample_slab_function = function(
  df, input, slab_type = "pdf", limits = NULL, n = 501, orientation = "vertical",
  adjust = 1, trim = TRUE, breaks = "Sturges", ...
) {
  x = switch(orientation,
    horizontal = "x",
    vertical = "y"
  )

  switch(slab_type,
    pdf = {
      cut = if (trim) 0 else 3
      d = density(df[[x]], n = n, adjust = adjust, cut = cut)
      data.frame(
        .input = d$x,
        .value = d$y
      )
    },
    cdf = {
      data.frame(
        .input = input,
        .value = ecdf(df[[x]])(input)
      )
    },
    ccdf = {
      data.frame(
        .input = input,
        .value = 1 - ecdf(df[[x]])(input)
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
        .input = as.vector(rbind(input_1, input_2)),
        .value = rep(h$density, each = 2)
      )
    }
  )
}


# stat_sample_slabinterval ------------------------------------------------

#' Distribution + interval plots (eye plots, half-eye plots, CCDF barplots, etc) for samples (ggplot stat)
#'
#' Stats for computing densities and CDFs + intervals from samples for use with
#' \code{\link{geom_slabinterval}}. Useful for creating eye plots, halfeye plots,
#' CCDF bar plots etc.
#'
#' A highly configurable stat for generating a variety of plots that combine a "slab"
#' that summarizes a sample plus an interval. Several "shortcut" stats are provided
#' which combine multiple options to create useful geoms, particularly \emph{eye} plots
#' (a combination of a violin plot and interval), \emph{half-eye} plots (a density plus interval),
#' and \emph{CCDF bar plots} (a complementary CDF plus interval). These can be
#' handy for visualizing posterior distributions in Bayesian inference, amongst other things.
#'
#' The shortcut stat names follow the pattern \code{stat_[name][h|]}, where the trailing
#' \code{h} (if present) indicates the horizontal version of the stat.
#'
#' Stats include:
#'
#' \itemize{
#'   \item \code{stat_eye} / \code{stat_eyeh}: Eye plots (violin + interval)
#'   \item \code{stat_halfeye} / \code{stat_halfeyeh}: Half-eye plots (density + interval)
#'   \item \code{stat_ccdfinterval} / \code{stat_ccdfintervalh}: CCDF bar plots (CCDF + interval)
#'   \item \code{stat_cdfinterval} / \code{stat_cdfintervalh}: CDF bar plots (CDF + interval)
#'   \item \code{stat_gradientinterval} / \code{stat_gradientintervalh}: Density gradient + interval plots
#' }
#'
#' @inheritParams stat_slabinterval
#' @inheritParams geom_slabinterval
#' @inheritParams stat_dist_slabinterval
#' @param slab_type The type of slab function to calculate: probability density (or mass) function (\code{"pdf"}),
#' cumulative distribution function (\code{"cdf"}), complementary CDF (\code{"ccdf"}), or histogram (\code{"histogram"}.
#' @param adjust If \code{slab_type} is \code{"pdf"}, bandwidth for the density estimator is adjusted by multiplying it
#' by this value. See \code{\link{density}} for more information.
#' @param trim If \code{slab_type} is \code{"pdf"}, should the density estimate be trimmed to the range of the
#' input data? Default \code{TRUE}.
#' @param breaks If \code{slab_type} is \code{"histogram"}, the \code{breaks} parameter that is passed to
#' \code{\link{hist}} to determine where to put breaks in the histogram.
#' @seealso See \code{\link{geom_slabinterval}} for more information on the geom these stats
#' use by default and some of the options they have. See \code{\link{stat_dist_slabinterval}}
#' for the versions of these stats that can be used on analytical distributions.
#' @examples
#'
#' #TODO
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

StatSampleSlabinterval <- ggproto("StatSampleSlabinterval", StatSlabinterval,
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

  show.legend = c(size = FALSE),
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
#' @export
#' @rdname stat_sample_slabinterval
stat_gradientintervalh = function(..., orientation = "horizontal") {
  stat_gradientinterval(..., orientation = orientation)
}
StatGradientinterval <- ggproto("StatGradientinterval", StatSampleSlabinterval,
  default_aes = defaults(aes(
    alpha = stat(f)
  ), StatSampleSlabinterval$default_aes)
)

#' @export
#' @rdname stat_sample_slabinterval
stat_histinterval = function(..., slab_type = "histogram") stat_sample_slabinterval(..., slab_type = slab_type)
#' @export
#' @rdname stat_sample_slabinterval
stat_histintervalh = function(..., slab_type = "histogram" , orientation = "horizontal")
  stat_sample_slabinterval(..., slab_type = slab_type, orientation = orientation)
