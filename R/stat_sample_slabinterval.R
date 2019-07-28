# Distribution + interval stat for samples
#
# Author: mjskay
###############################################################################


# slab function for samples -------------------------

sample_slab_function = function(
  df, input, slab_type = "pdf", limits = NULL, n = 501, orientation = "vertical", ...
) {
  x = switch(orientation,
    horizontal = "x",
    vertical = "y"
  )

  switch(slab_type,
    pdf = {
      d = density(df[[x]], n = n)
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
    }
  )
}


# stat_sample_slabinterval ------------------------------------------------

#' Distribution + interval plots (eye plots, half-eye plots, CCDF barplots, etc) for samples (ggplot stat)
#'
#' Stats for computing densities and CDFs + intervals from samples for use with
#' \code{\link{geom_slabinterval}}.
#'
#' @inheritParams stat_slabinterval
#' @param slab_type The type of slab function to calculate: probability density (or mass) function (\code{"pdf"}),
#' cumulative distribution function (\code{"cdf"}), or complementary CDF (\code{"ccdf"}).
#' @seealso See \code{\link{geom_slabinterval}} for more information on the geom these stats
#' use by default and some of the options they has.
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

  slab_type = c("pdf", "cdf", "ccdf"),

  orientation = c("vertical", "horizontal"),
  limits = NULL,
  n = 501,
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

      orientation = orientation,

      limits_function = NULL,
      limits_args = list(),
      limits = limits,

      slab_function = sample_slab_function,
      slab_args = list(),
      n = n,

      interval_function = NULL,
      interval_args = list(),
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
    "slab_type"
  ),

  default_params = defaults(list(
    slab_type = "pdf",

    slab_function = sample_slab_function,
    point_interval = median_qi
  ), StatSlabinterval$default_params),

  setup_params = function(self, data, params) {
    params = ggproto_parent(StatSlabinterval, self)$setup_params(data, params)

    params$slab_args = list(
      slab_type = params$slab_type %||% self$default_params$slab_type
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
stat_ccdfbar = function(...,
  slab_type = "ccdf", justification = 0.5, side = "left", normalize = "none"
) {
  stat_sample_slabinterval(..., slab_type = slab_type, justification = justification, side = side, normalize = normalize)
}
#' @export
#' @rdname stat_sample_slabinterval
stat_ccdfbarh = function(...,
  slab_type = "ccdf", justification = 0.5, side = "top", orientation = "horizontal", normalize = "none"
) {
  stat_sample_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @export
#' @rdname stat_sample_slabinterval
stat_cdfbar = function(...,
  slab_type = "cdf", justification = 0.5, side = "left", normalize = "none"
) {
  stat_sample_slabinterval(..., slab_type = slab_type, justification = justification, side = side, normalize = normalize)
}
#' @export
#' @rdname stat_sample_slabinterval
stat_cdfbarh = function(...,
  slab_type = "cdf", justification = 0.5, side = "top", orientation = "horizontal", normalize = "none"
) {
  stat_sample_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}
