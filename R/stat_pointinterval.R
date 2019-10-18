# A stat_summary with a geom_pointinterval
#
# Author: mjskay
###############################################################################


#' Point summary + multiple probability interval plots (ggplot stat)
#'
#' A combination of [stat_sample_slabinterval()] and
#' [geom_slabinterval()] with sensible defaults.
#' While the corresponding `geom`s are intended for use on
#' data frames that have already been summarized using a [point_interval()]
#' function, these `stat`s are intended for use directly on data frames of draws, and
#' will perform the summarization using a [point_interval()] function.
#'
#' @eval rd_slabinterval_aesthetics(geom = GeomPointinterval, geom_name = "geom_pointinterval", stat = StatPointinterval)
#' @inheritParams stat_sample_slabinterval
#' @inheritParams geom_slabinterval
#' @param .prob Deprecated. Use `.width` instead.
#' @param fun.data Deprecated. Use `point_interval` or `interval_function` instead.
#' @param fun.args Deprecated. Use `interval_args` instead.
#' @seealso See [geom_pointinterval()] / [geom_pointintervalh()] for the geom versions, intended
#' for use on points and intervals that have already been summarized using a [point_interval()] function.
#' See [stat_interval()] / [stat_intervalh()] for a similar stat intended for intervals without
#' point summaries. See [stat_sample_slabinterval()] for a variety of other
#' stats that combine intervals with densities and CDFs.
#' @seealso See [geom_pointinterval()] / [geom_pointintervalh()] for the geom versions, intended
#' for use on points and intervals that have already been summarized using a [point_interval()] function.
#' See [stat_interval()] / [stat_intervalh()] for a similar stat intended for intervals without
#' point summaries.
#' See [stat_sample_slabinterval()] for a variety of other
#' stats that combine intervals with densities and CDFs.
#' See [geom_slabinterval()] for the geom that these geoms wrap. All parameters of that geom are
#' available to these geoms.
#' @examples
#'
#' library(magrittr)
#' library(ggplot2)
#'
#' data(RankCorr, package = "tidybayes")
#'
#' RankCorr %>%
#'   spread_draws(u_tau[i]) %>%
#'   ggplot(aes(y = i, x = u_tau)) +
#'   stat_pointintervalh(.width = c(.66, .95))
#'
#' RankCorr %>%
#'   spread_draws(u_tau[i]) %>%
#'   ggplot(aes(x = i, y = u_tau)) +
#'   stat_pointinterval(.width = c(.66, .95))
#'
#' @export
stat_pointinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "pointinterval",
  position = "identity",
  ...,

  orientation = "vertical",
  interval_function = NULL,
  interval_args = list(),
  point_interval = median_qi,
  .width = c(.66, .95),
  show_slab = FALSE,
  na.rm = FALSE,

  show.legend = c(size = FALSE),
  inherit.aes = TRUE,

  #deprecated arguments
  .prob,
  fun.data,
  fun.args
) {
  interval_function = .Deprecated_argument_alias(interval_function, fun.data)
  interval_args = .Deprecated_argument_alias(interval_args, fun.args)
  .width = .Deprecated_argument_alias(.width, .prob)

  layer(
    data = data,
    mapping = mapping,
    stat = StatPointinterval,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      orientation = orientation,
      interval_function = interval_function,
      interval_args = interval_args,
      point_interval = point_interval,
      .width = .width,
      show_slab = show_slab,
      na.rm = na.rm,
      ...
    )
  )
}

StatPointinterval = ggproto("StatPointinterval", StatSampleSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval"
  ), StatSampleSlabinterval$default_aes),

  default_params = defaults(list(
    show_slab = FALSE
  ), StatSampleSlabinterval$default_params)
)
