# A stat_summary with a geom_interval
#
# Author: mjskay
###############################################################################


#' Multiple probability interval plots (ggplot stat)
#'
#' A combination of [stat_sample_slabinterval()] and
#' [geom_slabinterval()] with sensible defaults.
#' While the corresponding `geom`s are intended for use on
#' data frames that have already been summarized using a [point_interval()]
#' function, these `stat`s are intended for use directly on data frames of draws, and
#' will perform the summarization using a [point_interval()] function.
#'
#' @eval rd_slabinterval_aesthetics(geom = GeomInterval, geom_name = "geom_interval", stat = StatInterval)
#' @inheritParams stat_pointinterval
#' @inheritParams geom_slabinterval
#' @seealso See [geom_interval()] / [geom_intervalh()] for the geom versions, intended
#' for use on points and intervals that have already been summarized using a [point_interval()] function.
#' See [stat_pointinterval()] / [stat_pointintervalh()] for a similar stat intended for
#' point summaries and intervals.
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
#'   stat_intervalh() +
#'   scale_color_brewer()
#'
#' RankCorr %>%
#'   spread_draws(u_tau[i]) %>%
#'   ggplot(aes(x = i, y = u_tau)) +
#'   stat_interval() +
#'   scale_color_brewer()
#'
#' @export
stat_interval = function(
  mapping = NULL,
  data = NULL,
  geom = "interval",
  position = "identity",
  ...,

  orientation = "vertical",
  interval_function = NULL,
  interval_args = list(),
  point_interval = median_qi,
  .width = c(.50, .80, .95),
  show_point = FALSE,
  show_slab = FALSE,
  na.rm = FALSE,

  show.legend = NA,
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
    stat = StatInterval,
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
      show_point = show_point,
      show_slab = show_slab,
      na.rm = na.rm,
      ...
    )
  )
}

StatInterval = ggproto("StatInterval", StatPointinterval,
  default_aes = defaults(aes(
    color = stat(level)
  ), StatPointinterval$default_aes),

  default_params = defaults(list(
    show_point = FALSE,
    .width = c(.50, .80, .95)
  ), StatPointinterval$default_params)
)
# have to remove this here instead of in call to defaults()
# because otherwise it stays in the list as a value = NULL
# instead of being removed
StatInterval$default_aes$size = NULL
