# A stat_summary with a geom_interval
#
# Author: mjskay
###############################################################################


#' Multiple uncertainty interval plots (ggplot stat)
#'
#' A combination of [stat_sample_slabinterval()] and
#' [geom_slabinterval()] with sensible defaults.
#' While the corresponding `geom`s are intended for use on
#' data frames that have already been summarized using a [point_interval()]
#' function, these `stat`s are intended for use directly on data frames of draws, and
#' will perform the summarization using a [point_interval()] function.
#'
#' @eval rd_slabinterval_computed_variables()
#' @eval rd_slabinterval_aesthetics(geom = GeomInterval, geom_name = "geom_interval", stat = StatInterval)
#' @inheritParams stat_pointinterval
#' @inheritParams geom_slabinterval
#' @return A [ggplot2::Stat] representing a multiple interval geometry which can
#' be added to a [ggplot()] object.
#' @seealso See [geom_interval()] for the geom versions, intended
#' for use on points and intervals that have already been summarized using a [point_interval()] function.
#' See [stat_pointinterval()] for a similar stat intended for
#' point summaries and intervals.
#' See [stat_sample_slabinterval()] for a variety of other
#' stats that combine intervals with densities and CDFs.
#' See [geom_slabinterval()] for the geom that these geoms wrap. All parameters of that geom are
#' available to these geoms.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' theme_set(theme_ggdist())
#'
#' data(RankCorr_u_tau, package = "ggdist")
#'
#' RankCorr_u_tau %>%
#'   group_by(i) %>%
#'   ggplot(aes(y = factor(i), x = u_tau)) +
#'   stat_interval() +
#'   scale_color_brewer()
#'
#' RankCorr_u_tau %>%
#'   group_by(i) %>%
#'   ggplot(aes(x = factor(i), y = u_tau)) +
#'   stat_interval() +
#'   scale_color_brewer()
#'
#' @name stat_interval
NULL


#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatInterval = ggproto("StatInterval", StatDistPointinterval,
  default_aes = defaults(aes(
    color = stat(level)
  ), StatDistPointinterval$default_aes),

  default_params = defaults(list(
    show_point = FALSE,
    .width = c(.50, .80, .95)
  ), StatDistPointinterval$default_params),

  layer_args = defaults(list(
    show.legend = NA
  ), StatDistPointinterval$layer_args)
)
# have to remove this here instead of in call to defaults()
# because otherwise it stays in the list as a value = NULL
# instead of being removed
StatInterval$default_aes$size = NULL

#' @rdname stat_interval
#' @export
stat_interval = make_stat(StatInterval, geom = "interval")
