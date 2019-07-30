# A stat_summary with a geom_interval
#
# Author: mjskay
###############################################################################


#' Multiple probability interval plots (ggplot stat)
#'
#' A combination of \code{\link{stat_sample_slabinterval}} and
#' \code{\link{geom_slabinterval}} with sensible defaults.
#' While the corresponding \code{geom}s are intended for use on
#' data frames that have already been summarized using a \code{\link{point_interval}}
#' function, these \code{stat}s are intended for use directly on data frames of draws, and
#' will perform the summarization using a \code{\link{point_interval}} function.
#'
#' @inheritParams stat_pointinterval
#' @inheritParams geom_slabinterval
#' @seealso See \code{\link{geom_interval}} / \code{\link{geom_intervalh}} for the geom versions, intended
#' for use on points and intervals that have already been summarized using a \code{\link{point_interval}} function.
#' See \code{\link{stat_pointinterval}} / \code{\link{stat_pointintervalh}} for a similar stat intended for
#' point summaries and intervals.
#' See \code{\link{stat_sample_slabinterval}} for a variety of other
#' stats that combine intervals with densities and CDFs.
#' See \code{\link{geom_slabinterval}} for the geom that these geoms wrap. All parameters of that geom are
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
