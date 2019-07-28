# A stat_summary with a geom_lineribbon
#
# Author: mjskay
###############################################################################


#' Line + multiple probability ribbon stat for ggplot
#'
#' A combination of \code{\link{stat_summary}} and \code{\link{geom_lineribbon}} with sensible defaults.
#' While \code{geom_lineribbon} is intended for use on data frames that have already been summarized using
#' a \code{\link{point_interval}} function, \code{stat_lineribbon} is intended for use directly on data
#' frames of draws, and will perform the summarization using a \code{\link{point_interval}} function.
#'
#' @inheritParams stat_interval
#' @param geom Use to override the default connection between
#' \code{geom_lineribbon} and \code{stat_lineribbon}.
#' @param show.legend Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics
#' are mapped. \code{FALSE} never includes, and \code{TRUE} always includes.
#' @seealso See \code{\link{geom_lineribbon}} for the geom version, intended for use on points and intervals that have
#' already been summarized using a \code{\link{point_interval}} function. See \code{\link{stat_pointinterval}} /
#' \code{\link{stat_pointintervalh}} for a similar stat intended for point summaries and intervals.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' tibble(x = 1:10) %>%
#'   group_by_all() %>%
#'   do(tibble(y = rnorm(100, .$x))) %>%
#'   ggplot(aes(x = x, y = y)) +
#'   stat_lineribbon() +
#'   scale_fill_brewer()
#'
#' @export
stat_lineribbon = function(
  mapping = NULL,
  data = NULL,
  geom = "lineribbon",
  position = "identity",
  ...,

  interval_function = NULL,
  interval_args = list(),
  point_interval = median_qi,
  .width = c(.50, .80, .95),
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
    stat = StatLineribbon,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      interval_function = interval_function,
      interval_args = interval_args,
      point_interval = point_interval,
      .width = .width,
      show_slab = FALSE,
      na.rm = na.rm,
      ...
    )
  )
}

StatLineribbon <- ggproto("StatLineribbon", StatPointinterval,
  default_aes = aes(
    datatype = "interval",
    group = stat(level),
    fill = stat(level)
  ),

  default_params = defaults(list(
    .width = c(.50, .80, .95)
  ), StatPointinterval$default_params)
)
