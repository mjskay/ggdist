# A stat_summary with a geom_lineribbon
#
# Author: mjskay
###############################################################################


#' Line + multiple probability ribbon plots (ggplot stat)
#'
#' A combination of [stat_slabinterval()] and [geom_lineribbon()] with sensible defaults.
#' While `geom_lineribbon` is intended for use on data frames that have already been summarized using
#' a [point_interval()] function, `stat_lineribbon` is intended for use directly on data
#' frames of draws, and will perform the summarization using a [point_interval()] function;
#' `stat_dist_lineribbon` is intended for use on analytical distributions through the `dist`,
#' `arg1`, ... `arg9`, and `args` aesthetics.
#'
#' @inheritParams stat_interval
#' @inheritParams stat_dist_slabinterval
#' @param geom Use to override the default connection between
#' `geom_lineribbon` and `stat_lineribbon`.
#' @param show.legend Should this layer be included in the legends? `NA`, the default, includes if any aesthetics
#' are mapped. `FALSE` never includes, and `TRUE` always includes.
#' @return A [ggplot2::Stat] representing a combined line+uncertainty ribbon geometry which can
#' be added to a [ggplot()] object.
#' @seealso See [geom_lineribbon()] for the geom version, intended for use on points and intervals that have
#' already been summarized using a [point_interval()] function. See [stat_pointinterval()]
#' for a similar stat intended for point summaries and intervals.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' library(distributional)
#'
#' tibble(x = 1:10) %>%
#'   group_by_all() %>%
#'   do(tibble(y = rnorm(100, .$x))) %>%
#'   ggplot(aes(x = x, y = y)) +
#'   stat_lineribbon() +
#'   scale_fill_brewer()
#'
#' tibble(
#'   x = 1:10,
#'   sd = seq(1, 3, length.out = 10)
#' ) %>%
#'   ggplot(aes(x = x, dist = dist_normal(x, sd))) +
#'   stat_dist_lineribbon() +
#'   scale_fill_brewer()
#'
#' @name stat_lineribbon
NULL

StatLineribbon = ggproto("StatLineribbon", StatPointinterval,
  default_aes = aes(
    datatype = "interval",
    group = stat(level),
    fill = stat(level)
  ),

  default_params = defaults(list(
    .width = c(.50, .80, .95)
  ), StatPointinterval$default_params),

  layer_args = defaults(list(
    show.legend = NA
  ), StatPointinterval$layer_args),

  orientation_options = defaults(list(
    main_is_orthogonal = NA
  ), StatPointinterval$orientation_options)
)

#' @rdname stat_lineribbon
#' @export
stat_lineribbon = make_stat(StatLineribbon, geom = "lineribbon")


StatDistLineribbon = ggproto("StatDistLineribbon", StatPointinterval,
  default_aes = defaults(aes(
    datatype = "interval",
    group = stat(level),
    fill = stat(level)
  ), StatPointinterval$default_aes),

  default_params = defaults(list(
    .width = c(.50, .80, .95)
  ), StatPointinterval$default_params),

  layer_args = defaults(list(
    show.legend = NA
  ), StatPointinterval$layer_args),

  group_by_dist = FALSE
)
# have to remove this here instead of in call to defaults()
# because otherwise it stays in the list as a value = NULL
# instead of being removed
StatDistLineribbon$default_aes$size = NULL

#' @rdname stat_lineribbon
#' @export
stat_dist_lineribbon = make_stat(StatDistLineribbon, geom = "lineribbon")
