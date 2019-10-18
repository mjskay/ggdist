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
#' @inheritParams stat_slabinterval
#' @param geom Use to override the default connection between
#' `geom_lineribbon` and `stat_lineribbon`.
#' @param show.legend Should this layer be included in the legends? `NA`, the default, includes if any aesthetics
#' are mapped. `FALSE` never includes, and `TRUE` always includes.
#' @seealso See [geom_lineribbon()] for the geom version, intended for use on points and intervals that have
#' already been summarized using a [point_interval()] function. See [stat_pointinterval()] /
#' [stat_pointintervalh()] for a similar stat intended for point summaries and intervals.
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
#' tibble(
#'   x = 1:10,
#'   sd = seq(1, 3, length.out = 10)
#' ) %>%
#'   ggplot(aes(x = x, dist = "norm", arg1 = x, arg2 = sd)) +
#'   stat_dist_lineribbon() +
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

StatLineribbon = ggproto("StatLineribbon", StatPointinterval,
  default_aes = aes(
    datatype = "interval",
    group = stat(level),
    fill = stat(level)
  ),

  default_params = defaults(list(
    .width = c(.50, .80, .95)
  ), StatPointinterval$default_params)
)


#' @rdname stat_lineribbon
#' @export
stat_dist_lineribbon = function(
  mapping = NULL,
  data = NULL,
  geom = "lineribbon",
  position = "identity",
  ...,

  n = 501,
  .width = c(.50, .80, .95),

  na.rm = FALSE,

  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDistLineribbon,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      orientation = "vertical",

      limits_function = dist_limits_function,
      limits_args = list(),

      slab_function = dist_slab_function,
      slab_args = list(),
      n = n,

      interval_function = dist_interval_function,
      interval_args = list(),
      point_interval = NULL,
      .width = .width,

      show_slab = FALSE,
      show_interval = TRUE,

      na.rm = na.rm,
      ...
    )
  )
}

StatDistLineribbon = ggproto("StatDistLineribbon", StatDistSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval",
    group = stat(level),
    fill = stat(level)
  ), StatDistSlabinterval$default_aes),

  default_params = defaults(list(
    show_slab = FALSE,
    .width = c(.50, .80, .95)
  ), StatDistSlabinterval$default_params),

  group_by_dist = FALSE
)
# have to remove this here instead of in call to defaults()
# because otherwise it stays in the list as a value = NULL
# instead of being removed
StatDistLineribbon$default_aes$size = NULL
