# A stat_summary with a geom_pointinterval
#
# Author: mjskay
###############################################################################


#' @rdname stat_pointinterval
#' @export
stat_pointintervalh <- function(mapping = NULL, data = NULL,
  geom = "pointintervalh", position = "identity",
  ...,
  point_interval = median_qi,
  fun.data = NULL,
  .width = c(.66, .95),
  .prob,
  fun.args = list(),
  na.rm = FALSE,
  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
  .width = .Deprecated_argument_alias(.width, .prob)

  fun.data = fun.data %||% horizontal_aes(point_interval)

  layer(
    data = data,
    mapping = mapping,
    stat = StatPointintervalh,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun.data = fun.data,
      .width = .width,
      fun.args = fun.args,
      na.rm = na.rm,
      ...
    )
  )
}

#' @importFrom plyr defaults
StatPointintervalh <- ggproto("StatPointintervalh", StatSummary,
  default_aes = aes(
    size = stat(-.width)
  ),

  compute_panel = function(data, scales, fun.data = median_qih, .width = c(.66, .95),
    fun.args = list(), na.rm = FALSE
  ) {

    fun.args = modifyList(list(.width = .width), fun.args)

    # Function that takes complete data frame as input
    fun.data = match.fun(fun.data)
    fun = function(df) {
      do.call(fun.data, c(list(quote(df$x)), fun.args))
    }

    data = summarise_by_y(data, fun)
    data$level = forcats::fct_rev(ordered(data$.width))
    data
  }
)
