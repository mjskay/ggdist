# A stat_summaryh with a geom_intervalh
#
# Author: mjskay
###############################################################################


#' @rdname stat_interval
#' @export
stat_intervalh = function(
  mapping = NULL,
  data = NULL,
  geom = "interval",
  position = "identity",
  ...,

  orientation = "horizontal",
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
    stat = StatIntervalh,
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

StatIntervalh = ggproto("StatIntervalh", StatInterval,
  default_params = defaults(list(
    orientation = "horizontal"
  ), StatInterval$default_params)
)
