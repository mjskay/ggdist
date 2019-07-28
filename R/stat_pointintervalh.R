# Horizontal pointinterval stat
#
# Author: mjskay
###############################################################################


#' @rdname stat_pointinterval
#' @export
stat_pointintervalh = function(
  mapping = NULL,
  data = NULL,
  geom = "pointintervalh",
  position = "identity",
  ...,

  orientation = "horizontal",
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
    stat = StatPointintervalh,
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

StatPointintervalh = ggproto("StatPointintervalh", StatPointinterval,
  default_params = defaults(list(
    orientation = "horizontal"
  ), StatPointinterval$default_params)
)

