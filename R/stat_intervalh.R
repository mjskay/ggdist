# A stat_summaryh with a geom_intervalh
#
# Author: mjskay
###############################################################################


#' @rdname stat_interval
#' @export
stat_intervalh <- function(mapping = NULL, data = NULL,
  geom = "intervalh", position = "identity",
  ...,
  point_interval = median_qi,
  fun.data = NULL,
  .width = c(.50, .80, .95),
  .prob,
  fun.args = list(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  .width = .Deprecated_argument_alias(.width, .prob)

  # Probs are drawn on top of each other in order by geom_intervalh, so we have to sort in decreasing order
  # to make sure the largest interval is not drawn last (over-writing all other intervals)
  .width %<>% sort(decreasing = TRUE)

  fun.data = fun.data %||% horizontal_aes(point_interval)

  layer(
    data = data,
    mapping = mapping,
    stat = StatIntervalh,
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

StatIntervalh <- ggproto("StatIntervalh", StatPointintervalh,
  default_aes = aes(
    datatype = "interval",
    color = stat(level)
  )
)
