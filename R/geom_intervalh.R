# A multiple interval geom (horizontal)
#
# Author: mjskay
###############################################################################



#' @rdname geom_interval
#' @import ggplot2
#' @export
geom_intervalh = function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,

  side = "both",
  orientation = "horizontal",
  interval_size_range = c(1, 6),
  show_slab = FALSE,
  show_point = FALSE
) {

  layer_geom_slabinterval(
    data = data,
    mapping = mapping,
    default_mapping = aes(xmin = .lower, xmax = .upper, color = forcats::fct_rev(ordered(.width))),
    stat = stat,
    geom = GeomIntervalh,
    position = position,
    ...,

    side = side,
    orientation = orientation,
    interval_size_range = interval_size_range,
    show_slab = show_slab,
    show_point = show_point,

    datatype = "interval"
  )
}

#' @rdname tidybayes-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomIntervalh = ggproto("GeomIntervalh", GeomSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval"
  ), GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    size = 4,
    fill = NA
  ), GeomSlabinterval$default_key_aes),

  default_params = defaults(list(
    side = "both",
    orientation = "horizontal",
    interval_size_range = c(1, 6),
    show_slab = FALSE,
    show_point = FALSE
  ), GeomSlabinterval$default_params),

  default_datatype = "interval"
)
