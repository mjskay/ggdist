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
  show_point = FALSE,

  datatype = "interval"
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

    datatype = datatype
  )
}

#' @rdname tidybayes-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomIntervalh <- ggproto("GeomIntervalh", GeomSlabinterval,
  default_aes = modifyList(GeomSlabinterval$default_aes, aes(
    datatype = "interval",
    size = 4,
    fill = NA
  )),

  default_params = modifyList(GeomSlabinterval$default_params, list(
    side = "both",
    orientation = "horizontal",
    interval_size_range = c(1, 6),
    show_slab = FALSE,
    show_point = FALSE
  )),

  default_datatype = "interval"
)
