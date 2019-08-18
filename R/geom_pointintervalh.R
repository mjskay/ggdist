# A horizontal version of geom_pointinterval
#
# Author: mjskay
###############################################################################



#' @rdname geom_pointinterval
#' @import ggplot2
#' @export
geom_pointintervalh = function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,

  side = "both",
  orientation = "horizontal",
  show_slab = FALSE,

  show.legend = c(size = FALSE)
) {

  layer_geom_slabinterval(
    data = data,
    mapping = mapping,
    default_mapping = aes(xmin = .lower, xmax = .upper, size = -.width),
    stat = stat,
    geom = GeomPointintervalh,
    position = position,
    ...,

    side = side,
    orientation = orientation,
    show_slab = show_slab,

    datatype = "interval",

    show.legend = show.legend
  )
}

#' @rdname tidybayes-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomPointintervalh = ggproto("GeomPointintervalh", GeomSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval"
  ), GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    fill = NA
  ), GeomSlabinterval$default_key_aes),

  default_params = defaults(list(
    side = "both",
    orientation = "horizontal",
    show_slab = FALSE
  ), GeomSlabinterval$default_params),

  default_datatype = "interval"
)
