# A geom_pointrangeh but with sensible defaults for displaying multiple intervals
#
# Author: mjskay
###############################################################################



#' @rdname geom_pointinterval
#' @import ggplot2
#' @export
geom_pointintervalh <- function(mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  ...,
  size_domain = c(1, 6),
  size_range = c(0.6, 1.4),
  fatten_point = 1.8,
  na.rm = FALSE,
  show.legend = c(size = FALSE),
  inherit.aes = TRUE) {

  l = layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointintervalh,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      size_domain = size_domain,
      size_range = size_range,
      fatten_point = fatten_point,
      na.rm = na.rm,
      ...
    )
  )

  #provide some default computed aesthetics
  default_computed_aesthetics = aes(xmin = .lower, xmax = .upper, size = -.width)

  compute_aesthetics = l$compute_aesthetics
  l$compute_aesthetics = function(self, data, plot) {
    apply_default_computed_aesthetics(self, plot, default_computed_aesthetics)
    compute_aesthetics(data, plot)
  }

  map_statistic = l$map_statistic
  l$map_statistic = function(self, data, plot) {
    apply_default_computed_aesthetics(self, plot, default_computed_aesthetics)
    map_statistic(data, plot)
  }

  l
}

#' @rdname tidybayes-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom grid grobName gTree gList
#' @import ggplot2
#' @export
GeomPointintervalh <- ggproto("GeomPointintervalh", Geom,
  default_aes = aes(colour = "black", size = 1.35, linetype = 1, shape = 19,
    fill = NA, alpha = NA, stroke = 1),

  draw_key = draw_key_pointinterval,

  required_aes = c("x", "y", "xmin", "xmax"),

  draw_panel = function(
      data, panel_scales, coord, size_domain = c(1, 6), size_range = c(0.6, 1.4), fatten_point = 1.8
    ) {

    line_data = transform(data,
      size = pmax(
        (size - size_domain[[1]]) / (size_domain[[2]] - size_domain[[1]]) *
        (size_range[[2]] - size_range[[1]]) + size_range[[1]],
        0)
    )

    if (is.null(data$x)) {
      return(GeomIntervalh$draw_panel(line_data, panel_scales, coord))
    }

    ggname("geom_pointintervalh",
      gTree(children = gList(
        GeomIntervalh$draw_panel(line_data, panel_scales, coord),
        GeomPoint$draw_panel(transform(line_data, size = size * fatten_point), panel_scales, coord)
      ))
    )
  }
)
