# A geom_pointrangeh but with sensible defaults for displaying multiple intervals
#
# Author: mjskay
###############################################################################


# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("conf.low", "conf.high", ".prob"))


#' @rdname geom_pointinterval
#' @importFrom ggstance geom_linerangeh GeomLinerangeh
#' @import ggplot2
#' @export
geom_pointintervalh <- function(mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  ...,
  fatten.interval = 1.5 / 6,
  fatten.point = 2,
  na.rm = FALSE,
  show.legend = FALSE,
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
      fatten.point = fatten.point,
      fatten.interval = fatten.interval,
      na.rm = na.rm,
      ...
    )
  )

  #provide some default computed aesthetics
  default_computed_aesthetics = aes(xmin = conf.low, xmax = conf.high, size = -.prob)

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

#' @importFrom grid grobTree
draw_key_pointintervalh <- function(data, params, size) {
  grobTree(
    draw_key_path(transform(data, size = data$size * 1.5 / 6), params, size)
  )
}

#' @rdname tidybayes-ggproto
#' @format NULL
#' @usage NULL
#' @importFrom ggstance GeomLinerangeh
#' @importFrom grid grobName gTree gList
#' @import ggplot2
#' @export
GeomPointintervalh <- ggproto("GeomPointintervalh", Geom,
  default_aes = aes(colour = "black", size = 1.5, linetype = 1, shape = 19,
    fill = NA, alpha = NA, stroke = 1),

  draw_key = draw_key_pointintervalh,

  required_aes = c("x", "y", "xmin", "xmax"),

  draw_panel = function(data, panel_scales, coord, fatten.point = 1.8, fatten.interval = 1.5 / 6) {
    if (is.null(data$x))
      return(GeomLinerangeh$draw_panel(transform(data, size = size * fatten.interval), panel_scales, coord))

    ggname("geom_pointintervalh",
      gTree(children = gList(
        GeomLinerangeh$draw_panel(transform(data, size = size * fatten.interval), panel_scales, coord),
        GeomPoint$draw_panel(transform(data, size = size * fatten.point * fatten.interval), panel_scales, coord)
      ))
    )
  }
)
