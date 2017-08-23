# A geom_linerangeh but with sensible defaults for displaying multiple intervals
#
# Author: mjskay
###############################################################################


# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("conf.low", "conf.high", ".prob"))


#' @rdname geom_interval
#' @importFrom ggstance geom_linerangeh GeomLinerangeh
#' @import ggplot2
#' @export
geom_intervalh <- function(mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {

  l = layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomIntervalh,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )

  #provide some default computed aesthetics
  default_computed_aesthetics = aes(xmin = conf.low, xmax = conf.high, color = forcats::fct_rev(ordered(.prob)))

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
#' @import ggplot2
#' @importFrom ggstance GeomLinerangeh
#' @export
GeomIntervalh <- ggproto("GeomIntervalh", Geom,
  default_aes = aes(colour = "black", size = 4, linetype = 1, shape = 19,
    fill = NA, alpha = NA, stroke = 1),

  draw_key = draw_key_path,

  required_aes = c("x", "y", "xmin", "xmax"),

  draw_panel = function(data, panel_scales, coord) {
    GeomLinerangeh$draw_panel(data, panel_scales, coord)  # nolint
  }
)
