# A geom_linerangeh but with sensible defaults for displaying multiple intervals
#
# Author: mjskay
###############################################################################



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
  default_computed_aesthetics = aes(xmin = .lower, xmax = .upper, color = forcats::fct_rev(ordered(.width)))

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
#' @export
GeomIntervalh <- ggproto("GeomIntervalh", Geom,
  default_aes = aes(colour = "black", size = 4, linetype = 1, shape = 19,
    fill = NA, alpha = NA, stroke = 1),

  draw_key = draw_key_path,

  required_aes = c("x", "y", "xmin", "xmax"),

  draw_panel = function(data, panel_scales, coord) {
    # draw all the intervals
    interval_grobs = data %>%
      dlply("group", function(d) {
        group_grobs = list(GeomLinerangeh$draw_panel(d, panel_scales, coord))
        list(
          width = d %$% mean(abs(xmax - xmin)),
          grobs = group_grobs
        )
      })

    # this is a slightly hackish approach to getting the draw order correct for the common
    # use case of fit lines / curves: draw the intervals in order from largest mean width to
    # smallest mean width, so that the widest intervals are on the bottom.
    interval_grobs = interval_grobs[order(-map_dbl(interval_grobs, "width"))] %>%
      map("grobs") %>%
      reduce(c)

    ggname("geom_intervalh",
      gTree(children = do.call(gList, interval_grobs))
    )
  }
)
