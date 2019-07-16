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
  inherit.aes = TRUE
) {

  f = function(x) forcats::fct_rev(ordered(x))
  mapping = default_aes(mapping, xmin = .lower, xmax = .upper, color = f(.width))

  layer(
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

  setup_data = function(data, params) {
    # provide a default height so that position_dodgev works sensibly by default
    if (is.null(data$ymin) && is.null(data$ymax)) {
      data$height = data$height %||%
        params$height %||% (resolution(data$y, FALSE) * 0.75)
      transform(data,
        ymin = y - height / 2, ymax = y + height / 2, height = NULL
      )
    }
  },

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
