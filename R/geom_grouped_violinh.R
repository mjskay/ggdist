# Variant of geom_violinh and stat_xdensity that groups at x values.
# These are kind of hacky and mostly to fix #76. These are not exported for
# for now, since at some point they will probably be replaced with more
# comprehensive geoms for eye plots.
#
# Author: mjskay
###############################################################################

#' @importFrom ggstance StatXdensity GeomViolinh position_dodgev
#' @importFrom ggplot2  ggproto_parent
#' @importFrom plyr ddply
#' @importFrom rlang %||%


geom_grouped_violinh = function(mapping = NULL, data = NULL,
  stat = "grouped_xdensity", position = "dodgev",
  ...,
  trim = TRUE,
  scale = "area",
  relative_scale = 1,
  side = "both",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGroupedViolinh,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      relative_scale = relative_scale,
      side = side,
      na.rm = na.rm,
      ...
    )
  )
}

GeomGroupedViolinh = ggproto("GeomGroupedViolinh", GeomViolinh,
  default_aes = aes(weight = 1, colour = NA, fill = "gray65", size = 0,
    alpha = NA, linetype = "solid"),

  extra_params = c("na.rm", "side", "relative_scale"),

  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$y, FALSE) * 0.9 * params$relative_scale)

    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    switch(params$side,
      top = plyr::ddply(data, "group", transform,
        ymin = y,
        ymax = y + width
      ),
      both = plyr::ddply(data, "group", transform,
        ymin = y - width / 2,
        ymax = y + width / 2
      )
    )
  },

  draw_group = function(self, data, ...) {
    grobs = dlply(data, "y", function(d) ggproto_parent(GeomViolinh, self)$draw_group(d, ...))

    ggname("geom_grouped_violinh",
      gTree(children = do.call(gList, grobs))
    )
  }
)

stat_grouped_xdensity = function(mapping = NULL, data = NULL,
  geom = "violinh", position = "dodgev",
  ...,
  bw = "nrd0",
  adjust = 1,
  kernel = "gaussian",
  trim = TRUE,
  scale = "area",
  relative_scale = 1,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {
  scale <- match.arg(scale, c("area", "count", "width"))

  layer(
    data = data,
    mapping = mapping,
    stat = StatGroupedXdensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      trim = trim,
      scale = scale,
      relative_scale = relative_scale,
      na.rm = na.rm,
      ...
    )
  )
}

StatGroupedXdensity = ggproto("StatXdensity", StatXdensity,
  compute_group = function(self, data, scales, ...) {

    ddply(data, "y", function(d) ggproto_parent(StatXdensity, self)$compute_group(d, scales, ...))
  }
)
