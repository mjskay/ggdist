# Variant of geom_violin and stat_xdensity that groups at x values.
# These are kind of hacky and mostly to fix #76. These are not exported for
# for now, since at some point they will probably be replaced with more
# comprehensive geoms for eye plots.
#
# Author: mjskay
###############################################################################

#' @importFrom ggplot2 StatYdensity ggproto_parent
#' @importFrom plyr ddply
#' @importFrom rlang %||%


geom_grouped_violin = function(mapping = NULL, data = NULL,
  stat = "grouped_ydensity", position = "dodge",
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
    geom = GeomGroupedViolin,
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

GeomGroupedViolin = ggproto("GeomGroupedViolin", GeomViolin,
  default_aes = aes(weight = 1, colour = NA, fill = "gray65", size = 0,
    alpha = NA, linetype = "solid"),

  extra_params = c("na.rm", "side", "relative_scale"),

  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9 * params$relative_scale)

    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    switch(params$side,
      right = plyr::ddply(data, "group", transform,
        xmin = x,
        xmax = x + width
      ),
      both = plyr::ddply(data, "group", transform,
        xmin = x - width / 2,
        xmax = x + width / 2
      )
    )
  },

  draw_group = function(self, data, ...) {
    grobs = dlply(data, "x", function(d) ggproto_parent(GeomViolin, self)$draw_group(d, ...))

    ggname("geom_grouped_violin",
      gTree(children = do.call(gList, grobs))
    )
  }
)

stat_grouped_ydensity = function(mapping = NULL, data = NULL,
  geom = "violin", position = "dodge",
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
    stat = StatGroupedYdensity,
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

StatGroupedYdensity = ggproto("StatYdensity", StatYdensity,
  compute_group = function(self, data, scales, ...) {

    ddply(data, "x", function(d) ggproto_parent(StatYdensity, self)$compute_group(d, scales, ...))
  }
)
