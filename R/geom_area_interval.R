# Meta-geom for intervals, densities, and their combinations
#
# Author: mjskay
###############################################################################

#' Area + point + interval meta-geom
#'
#' This meta-geom supports drawing combinations of functions (as areas), points, and intervals. It acts as a meta-geom
#' for implementing eye plots, half-eye plots, CCDF barplots, and point+multiple interval plots with horizontal or
#' vertical orientations, and with appropriate support for dodging.
#'
#' \code{geom_area_interval} is a flexible meta-geom that you can use directly, though in most cases you will want to
#' use shortcut geoms that combine appropriate stats with this geom to create more useful primitives, such as eye plots,
#' halfeye plots, point+interval plots, or CCDF barplots.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position The position adjustment to use for overlapping points on this layer.
#' @param ...  Other arguments passed to \code{\link{layer}}.
#' @param na.rm	If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing
#' values are silently removed.
#' @param show.legend Should this layer be included in the legends? Default is \code{c(size = FALSE)}, unlike most geoms,
#' to match its common use cases. \code{FALSE} hides all legends, \code{TRUE} shows all legends, and \code{NA} shows only
#' those that are mapped (the default for most geoms).
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them. This is
#' most useful for helper functions that define both data and aesthetics and shouldn't inherit behavior from the
#' default plot specification, e.g. borders.
#' @author Matthew Kay
#' @seealso See \code{\link{geom_lineribbon}} for a similar geom designed for curves plus probability bands. See
#' \code{\link{geom_pointrange}} and \code{\link{geom_pointrangeh}} for the geoms these are based on.
#' @keywords manip
#' @examples
#'
#' # TODO
#'
#' @importFrom ggplot2 GeomSegment GeomPolygon
#' @importFrom plyr dlply
#' @importFrom rlang %||%
#' @export
geom_area_interval = function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  ...,

  side = c("topright", "top", "right", "bottomleft", "bottom", "left", "both"),
  scale = 0.9,
  orientation = c("horizontal", "vertical"),
  justification = NULL,
  normalize = c("max_height", "height", "none"),
  area = TRUE,
  interval = TRUE,
  na.rm = FALSE,

  show.legend = NA,
  inherit.aes = TRUE
) {

  side = match.arg(side)
  orientation = match.arg(orientation)
  normalize = match.arg(normalize)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAreaInterval,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      side = side,
      scale = scale,
      orientation = orientation,
      justification = justification,
      normalize = normalize,
      area = area,
      interval = interval,
      na.rm = na.rm,
      ...
    )
  )
}

GeomAreaInterval = ggproto("GeomAreaInterval", Geom,
  default_aes = aes(
    # shared aesthetics
    alpha = NA,

    # shared point and interval aesthetics
    colour = "black",

    # shared area and interval aesthetics
    linetype = "solid",

    # shared point and area aesthetics
    fill = "gray65",

    # point aesthetics
    shape = 19,
    stroke = 0.5,
    point_colour = NULL,      # falls back to colour
    point_fill = NULL,        # falls back to fill
    point_size = 3,

    # interval aesthetics
    size = 1,
    interval_size = NULL,     # falls back to size
    interval_linetype = NULL, # falls back to linetype
    interval_colour = NULL,   # falls back to colour

    # area aesthetics
    outside_colour = NA,         # no line around the outside of the area by default
    outside_linetype = NULL,     # falls back to linetype
    outside_size = 1
  ),

  extra_params = c(
    "side",
    "scale",
    "orientation",
    "justification",
    "normalize",
    "area",
    "interval",
    "na.rm"
  ),

  setup_data = function(self, data, params) {
    define_orientation_variables(params$orientation)

    # normalize functions according to how we want to scale them
    switch(params$normalize,
      max_height = {
        # normalize so max height across all is 1
        # this preserves areas across groups in area plots
        finite_f = data$thickness[is.finite(data$thickness)]
        if (length(finite_f) > 0) {
          data$thickness = data$thickness / max(finite_f)
        }
      },
      height = {
        # normalize so height in each group is 1
        data = ddply(data, c("group", y), function(d) {
          finite_f = d$thickness[is.finite(d$thickness)]
          if (length(finite_f) > 0) {
            d$thickness = d$thickness / max(finite_f)
          }
          d
        })
      },
      none =,
    )

    # figure out the bounding rectangles for each group
    # this is necessary so that the bounding box is correct for
    # positions to work (e.g. position_dodge, etc)
    data[[height]] = data[[height]] %||% params[[height]] %||%
      resolution(data[[y]], FALSE)

    # determine bounding boxes based on justification: position
    # the min/max bounds around y such that y is at the correct
    # justification relative to the bounds
    justification = get_justification(params$justification, params$side)
    data[[ymin]] = data[[y]] - justification * data[[height]]
    data[[ymax]] = data[[y]] + (1 - justification) * data[[height]]

    data
  },

  draw_panel = function(self, data, panel_params, coord,
      side, scale, orientation, justification, area, interval
    ) {

    define_orientation_variables(orientation)

    # recover height (position_dodge adjusts ymax/ymix but not height)
    data[[height]] = data[[ymax]] - data[[ymin]]
    justification = get_justification(justification, side)

    area_grobs = list()
    if (area && !is.null(data$thickness)) {
      # thickness values were provided, draw them

      # area data is any of the data with finite thickness values
      a_data = data[is.finite(data$thickness),]

      if (nrow(a_data) > 0) {
        # rescale the data to be within the confines of the bounding box
        # we do this *again* here (rather than in setup_data) because
        # position_dodge doesn't work if we only do it up there
        a_scale = scale * a_data[[height]]

        switch_side(side,
          top = {
            # the slight nudge of justification * a_data[[height]] * (1 - scale) ensures that
            # justifications work properly when scale != 1 (and similarly for other values of `side`)
            a_data[[y]] = a_data[[ymin]] + justification * a_data[[height]] * (1 - scale)
            a_data[[ymin]] = a_data[[y]]
            a_data[[ymax]] = a_data[[y]] + a_data$thickness * a_scale
          },
          bottom = {
            a_data[[y]] = a_data[[ymax]] - (1 - justification) * a_data[[height]] * (1 - scale)
            a_data[[ymin]] = a_data[[y]] - a_data$thickness * a_scale
            a_data[[ymax]] = a_data[[y]]
          },
          both = {
            a_data[[y]] = (a_data[[ymin]] + a_data[[ymax]]) / 2 - (0.5 - justification) * a_data[[height]] * (1 - scale)
            a_data[[ymin]] = a_data[[y]] - a_data$thickness * a_scale / 2
            a_data[[ymax]] = a_data[[y]] + a_data$thickness * a_scale / 2
          }
        )

        a_data$colour = a_data$outside_colour
        a_data$linetype = a_data$outside_linetype %||% a_data$linetype
        a_data$size = a_data$outside_size

        # build grobs to display the areas
        area_grobs = dlply(a_data, c("group", y), function(d) {
          data_order = order(d[[x]])
          area_data_top = d[data_order,]
          area_data_top[[y]] = area_data_top[[ymax]]

          area_data_bottom = d[rev(data_order),]
          area_data_bottom[[y]] = area_data_bottom[[ymin]]

          area_data = rbind(area_data_top, area_data_bottom)

          area_grob = GeomPolygon$draw_panel(transform(area_data, colour = NA), panel_params, coord)

          if (!is.null(area_data_top$colour) && !all(is.na(area_data_top$colour))) {
            # we have an outline to draw around the outside of the area:
            # the definition of "outside" depends on the value of `side`:
            outside_data = switch_side(side, top = area_data_top, bottom = area_data_bottom, both = area_data)
            gList(area_grob, GeomPath$draw_panel(outside_data, panel_params, coord))
          } else {
            area_grob
          }
        })
      }
    }

    interval_grobs = list()
    point_grobs = list()
    if (interval && !is.null(data[[xmin]]) && !is.null(data[[xmax]])) {
      # intervals were provided, draw them

      # interval data is any of the data with non-missing interval values
      i_data = data[!is.na(data[[xmin]]) & !is.na(data[[xmax]]),]

      # adjust y position based on justification
      i_data[[y]] = i_data[[ymin]] + justification * i_data[[height]]

      if (nrow(i_data) > 0) {
        # reorder by interval width so largest intervals are drawn first
        i_data = i_data[order(abs(i_data[[xmax]] - i_data[[xmin]]), decreasing = TRUE),]

        p_data = i_data
        p_data$colour = p_data$point_colour %||% p_data$colour
        p_data$fill = p_data$point_fill %||% p_data$fill
        p_data$size = p_data$point_size
        point_grobs = list(GeomPoint$draw_panel(p_data, panel_params, coord))

        i_data[[x]] = i_data[[xmin]]
        i_data[[xend]] = i_data[[xmax]]
        i_data[[yend]] = i_data[[y]]
        i_data$colour = i_data$interval_colour %||% i_data$colour
        i_data$size = i_data$interval_size %||% i_data$size
        i_data$linetype = i_data$interval_linetype %||% i_data$linetype
        interval_grobs = list(GeomSegment$draw_panel(i_data, panel_params, coord, lineend = "butt"))
      }
    }

    ggname("geom_area_interval",
      gTree(children = do.call(gList, c(area_grobs, interval_grobs, point_grobs)))
    )
  }
)

# defines "orientation" variables in the environment of the calling
# function (for convenience): these are variables (typically aesthetics)
# that differ depending on whether the geom is horizontal or vertical.
# They are named assuming a horizontal orientation.
define_orientation_variables = function(orientation) {
  f = parent.frame()

  if (orientation == "horizontal") {
    f$height = "height"

    f$y = "y"
    f$ymin = "ymin"
    f$ymax = "ymax"
    f$yend = "yend"

    f$x = "x"
    f$xmin = "xmin"
    f$xmax = "xmax"
    f$xend = "xend"
  } else if (orientation == "vertical") {
    f$height = "width"

    f$y = "x"
    f$ymin = "xmin"
    f$ymax = "xmax"
    f$yend = "xend"

    f$x = "y"
    f$xmin = "ymin"
    f$xmax = "ymax"
    f$xend = "yend"
  } else {
    stop("Unknown orientation: `", orientation, "`")
  }
}

switch_side = function(side, top, bottom, both) {
  switch(side,
    top = ,
    topright = ,
    right = top,
    bottom = ,
    left = ,
    bottomleft = bottom,
    both = both,
    stop("Invalid side: `", side, "`")
  )
}

get_justification = function(justification, side) {
  if (is.null(justification)) {
    switch_side(side,
      top = 0,
      bottom = 1,
      both = 0.5
    )
  } else {
    justification
  }
}
