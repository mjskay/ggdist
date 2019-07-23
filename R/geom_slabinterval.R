# Meta-geom for intervals, densities, and their combinations
#
# Author: mjskay
###############################################################################

#' Slab + point + interval meta-geom
#'
#' This meta-geom supports drawing combinations of functions (as slabs), points, and intervals. It acts as a meta-geom
#' for implementing eye plots, half-eye plots, CCDF barplots, and point+multiple interval plots (among other things)
#' and supports both horizontal and vertical orientations, dodging with position = dodge, and relative justification of
#' slabs with their corresponding intervals.
#'
#' \code{geom_slabinterval} is a flexible meta-geom that you can use directly, though in most cases you will want to
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
#' @param side Which side to draw the slab on. \code{"topright"}, \code{"top"}, and \code{"right"} are synonyms
#' which cause the slab to be drawn on the top or the right depending on if \code{orientation} is \code{"horizontal"}
#' or \code{"vertical"}. \code{"bottomleft"}, \code{"bottom"}, and \code{"left"} are synonyms which cause the slab
#' to be drawn on the bottom of the left depending on if \code{orientation} is \code{"horizontal"} or
#' \code{"vertical"}
#' @param scale What proportion of the region allocated to this geom to use to draw the slab. If \code{scale = 1},
#' slabs that use the maximum range will just touch each other. Default is \code{0.9} to leave some space.
#' @param orientation Whether this geom is drawn horizontally (\code{"horizontal"}, the default) or
#' vertically (\code{"vertical"}). When horizontal (resp. vertical), the geom uses the \code{y} (resp. \code{x})
#' aesthetic to identify different groups, then for each group uses the \code{x} (resp. \code{y}) aesthetic and the
#' \code{thickness} aesthetic to draw a function as an slab, and draws points and intervals horizontally
#' (resp. vertically) using the \code{xmin}, \code{x}, and \code{xmax} (resp. \code{ymin}, \code{y}, and \code{ymax})
#' aesthetics.
#' @param justification Justification of the interval relative to the slab, where \code{0} indicates bottom/left
#' justification and \code{1} indicates top/right justification (depending on \code{orientation}). If \code{justification}
#' is \code{NULL} (the default), then it is set automatically based on the value of \code{side}: when \code{side} is
#' \code{"top"}/\code{"right"} \code{justification} is set to \code{0}, when \code{side} is \code{"bottom"}/\code{"left"}
#' \code{justification} is set to \code{1}, and when \code{side} is \code{"both"} \code{justification} is set to
#' \code{0.5}.
#' @param normalize How to normalize heights of functions input to the \code{thickness} aesthetic. If \code{"max_height"}
#' (the default), normalize so that the maximum height across all data is \code{1}; if \code{"height"}, normalize within
#' groups so that the maximum height in each group is \code{1}; if \code{"none"}, values are taken as is with no
#'  normalization (this should probably only be used with functions whose values are in [0,1]).
#' @param show_slab Should the slab portion of the geom be drawn? Default \code{TRUE}.
#' @param show_point Should the point portion of the geom be drawn? Default \code{TRUE}.
#' @param show_interval Should the interval portion of the geom be drawn? Default \code{TRUE}.
#' @param na.rm	If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing
#' values are silently removed.
#' @param show.legend Should this layer be included in the legends? \code{FALSE} hides all legends,
#' \code{TRUE} shows all legends, and \code{NA} (the default) shows only those that are mapped.
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
geom_slabinterval = function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  ...,

  # IF YOU ARE CHANGING THESE,
  # YOU MUST ALSO UPDATE:
  # 1. The call to layer_geom_slabinterval below
  # 2. The definition of GeomSlabinterval$extra_params
  # 3. The definition of GeomSlabinterval$default_params
  # 4. The argument definitions of GeomSlabinterval$draw_panel
  # This is needed to support how defaults work with child geoms,
  # amongst other things
  side = c("topright", "top", "right", "bottomleft", "bottom", "left", "both"),
  scale = 0.9,
  orientation = c("horizontal", "vertical"),
  justification = NULL,
  normalize = c("max_height", "height", "none"),
  interval_size_domain = c(1, 6),
  interval_size_range = c(0.6, 1.4),
  fatten_point = 1.8,
  show_slab = TRUE,
  show_point = TRUE,
  show_interval = TRUE,
  na.rm = FALSE,

  show.legend = NA,
  inherit.aes = TRUE
) {
  layer_geom_slabinterval(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    geom = GeomSlabinterval,
    ...,

    side = side,
    scale = scale,
    orientation = orientation,
    justification = justification,
    normalize = normalize,
    interval_size_domain = interval_size_domain,
    interval_size_range = interval_size_range,
    fatten_point = fatten_point,
    show_slab = show_slab,
    show_point = show_point,
    show_interval = show_interval,
    na.rm = na.rm,

    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

layer_geom_slabinterval = function(
  mapping = NULL,
  default_mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  geom = GeomSlabinterval,
  ...,

  show.legend = NA,
  inherit.aes = TRUE
) {

  .Deprecated_arguments(
    c("size_domain", "size_range"), ..., which = -2,
    message = "Use the interval_size_domain and interval_size_range arguments instead."
  )

  l = layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      ...
    )
  )

  if (!is.null(default_mapping)) {
    add_default_computed_aesthetics(l, default_mapping)
  } else {
    l
  }
}

draw_key_slabinterval = function(self, data, params, size) {
  params = modifyList(self$default_params, params)

  slab_grob = if(any(data$datatype == "slab")) {
    draw_key_rect(override_slab_aesthetics(data), params, size)
  }

  interval_grob = if(any(data$datatype == "interval")) {
    orientation = params$orientation
    interval_key = switch(orientation,
      horizontal = draw_key_hpath,
      vertical = draw_key_vpath
    )
    interval_key(
      override_interval_aesthetics(data, params$interval_size_domain, params$interval_size_range),
      params, size
    )
  }

  point_grob = if(any(data$datatype == "point")) {
    draw_key_point(
      override_point_aesthetics(data, params$interval_size_domain, params$interval_size_range, params$fatten_point),
      params, size
    )
  }

  gTree(children = gList(slab_grob, interval_grob, point_grob))
}

GeomSlabinterval = ggproto("GeomSlabinterval", Geom,
  default_aes = aes(
    # default datatype is slab (other valid value is "interval" for points/intervals)
    datatype = "slab",

    # shared aesthetics
    alpha = NA,

    # shared point and interval aesthetics
    colour = "black",

    # shared slab and interval aesthetics
    linetype = "solid",

    # shared point and slab aesthetics
    fill = "gray65",

    # point aesthetics
    shape = 19,
    stroke = 1,
    point_size = NULL,        # falls back to size
    point_colour = NULL,      # falls back to colour
    point_fill = NULL,        # falls back to fill

    # interval aesthetics
    size = 1,
    interval_size = NULL,     # falls back to size
    interval_linetype = NULL, # falls back to linetype
    interval_colour = NULL,   # falls back to colour

    # slab aesthetics
    outline_colour = NA,         # no outline around the slab by default
    outline_linetype = NULL,     # falls back to linetype
    outline_size = 1
  ),

  optional_aes = c(
    "ymin", "ymax", "xmin", "xmax", "width", "height"
  ),

  extra_params = c(
    "side",
    "scale",
    "orientation",
    "justification",
    "normalize",
    "interval_size_domain",
    "interval_size_range",
    "fatten_point",
    "show_slab",
    "show_point",
    "show_interval",
    "na.rm"
  ),

  default_params = list(
    side = "topright",
    scale = 0.9,
    orientation = "horizontal",
    justification = NULL,
    normalize = "max_height",
    interval_size_domain = c(1, 6),
    interval_size_range = c(0.6, 1.4),
    fatten_point = 1.8,
    show_slab = TRUE,
    show_point = TRUE,
    show_interval = TRUE
  ),

  default_datatype = "slab",

  setup_data = function(self, data, params) {
    params = modifyList(self$default_params, params)

    define_orientation_variables(params$orientation)

    data$datatype = data$datatype %||% self$default_datatype

    # normalize functions according to how we want to scale them
    switch(params$normalize,
      max_height = {
        # normalize so max height across all is 1
        # this preserves slabs across groups in slab plots
        finite_thickness = data$thickness[data$datatype == "slab" & is.finite(data$thickness)]
        if (length(finite_thickness) > 0) {
          data$thickness = data$thickness / max(finite_thickness)
        }
      },
      height = {
        # normalize so height in each group is 1
        data = ddply(data, c("group", y), function(d) {
          finite_thickness = d$thickness[d$datatype == "slab" & is.finite(d$thickness)]
          if (length(finite_thickness) > 0) {
            d$thickness = d$thickness / max(finite_thickness)
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

  draw_key = draw_key_slabinterval,

  draw_panel = function(self, data, panel_params, coord,
      side = self$default_params$side,
      scale = self$default_params$scale,
      orientation = self$default_params$orientation,
      justification = self$default_params$justification,
      normalize = self$default_params$normalize,
      interval_size_domain = self$default_params$interval_size_domain,
      interval_size_range = self$default_params$interval_size_range,
      fatten_point = self$default_params$fatten_point,
      show_slab = self$default_params$show_slab,
      show_point = self$default_params$show_point,
      show_interval = self$default_params$show_interval
    ) {

    define_orientation_variables(orientation)

    # recover height (position_dodge adjusts ymax/ymix but not height)
    data[[height]] = data[[ymax]] - data[[ymin]]
    justification = get_justification(justification, side)

    slab_grobs = list()
    if (show_slab && !is.null(data$thickness)) {
      # thickness values were provided, draw them

      # slab data is any of the data with datatype == "slab"
      s_data = data[data$datatype == "slab",]

      if (nrow(s_data) > 0) {
        # rescale the data to be within the confines of the bounding box
        # we do this *again* here (rather than in setup_data) because
        # position_dodge doesn't work if we only do it up there
        a_scale = scale * s_data[[height]]

        switch_side(side,
          top = {
            # the slight nudge of justification * s_data[[height]] * (1 - scale) ensures that
            # justifications work properly when scale != 1 (and similarly for other values of `side`)
            s_data[[y]] = s_data[[ymin]] + justification * s_data[[height]] * (1 - scale)
            s_data[[ymin]] = s_data[[y]]
            s_data[[ymax]] = s_data[[y]] + s_data$thickness * a_scale
          },
          bottom = {
            s_data[[y]] = s_data[[ymax]] - (1 - justification) * s_data[[height]] * (1 - scale)
            s_data[[ymin]] = s_data[[y]] - s_data$thickness * a_scale
            s_data[[ymax]] = s_data[[y]]
          },
          both = {
            s_data[[y]] = (s_data[[ymin]] + s_data[[ymax]]) / 2 - (0.5 - justification) * s_data[[height]] * (1 - scale)
            s_data[[ymin]] = s_data[[y]] - s_data$thickness * a_scale / 2
            s_data[[ymax]] = s_data[[y]] + s_data$thickness * a_scale / 2
          }
        )

        s_data = override_slab_aesthetics(s_data)

        # build grobs to display the slabs
        slab_grobs = dlply(s_data, c("group", y), function(d) {
          data_order = order(d[[x]])
          slab_data_top = d[data_order,]
          slab_data_top[[y]] = slab_data_top[[ymax]]

          slab_data_bottom = d[rev(data_order),]
          slab_data_bottom[[y]] = slab_data_bottom[[ymin]]

          slab_data = rbind(slab_data_top, slab_data_bottom)

          slab_grob = GeomPolygon$draw_panel(transform(slab_data, colour = NA), panel_params, coord)

          if (!is.null(slab_data_top$colour) && !all(is.na(slab_data_top$colour))) {
            # we have an outline to draw around the outside of the slab:
            # the definition of "outside" depends on the value of `side`:
            outline_data = switch_side(side, top = slab_data_top, bottom = slab_data_bottom, both = slab_data)
            gList(slab_grob, GeomPath$draw_panel(outline_data, panel_params, coord))
          } else {
            slab_grob
          }
        })
      }
    }

    interval_grobs = list()
    point_grobs = list()
    if (show_interval && !is.null(data[[xmin]]) && !is.null(data[[xmax]])) {
      # intervals were provided, draw them

      # interval data is any of the data with datatype == "interval"
      i_data = data[data$datatype == "interval",]

      # adjust y position based on justification
      i_data[[y]] = i_data[[ymin]] + justification * i_data[[height]]

      if (nrow(i_data) > 0) {
        # reorder by interval width so largest intervals are drawn first
        i_data = i_data[order(abs(i_data[[xmax]] - i_data[[xmin]]), decreasing = TRUE),]

        if (show_point) {
          p_data = override_point_aesthetics(i_data, interval_size_domain, interval_size_range, fatten_point)
          point_grobs = list(GeomPoint$draw_panel(p_data, panel_params, coord))
        }

        i_data[[x]] = i_data[[xmin]]
        i_data[[xend]] = i_data[[xmax]]
        i_data[[yend]] = i_data[[y]]
        i_data = override_interval_aesthetics(i_data, interval_size_domain, interval_size_range)
        interval_grobs = list(GeomSegment$draw_panel(i_data, panel_params, coord, lineend = "butt"))
      }
    }

    ggname("geom_slabinterval",
      gTree(children = do.call(gList, c(slab_grobs, interval_grobs, point_grobs)))
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


# aesthetic overrides -----------------------------------------------------

override_slab_aesthetics = function(s_data) {
  s_data$colour = s_data$outline_colour
  s_data$linetype = s_data$outline_linetype %||% s_data$linetype
  s_data$size = s_data$outline_size
  s_data
}

override_point_aesthetics = function(p_data, size_domain, size_range, fatten_point) {
  p_data$colour = p_data$point_colour %||% p_data$colour
  p_data$fill = p_data$point_fill %||% p_data$fill
  p_data$size = p_data$point_size %||% (fatten_point * get_line_size(p_data, size_domain, size_range))
  p_data
}

override_interval_aesthetics = function(i_data, size_domain, size_range) {
  i_data$colour = i_data$interval_colour %||% i_data$colour
  i_data$size = get_line_size(i_data, size_domain, size_range)
  i_data$linetype = i_data$interval_linetype %||% i_data$linetype
  i_data
}

get_line_size = function(i_data, size_domain, size_range) {
  size = i_data$interval_size %||% i_data$size
  pmax(
    (size - size_domain[[1]]) / (size_domain[[2]] - size_domain[[1]]) *
    (size_range[[2]] - size_range[[1]]) + size_range[[1]],
  0)
}
