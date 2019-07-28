# Meta-geom for intervals, densities, and their combinations
#
# Author: mjskay
###############################################################################


#' Slab + point + interval meta-geom
#'
#' This meta-geom supports drawing combinations of functions (as slabs, aka ridge plots or joy plots), points, and
#' intervals. It acts as a meta-geom for many other tidybayes geoms that are wrappers around this geom, including
#' eye plots, half-eye plots, CCDF barplots, and point+multiple interval plots, and supports both horizontal and
#' vertical orientations, dodging (via the \code{position} argument), and relative justification of slabs with their
#' corresponding intervals.
#'
#' \code{geom_slabinterval} is a flexible meta-geom that you can use directly or through a variety of "shortcut"
#' geoms that represent useful combinations of the various parameters of this geom. In many cases you will want to
#' use the shortcut geoms instead as they create more useful mnemonic primitives, such as eye plots,
#' halfeye plots, point+interval plots, or CCDF barplots.
#'
#' The \emph{slab} portion of the geom is much like a ridge or "joy" plot: it represents the value of a function
#' scaled to fit between values on the x or y access (depending on the value of \code{orientation}). Values of
#' the functions are specified using the \code{thickness} aesthetic and are scaled to fit into \code{scale}
#' times the distence between points on the relevant axis. E.g., if \code{orientation} is \code{"horizontal"},
#' \code{scale} is 0.9, and \code{y} is a discrete variable, then the \code{thickness} aesthetic specifies the
#' value of some function of \code{x} that is drawn for every \code{y} value and scaled to fit into 0.9 times
#' the distance between points on the y axis.
#'
#' For the \emph{interval} portion of the geom, \code{x} and \code{y} aesthetics specify the location of the
#' point and \code{ymin}/\code{ymax} or \code{xmin}/\code{xmax} (depending on the value of \code{orientation}
#' specifying the endpoints of the interval. A scaling factor for interval line width and point size is applied
#' through the \code{interval_size_domain}, \code{interval_size_range}, and \code{fatten_point} parameters.
#' These scaling factors are designed to give multiple probability intervals reasonable
#' scaling at the default settings for \code{\link{scale_size_continuous}}.
#'
#' As a combination geom, this geom expects a \code{datatype} aesthetic specifying which part of the geom a given
#' row in the input data corresponds to: \code{"slab"} or \code{"interval"}. However, specifying this aesthetic
#' manually is typically only necessary if you use this geom directly; the numerous wrapper geoms will
#' usually set this aesthetic for you as needed, and their use is recommended unless you have a very custom
#' use case.
#'
#' Wrapper geoms and stats include:
#'
#' \itemize{
#'   \item \code{\link{geom_pointinterval}} / \code{\link{geom_pointintervalh}}
#'   \item \code{\link{stat_pointinterval}} / \code{\link{stat_pointintervalh}}
#'   \item \code{\link{geom_interval}} / \code{\link{geom_intervalh}}
#'   \item \code{\link{stat_interval}} / \code{\link{stat_intervalh}}
#' }
#'
#' Typically, the \code{geom_*} versions are meant for use with already-summarized data (such as intervals) and the
#' \code{stat_*} versions are summarize the data themselves (usually draws from a distribution) to produce the geom.
#'
#' @inheritParams ggplot2::layer
#' @param ...  Other arguments passed to \code{\link{layer}}.
#' @param side Which side to draw the slab on. \code{"topright"}, \code{"top"}, and \code{"right"} are synonyms
#' which cause the slab to be drawn on the top or the right depending on if \code{orientation} is \code{"horizontal"}
#' or \code{"vertical"}. \code{"bottomleft"}, \code{"bottom"}, and \code{"left"} are synonyms which cause the slab
#' to be drawn on the bottom of the left depending on if \code{orientation} is \code{"horizontal"} or
#' \code{"vertical"}. \code{"both"} draws the slab mirrored on both sides (as in a violin plot).
#' @param scale What proportion of the region allocated to this geom to use to draw the slab. If \code{scale = 1},
#' slabs that use the maximum range will just touch each other. Default is \code{0.9} to leave some space.
#' @param orientation Whether this geom is drawn horizontally (\code{"horizontal"}) or
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
#' @param interval_size_domain The minimum and maximum of the values of the size aesthetic that will be translated into actual
#' sizes for intervals drawn according to \code{interval_size_range} (see the documentation for that argument.)
#' @param interval_size_range This geom scales the raw size aesthetic values when drawing interval and point sizes, as
#' they tend to be too thick when using the default settings of \code{\link{scale_size_continuous}}, which give sizes
#' with a range of \code{c(1, 6)}. The \code{interval_size_domain} value indicates the input domain of raw size values
#' (typically this should be equal to the value of the \code{range} argument of the \code{\link{scale_size_continuous}}
#' function), and \code{interval_size_range} indicates the desired output range of the size values (the min and max of
#' the actual sizes used to draw intervals).
#' @param fatten_point A multiplicative factor used to adjust the size of the point relative to the size of the
#' thickest interval line. If you wish to specify point sizes directly, you can also use the \code{point_size}
#' aesthetic and \code{\link{scale_point_size_continuous}} or \code{\link{scale_point_size_discrete}}; sizes
#' specified with that aesthetic will not be adjusted using \code{fatten_point}.
#' @param show_slab Should the slab portion of the geom be drawn? Default \code{TRUE}.
#' @param show_point Should the point portion of the geom be drawn? Default \code{TRUE}.
#' @param show_interval Should the interval portion of the geom be drawn? Default \code{TRUE}.
#' @param na.rm	If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing
#' values are silently removed.
#' @author Matthew Kay
#' @seealso See \code{\link{geom_lineribbon}} for a combination geom designed for fit curves plus probability bands.
#' See \code{\link{scales}} for more information on custom aesthetics that this geom supports.
#' @examples
#'
#' # TODO
#'
#' @importFrom ggplot2 GeomSegment GeomPolygon
#' @importFrom plyr dlply
#' @importFrom rlang %||%
#' @export
geom_slabinterval = function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
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
  orientation = c("vertical", "horizontal"),
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
  side = match.arg(side)
  orientation = match.arg(orientation)
  normalize = match.arg(normalize)

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

GeomSlabinterval = ggproto("GeomSlabinterval", Geom,
  default_aes = aes(
    # default datatype is slab (other valid value is "interval" for points/intervals)
    datatype = "slab",

    # shared aesthetics
    alpha = NA,

    # shared point and interval aesthetics
    colour = NULL,

    # shared slab and interval aesthetics
    linetype = NULL,

    # shared point and slab aesthetics
    fill = NULL,

    # point aesthetics
    shape = NULL,
    stroke = NULL,
    point_size = NULL,        # falls back to size
    point_colour = NULL,      # falls back to colour
    point_fill = NULL,        # falls back to fill

    # interval aesthetics
    size = NULL,
    interval_size = NULL,     # falls back to size
    interval_linetype = NULL, # falls back to linetype
    interval_colour = NULL,   # falls back to colour

    # slab aesthetics
    slab_colour = NA,         # no outline around the slab by default
    slab_linetype = NULL,     # falls back to linetype
    slab_size = 1
  ),

  # default aesthetics as they will actually be set (here or in the key)
  # this is different from default_aes (above) so that we can identify what
  # aesthetics are *actually* being asked for when creating the key
  default_key_aes = aes(
    colour = "black",
    linetype = "solid",
    fill = "gray65",
    shape = 19,
    stroke = 1,
    size = 1
  ),

  optional_aes = c(
    "ymin", "ymax", "xmin", "xmax", "width", "height", "thickness"
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
    orientation = "vertical",
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
    params = defaults(params, self$default_params)

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

    # provide defaults for color aesthetics --- we do this here because
    # doing it with default_aes makes the scales very busy (as all of
    # these elements get drawn even if they aren't mapped). By
    # setting the defaults here we can then check if these are present
    # in draw_key and not draw them if they aren't mapped.
    for (aesthetic in names(self$default_key_aes)) {
      data[[aesthetic]] = data[[aesthetic]] %||% self$default_key_aes[[aesthetic]]
    }

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


# side and justification calculations -------------------------------------

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
  s_data$colour = s_data$slab_colour
  s_data$linetype = s_data$slab_linetype %||% s_data$linetype
  s_data$size = s_data$slab_size
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
