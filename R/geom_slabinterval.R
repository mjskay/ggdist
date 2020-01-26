# Meta-geom for intervals, densities, and their combinations
#
# Author: mjskay
###############################################################################


# drawing functions -------------------------------------------------------

rescale_slab_thickness = function(s_data, side, scale, orientation, justification, normalize, height, y, ymin, ymax) {
  # rescale the slab data to be within the confines of the bounding box
  # we do this *again* here (rather than in setup_data) because
  # position_dodge doesn't work if we only do it up there:
  # positions (like dodge) might change the heights so they aren't
  # all the same, and we want to preserve our normalization settings.
  # so we scale things based on the min height to ensure everything
  # is the same height
  thickness_scale = scale * min(s_data[[height]])
  y_scale = thickness_scale / s_data[[height]]

  switch_side(side,
    top = {
      # the slight nudge of justification * s_data[[height]] * (1 - y_scale) ensures that
      # justifications work properly when scale != 1 (and similarly for other values of `side`)
      s_data[[y]] = s_data[[ymin]] + justification * s_data[[height]] * (1 - y_scale)
      s_data[[ymin]] = s_data[[y]]
      s_data[[ymax]] = s_data[[y]] + s_data$thickness * thickness_scale
    },
    bottom = {
      s_data[[y]] = s_data[[ymax]] - (1 - justification) * s_data[[height]] * (1 - y_scale)
      s_data[[ymin]] = s_data[[y]] - s_data$thickness * thickness_scale
      s_data[[ymax]] = s_data[[y]]
    },
    both = {
      s_data[[y]] = (s_data[[ymin]] + s_data[[ymax]]) / 2 - (0.5 - justification) * s_data[[height]] * (1 - y_scale)
      s_data[[ymin]] = s_data[[y]] - s_data$thickness * thickness_scale / 2
      s_data[[ymax]] = s_data[[y]] + s_data$thickness * thickness_scale / 2
    }
  )

  s_data
}

draw_slabs = function(self, s_data, panel_params, coord,
  side, scale, orientation, justification, normalize,
  child_params
) {
  define_orientation_variables(orientation)

  s_data = self$override_slab_aesthetics(rescale_slab_thickness(
    s_data, side, scale, orientation, justification, normalize, height, y, ymin, ymax
  ))

  # build groups for the slabs
  # must group within both group and y for the polygon and path drawing functions to work
  slab_grobs = dlply(s_data, c("group", y), function(d) {
    d = d[order(d[[x]]),]

    slab_grob = if (!is.null(d$fill) && !all(is.na(d$fill))) {
      # only bother drawing the slab if it has some fill colour to it

      #split out slab data according to aesthetics that we want to be able to
      # vary along the length of the slab, then assemble the top and bottom lines
      # into a single entity
      slab_data = group_slab_data_by(d, c("fill", "alpha"), x, y, ymin, ymax, "both")
      GeomPolygon$draw_panel(transform(slab_data, colour = NA), panel_params, coord)
    }

    if (!is.null(d$colour) && !all(is.na(d$colour))) {
      # we have an outline to draw around the outside of the slab:
      # the definition of "outside" depends on the value of `side`:
      outline_data = group_slab_data_by(d, c("colour", "alpha", "size", "linetype"), x, y, ymin, ymax, side)
      gList(slab_grob, draw_path(outline_data, panel_params, coord))
    } else {
      slab_grob
    }
  })

  # when side = "top", need to invert draw order so that overlaps happen in a sensible way
  # (only bother doing this when scale > 1 since that's the only time it will matter)
  if (side == "top" && scale > 1) {
    rev(slab_grobs)
  } else {
    slab_grobs
  }
}


draw_pointintervals = function(self, i_data, panel_params, coord,
  orientation, justification, interval_size_domain, interval_size_range, fatten_point, show_point, na.rm,
  child_params
) {
  if (nrow(i_data) == 0) return(list())
  define_orientation_variables(orientation)

  interval_grobs = list()
  point_grobs = list()

  # adjust y position based on justification
  i_data[[y]] = i_data[[ymin]] + justification * i_data[[height]]

  if (nrow(i_data) > 0) {
    # reorder by interval width so largest intervals are drawn first
    i_data = i_data[order(abs(i_data[[xmax]] - i_data[[xmin]]), decreasing = TRUE),]

    point_grobs = if (show_point) {
      p_data = self$override_point_aesthetics(i_data, interval_size_domain, interval_size_range, fatten_point)
      point_grobs = list(GeomPoint$draw_panel(p_data, panel_params, coord, na.rm = na.rm))
    }

    i_data[[x]] = i_data[[xmin]]
    i_data[[xend]] = i_data[[xmax]]
    i_data[[yend]] = i_data[[y]]
    i_data = self$override_interval_aesthetics(i_data, interval_size_domain, interval_size_range)
    interval_grobs = list(GeomSegment$draw_panel(i_data, panel_params, coord, lineend = "butt", na.rm = na.rm))
  }

  c(interval_grobs, point_grobs)
}


draw_path = function(data, panel_params, coord) {
  do.call(gList, dlply(data, "group", function(outline_data) {
    munched_path = ggplot2::coord_munch(coord, outline_data, panel_params)
    grid::polylineGrob(
      munched_path$x,
      munched_path$y,
      default.units = "native",
      gp = grid::gpar(
        col = munched_path$colour,
        alpha = munched_path$alpha,
        lwd = munched_path$size * .pt,
        lty = munched_path$linetype,
        lineend = "butt",
        linejoin = "round",
        linemitre = 10
      )
    )
  }))
}


# aesthetic overrides -----------------------------------------------------

override_slab_aesthetics = function(self, s_data) {
  s_data$colour = s_data$slab_colour
  s_data$fill = s_data$slab_fill %||% s_data$fill
  s_data$alpha = s_data$slab_alpha %||% s_data$alpha
  s_data$size = s_data$slab_size
  s_data$linetype = s_data$slab_linetype %||% s_data$linetype
  s_data
}

override_point_aesthetics = function(self, p_data, size_domain, size_range, fatten_point) {
  p_data$colour = p_data$point_colour %||% p_data$colour
  p_data$fill = p_data$point_fill %||% p_data$fill
  p_data$alpha = p_data$point_alpha %||% p_data$alpha
  p_data$size = p_data$point_size %||% (fatten_point * get_line_size(p_data, size_domain, size_range))
  p_data
}

override_interval_aesthetics = function(self, i_data, size_domain, size_range) {
  i_data$colour = i_data$interval_colour %||% i_data$colour
  i_data$alpha = i_data$interval_alpha %||% i_data$alpha
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


# geom_slabinterval -------------------------------------------------------

#' Slab + point + interval meta-geom
#'
#' This meta-geom supports drawing combinations of functions (as slabs, aka ridge plots or joy plots), points, and
#' intervals. It acts as a meta-geom for many other tidybayes geoms that are wrappers around this geom, including
#' eye plots, half-eye plots, CCDF barplots, and point+multiple interval plots, and supports both horizontal and
#' vertical orientations, dodging (via the `position` argument), and relative justification of slabs with their
#' corresponding intervals.
#'
#' `geom_slabinterval` is a flexible meta-geom that you can use directly or through a variety of "shortcut"
#' geoms that represent useful combinations of the various parameters of this geom. In many cases you will want to
#' use the shortcut geoms instead as they create more useful mnemonic primitives, such as eye plots,
#' half-eye plots, point+interval plots, or CCDF barplots.
#'
#' The *slab* portion of the geom is much like a ridge or "joy" plot: it represents the value of a function
#' scaled to fit between values on the x or y access (depending on the value of `orientation`). Values of
#' the functions are specified using the `thickness` aesthetic and are scaled to fit into `scale`
#' times the distance between points on the relevant axis. E.g., if `orientation` is `"horizontal"`,
#' `scale` is 0.9, and `y` is a discrete variable, then the `thickness` aesthetic specifies the
#' value of some function of `x` that is drawn for every `y` value and scaled to fit into 0.9 times
#' the distance between points on the y axis.
#'
#' For the *interval* portion of the geom, `x` and `y` aesthetics specify the location of the
#' point and `ymin`/`ymax` or `xmin`/`xmax` (depending on the value of `orientation`
#' specifying the endpoints of the interval. A scaling factor for interval line width and point size is applied
#' through the `interval_size_domain`, `interval_size_range`, and `fatten_point` parameters.
#' These scaling factors are designed to give multiple probability intervals reasonable
#' scaling at the default settings for [scale_size_continuous()].
#'
#' As a combination geom, this geom expects a `datatype` aesthetic specifying which part of the geom a given
#' row in the input data corresponds to: `"slab"` or `"interval"`. However, specifying this aesthetic
#' manually is typically only necessary if you use this geom directly; the numerous wrapper geoms will
#' usually set this aesthetic for you as needed, and their use is recommended unless you have a very custom
#' use case.
#'
#' Wrapper geoms and stats include:
#'
#' \itemize{
#'   \item [stat_sample_slabinterval()] and associated stats
#'   \item [stat_dist_slabinterval()] and associated stats
#'   \item [geom_pointinterval()] / [geom_pointintervalh()]
#'   \item [stat_pointinterval()] / [stat_pointintervalh()]
#'   \item [geom_interval()] / [geom_intervalh()]
#'   \item [stat_interval()] / [stat_intervalh()]
#' }
#'
#' Typically, the `geom_*` versions are meant for use with already-summarized data (such as intervals) and the
#' `stat_*` versions are summarize the data themselves (usually draws from a distribution) to produce the geom.
#'
#' @eval rd_slabinterval_aesthetics()
#' @inheritParams ggplot2::layer
#' @param ...  Other arguments passed to [layer()].
#' @param side Which side to draw the slab on. `"topright"`, `"top"`, and `"right"` are synonyms
#' which cause the slab to be drawn on the top or the right depending on if `orientation` is `"horizontal"`
#' or `"vertical"`. `"bottomleft"`, `"bottom"`, and `"left"` are synonyms which cause the slab
#' to be drawn on the bottom of the left depending on if `orientation` is `"horizontal"` or
#' `"vertical"`. `"both"` draws the slab mirrored on both sides (as in a violin plot).
#' @param scale What proportion of the region allocated to this geom to use to draw the slab. If `scale = 1`,
#' slabs that use the maximum range will just touch each other. Default is `0.9` to leave some space.
#' @param orientation Whether this geom is drawn horizontally (`"horizontal"`) or
#' vertically (`"vertical"`). When horizontal (resp. vertical), the geom uses the `y` (resp. `x`)
#' aesthetic to identify different groups, then for each group uses the `x` (resp. `y`) aesthetic and the
#' `thickness` aesthetic to draw a function as an slab, and draws points and intervals horizontally
#' (resp. vertically) using the `xmin`, `x`, and `xmax` (resp. `ymin`, `y`, and `ymax`)
#' aesthetics.
#' @param justification Justification of the interval relative to the slab, where `0` indicates bottom/left
#' justification and `1` indicates top/right justification (depending on `orientation`). If `justification`
#' is `NULL` (the default), then it is set automatically based on the value of `side`: when `side` is
#' `"top"`/`"right"` `justification` is set to `0`, when `side` is `"bottom"`/`"left"`
#' `justification` is set to `1`, and when `side` is `"both"` `justification` is set to
#' `0.5`.
#' @param normalize How to normalize heights of functions input to the `thickness` aesthetic. If `"all"`
#' (the default), normalize so that the maximum height across all data is `1`; if `"panels"`, normalize within
#' panels so that the maximum height in each panel is `1`; if `"xy"`, normalize within
#' the x/y axis opposite the `orientation` of this geom so that the maximum height at each value of the
#' opposite axis is `1`; if `"groups"`, normalize within values of the opposite axis and within
#' groups so that the maximum height in each group is `1`; if `"none"`, values are taken as is with no
#' normalization (this should probably only be used with functions whose values are in \[0,1\], such as CDFs).
#' @param interval_size_domain The minimum and maximum of the values of the size aesthetic that will be translated into actual
#' sizes for intervals drawn according to `interval_size_range` (see the documentation for that argument.)
#' @param interval_size_range This geom scales the raw size aesthetic values when drawing interval and point sizes, as
#' they tend to be too thick when using the default settings of [scale_size_continuous()], which give sizes
#' with a range of `c(1, 6)`. The `interval_size_domain` value indicates the input domain of raw size values
#' (typically this should be equal to the value of the `range` argument of the [scale_size_continuous()]
#' function), and `interval_size_range` indicates the desired output range of the size values (the min and max of
#' the actual sizes used to draw intervals).
#' @param fatten_point A multiplicative factor used to adjust the size of the point relative to the size of the
#' thickest interval line. If you wish to specify point sizes directly, you can also use the `point_size`
#' aesthetic and [scale_point_size_continuous()] or [scale_point_size_discrete()]; sizes
#' specified with that aesthetic will not be adjusted using `fatten_point`.
#' @param show_slab Should the slab portion of the geom be drawn? Default `TRUE`.
#' @param show_point Should the point portion of the geom be drawn? Default `TRUE`.
#' @param show_interval Should the interval portion of the geom be drawn? Default `TRUE`.
#' @param na.rm	If `FALSE`, the default, missing values are removed with a warning. If `TRUE`, missing
#' values are silently removed.
#' @author Matthew Kay
#' @seealso See [geom_lineribbon()] for a combination geom designed for fit curves plus probability bands.
#' See [stat_sample_slabinterval()] and [stat_dist_slabinterval()] for families of stats
#' built on top of this geom for common use cases (like `stat_halfeyeh`).
#' See `vignette("slabinterval")` for a variety of examples of use.
#' @examples
#'
#' # geom_slabinterval() is typically not that useful on its own.
#' # See vignette("slabinterval") for a variety of examples of the use of its
#' # shortcut geoms and stats, which are more useful than using
#' # geom_slabinterval() directly.
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
  normalize = c("all", "panels", "xy", "groups", "none"),
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
    alpha = NULL,

    # shared point and interval aesthetics
    colour = NULL,

    # shared slab and interval aesthetics
    linetype = NULL,

    # shared point and slab aesthetics
    fill = NULL,

    # point aesthetics
    shape = NULL,
    stroke = NULL,
    point_colour = NULL,      # falls back to colour
    point_fill = NULL,        # falls back to fill
    point_alpha = NULL,       # falls back to alpha
    point_size = NULL,        # falls back to size

    # interval aesthetics
    size = NULL,
    interval_colour = NULL,   # falls back to colour
    interval_alpha = NULL,    # falls back to alpha
    interval_size = NULL,     # falls back to size
    interval_linetype = NULL, # falls back to linetype

    # slab aesthetics
    slab_size = NULL,         # no fallback
    slab_colour = NULL,       # no fallback
    slab_fill = NULL,         # falls back to fill
    slab_alpha = NULL,        # falls back to alpha
    slab_linetype = NULL      # falls back to linetype
  ),

  # default aesthetics as they will actually be set (here or in the key)
  # this is different from default_aes (above) so that we can identify what
  # aesthetics are *actually* being asked for when creating the key
  default_key_aes = aes(
    alpha = 1,
    colour = "black",
    linetype = "solid",
    fill = "gray65",
    shape = 19,
    stroke = 0.75,
    size = 1,
    slab_size = 1,
    slab_colour = NA
  ),

  optional_aes = c(
    "y", "ymin", "ymax", "x", "xmin", "xmax", "width", "height", "thickness"
  ),

  override_slab_aesthetics = override_slab_aesthetics,

  override_point_aesthetics = override_point_aesthetics,

  override_interval_aesthetics = override_interval_aesthetics,

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
    normalize = "all",
    interval_size_domain = c(1, 6),
    interval_size_range = c(0.6, 1.4),
    fatten_point = 1.8,
    show_slab = TRUE,
    show_point = TRUE,
    show_interval = TRUE,
    na.rm = FALSE
  ),

  default_datatype = "slab",

  setup_data = function(self, data, params) {
    params = defaults(params, self$default_params)

    define_orientation_variables(params$orientation)

    data$datatype = data$datatype %||% self$default_datatype

    # normalize functions according to how we want to scale them
    switch(params$normalize,
      all = {
        # normalize so max height across all data is 1
        # this preserves slabs across groups in slab plots
        finite_thickness = data$thickness[data$datatype == "slab" & is.finite(data$thickness)]
        if (length(finite_thickness) > 0) {
          data$thickness = data$thickness / max(finite_thickness)
        }
      },
      panels = ,
      xy = ,
      groups = {
        # normalize so height in each group or panel is 1
        normalization_groups = switch(params$normalize,
          panels = "PANEL",
          xy = c("PANEL", y),
          groups = c("PANEL", y, "group")
        )
        data = plyr::ddply(data, normalization_groups, function(d) {
          finite_thickness = d$thickness[d$datatype == "slab" & is.finite(d$thickness)]
          if (length(finite_thickness) > 0) {
            d$thickness = d$thickness / max(finite_thickness)
          }
          d
        })
      },
      none = {},
      stop('`normalize` must be "all", "panels", "xy", groups", or "none", not "', params$normalize, '"')
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

  draw_key = draw_key_slabinterval_,
  draw_key_slab = draw_key_slab_,
  draw_key_point = draw_key_point_,
  draw_key_interval = draw_key_interval_,

  draw_slabs = draw_slabs,
  draw_pointintervals = draw_pointintervals,

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
      show_interval = self$default_params$show_interval,
      na.rm = self$default_params$na.rm,
      # because draw_panel cannot take ... for some reason (!!), if we
      # want child geoms to add their own parameters we need some way to
      # pass them along
      child_params = list()
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

    # recover height: position_dodge adjusts ymax/ymin but not height
    data[[height]] = data[[ymax]] - data[[ymin]]

    justification = get_justification(justification, side)

    slab_grobs = if (show_slab && !is.null(data$thickness)) {
      # thickness values were provided, draw the slabs
      s_data = data[data$datatype == "slab",]
      if (nrow(s_data) > 0) {
        self$draw_slabs(s_data, panel_params, coord,
          side, scale, orientation, justification, normalize,
          child_params
        )
      }
    }

    point_interval_grobs = if (show_interval && !is.null(data[[xmin]]) && !is.null(data[[xmax]])) {
      self$draw_pointintervals(data[data$datatype == "interval",], panel_params, coord,
        orientation, justification, interval_size_domain, interval_size_range, fatten_point, show_point, na.rm,
        child_params
      )
    }

    ggname("geom_slabinterval",
      gTree(children = do.call(gList, c(list(), slab_grobs, point_interval_grobs)))
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


# gradient helpers --------------------------------------------------------

# groups slab data into contiguous components based on (usually) fill, colour, and alpha aesthetics,
# interpolating values ymin/ymax values at the cutpoints, then returns the necessary data frame
# (depending on `side`) that has top, bottom, or both sides to it
group_slab_data_by = function(slab_data, aesthetics = c("fill", "colour", "alpha"), x = "x", y = "y", ymin = "ymin", ymax = "ymax", side = "top") {
  aesthetics = intersect(aesthetics, names(slab_data))
  groups = factor(do.call(paste, slab_data[,aesthetics]))

  if (nlevels(groups) > 1) {
    # need to split into groups based on varying aesthetics

    last_in_group = groups != lead(groups, default = groups[[length(groups)]])
    first_in_group = groups != lag(groups, default = groups[[1]])
    slab_data$group = cumsum(first_in_group)

    # we want the two rows on each side of every cutpoint, row i and row j = i + 1
    new_row__i = slab_data[last_in_group,]
    new_row__j = slab_data[first_in_group,]
    new_x = (new_row__i[[x]] + new_row__j[[x]]) / 2
    new_ymin = (new_row__i[[ymin]] + new_row__j[[ymin]]) / 2
    new_ymax = (new_row__i[[ymax]] + new_row__j[[ymax]]) / 2
    new_row__i[[x]] = new_x
    new_row__i[[ymin]] = new_ymin
    new_row__i[[ymax]] = new_ymax
    new_row__j[[x]] = new_x
    new_row__j[[ymin]] = new_ymin
    new_row__j[[ymax]] = new_ymax

    # now we bind things with the new j rows at the beginning (they were first in each
    # group) and the new i rows at the end (they were last). This ensures that when the rows
    # are pulled out to draw a given group, they are in order within that group
    slab_data = bind_rows(
      new_row__j,
      slab_data,
      new_row__i
    )
  }

  # only calculate top / bottom as needed depending on `side`
  top = function() {
    slab_data[[y]] = slab_data[[ymax]]
    slab_data
  }
  bottom = function() {
    slab_data = slab_data[nrow(slab_data):1,]
    slab_data[[y]] = slab_data[[ymin]]
    slab_data
  }
  switch_side(side,
    top = top(),
    bottom = bottom(),
    both = bind_rows(top(), bottom())
  )
}


# shortcut geoms ----------------------------------------------------------

#' @export
#' @rdname geom_slabinterval
geom_slab = function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",

  ...,

  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    geom = GeomSlab,
    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      show_point = FALSE,
      show_interval = FALSE,

      na.rm = na.rm,
      ...
    )
  )
}
GeomSlab = ggproto("GeomSlab", GeomSlabinterval,
  default_key_aes = defaults(aes(
    size = 1,
    colour = NA
  ), GeomSlabinterval$default_key_aes),

  override_slab_aesthetics = function(self, s_data) {
    # we define these differently from geom_slabinterval to make this easier to use on its own
    s_data$colour = s_data$slab_colour %||% s_data$colour
    s_data$fill = s_data$slab_fill %||% s_data$fill
    s_data$alpha = s_data$slab_alpha %||% s_data$alpha
    s_data$size = s_data$slab_size %||% s_data$size
    s_data$linetype = s_data$slab_linetype %||% s_data$linetype
    s_data
  },

  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), GeomSlabinterval$default_params),

  draw_key_slab = function(self, data, key_data, params, size) {
    # can drop all the complicated checks from this key since it's just one geom
    s_key_data = self$override_slab_aesthetics(key_data)

    # what point calls "stroke" is what we call "size", since "size" is determined automatically
    if (is.na(data$colour) && (!is.na(data$size) || !is.na(data$linetype))) {
      # because the default colour is NA, if we want to draw a key for size / linetype we need to
      # reset the colour to something reasonable
      s_key_data$colour = "black"
    }
    draw_key_polygon(s_key_data, params, size)
  }
)
# have to unset these here because defaults() does not treat NULLs as unsetting values
GeomSlab$default_key_aes$slab_colour = NULL
GeomSlab$default_key_aes$slab_size = NULL

#' @export
#' @rdname geom_slabinterval
geom_slabh = function(..., orientation = "horizontal") {
  geom_slab(..., orientation = orientation)
}
