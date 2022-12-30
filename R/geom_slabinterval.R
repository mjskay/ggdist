# Meta-geom for intervals, densities, and their combinations
#
# Author: mjskay
###############################################################################


# thickness handling functions -------------------------------------------------------

#' normalize thickness values to between 0 and 1
#' @noRd
normalize_thickness = function(x) UseMethod("normalize_thickness")

#' @export
normalize_thickness.default = function(x) {
  finite_thickness = x[is.finite(x)]
  if (length(finite_thickness) > 0) {
    max_finite_thickness = max(finite_thickness)
    min_finite_thickness = min(finite_thickness, 0)
    if (max_finite_thickness > min_finite_thickness) {
      x = (x - min_finite_thickness) / (max_finite_thickness - min_finite_thickness)
    }
  }
  # infinite values get plotted at the max height (e.g. for point masses)
  if (length(x) > 0) {
    x[x == Inf] = 1
  }
  x
}

#' @export
normalize_thickness.ggdist_thickness = function(x) {
  # thickness values passed directly into the geom (e.g. by
  # scale_thickness_shared()) are not normalized again.
  x = as.double(x)
  # infinite values get plotted at the max height (e.g. for point masses)
  if (length(x) > 0) {
    x[x == Inf] = 1
  }
  x
}

#' @export
normalize_thickness.data.frame = function(x) {
  x$thickness = normalize_thickness(x$thickness)
  x
}


#' rescale the slab data (ymin / ymax) to be within the confines of the bounding box
#' we do this *again* here (rather than in setup_data) because
#' position_dodge doesn't work if we only do it up there:
#' positions (like dodge) might change the heights so they aren't
#' all the same, and we want to preserve our normalization settings.
#' so we scale things based on the min height to ensure everything
#' is the same height
#' @noRd
rescale_slab_thickness = function(s_data, orientation, normalize, height, y, ymin, ymax) {
  min_height = min(s_data[[height]])

  # must do this within groups so that `side` can vary by slab
  ddply_(s_data, c("group", y), function(d) {
    scaling_aes = c("side", "justification", "scale")
    for (a in scaling_aes) {
      # use %in% here so that `NA`s are treated as equal
      if (!isTRUE(all(d[[a]] %in% d[[a]][[1]]))) {
        stop0(
          "Slab `", a, "` cannot vary within groups:\n",
          "all rows within the same slab must have the same `", a, "`."
        )
      }
    }

    thickness_scale = d$scale * min_height

    switch_side(d$side[[1]], orientation,
      topright = {
        d[[ymin]] = d[[y]] - d$justification * thickness_scale
        d[[ymax]] = d[[y]] + (d$thickness - d$justification) * thickness_scale
      },
      bottomleft = {
        d[[ymin]] = d[[y]] - (d$thickness - 1 + d$justification) * thickness_scale
        d[[ymax]] = d[[y]] + (1 - d$justification) * thickness_scale
      },
      both = {
        d[[ymin]] = d[[y]] - d$thickness * thickness_scale/2 + (0.5 - d$justification) * thickness_scale
        d[[ymax]] = d[[y]] + d$thickness * thickness_scale/2 + (0.5 - d$justification) * thickness_scale
      }
    )

    d
  })
}


# drawing functions -------------------------------------------------------

draw_slabs = function(self, s_data, panel_params, coord,
  orientation, normalize, fill_type, na.rm,
  ...
) {
  define_orientation_variables(orientation)

  # remove missing values - most NAs are fine here since they just create
  # breaks in the slab (handled below), but missing height means we can't
  # even determine slab dimensions, so need a warning
  s_data = ggplot2::remove_missing(s_data, na.rm, c(height), name = "geom_slabinterval", finite = TRUE)

  s_data = self$override_slab_aesthetics(rescale_slab_thickness(
    s_data, orientation, normalize, height, y, ymin, ymax
  ))

  # avoid giving fill type warnings multiple times
  fill_type = switch_fill_type(fill_type, segments = "segments", gradient = "gradient")

  # handle NAs: NAs in thickness should cause slabs to be broken apart into separate
  # pieces. We'll do this by creating a column to encode runs of NAs that we can
  # use in the grouping below.
  s_data$na_thickness_group = cumsum(is.na(s_data$thickness))
  # now that we've used them for grouping, we can drop rows with NA values of thickness
  s_data = s_data[!is.na(s_data$thickness),]

  # if dropping NAs caused this slab to be empty, return early
  if (nrow(s_data) == 0) return(list())

  # build groups for the slabs
  # must group within both group and y for the polygon and path drawing functions to work
  slab_grobs = dlply_(s_data, c("group", "na_thickness_group", y), function(d) {
    d = d[order(d[[x]]),]

    slab_grob = if (!is.null(d$fill) && !all(is.na(d$fill))) {
      # only bother drawing the slab if it has some fill colour to it

      switch_fill_type(fill_type,
        segments = {
          # split out slab data according to aesthetics that we want to be able to
          # vary along the length of the slab, then assemble the top and bottom lines
          # into a single entity
          slab_data = group_slab_data_by(d, c("fill", "alpha"), orientation, side = "both")
          draw_polygon(transform(slab_data, colour = NA), panel_params, coord)
        },
        gradient = {
          # build a linearGradient() representing the varying fill
          gradient = make_gradient_fill(coord$transform(d, panel_params), orientation)
          slab_data = group_slab_data_by(d, NULL, orientation, side = "both")
          draw_polygon(transform(slab_data, colour = NA), panel_params, coord, fill = gradient)
        }
      )
    }

    if (!is.null(d$colour) && !all(is.na(d$colour))) {
      # we have an outline to draw around the outside of the slab:
      # the definition of "outside" depends on the value of `side`:
      side = d$side[[1]]
      if (side == "both") {
        outline_data_top = group_slab_data_by(d, c("colour", "alpha", "linewidth", "linetype"), orientation, "top")
        outline_data_bottom = group_slab_data_by(d, c("colour", "alpha", "linewidth", "linetype"), orientation, "bottom")
        gList(slab_grob, draw_path(outline_data_top, panel_params, coord), draw_path(outline_data_bottom, panel_params, coord))
      } else {
        outline_data = group_slab_data_by(d, c("colour", "alpha", "linewidth", "linetype"), orientation, side)
        gList(slab_grob, draw_path(outline_data, panel_params, coord))
      }
    } else {
      slab_grob
    }
  })

  # when side = "top" or "right", need to invert draw order so that overlaps happen in a sensible way
  # unfortunately we can only do this by checking the first value of `side`, which
  # means this may be incorrect if `side` varies across slabs
  switch_side(s_data$side[[1]], orientation,
    topright = rev(slab_grobs),
    bottomleft = slab_grobs,
    both = slab_grobs
  )
}


draw_pointintervals = function(self, i_data, panel_params, coord,
  orientation, interval_size_domain, interval_size_range, fatten_point, show_point, na.rm,
  ...
) {
  if (nrow(i_data) == 0) return(list())
  define_orientation_variables(orientation)

  if (is.null(i_data[[xmin]]) || is.null(i_data[[xmax]])) {
    stop0(glue('
      You did not specify {xmin} or {xmax} aesthetics, which are needed to
      draw intervals with {snake_case(class(self)[[1]])}.
       - If you were using ggdist or tidybayes prior to version 2.1,
         these aesthetics were automatically set to ".lower" and ".upper" if
         those columns were in your data, in which case you may need to set
         aes({xmin} = .lower, {xmax} = .upper) explicitly.
      '))
  }

  interval_grobs = list()
  point_grobs = list()

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

  c(interval_grobs, point_grobs)
}


draw_path = function(data, panel_params, coord) {
  do.call(gList, dlply_(data, "group", function(outline_data) {
    munched_path = ggplot2::coord_munch(coord, outline_data, panel_params)
    grid::polylineGrob(
      munched_path$x,
      munched_path$y,
      default.units = "native",
      gp = grid::gpar(
        col = alpha(munched_path$colour, munched_path$alpha),
        lwd = munched_path$linewidth * .pt,
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
  s_data$colour = s_data[["slab_colour"]]
  s_data$fill = s_data[["slab_fill"]] %||% s_data[["fill"]]
  s_data$fill = apply_colour_ramp(s_data[["fill"]], s_data[["fill_ramp"]])
  s_data$alpha = s_data[["slab_alpha"]] %||% s_data[["alpha"]]
  #TODO: insert slab_size deprecation warning?
  s_data$linewidth = s_data[["slab_linewidth"]] %||% s_data[["slab_size"]]
  s_data$linetype = s_data[["slab_linetype"]] %||% s_data[["linetype"]]
  s_data
}

override_point_aesthetics = function(self, p_data, size_domain, size_range, fatten_point) {
  p_data$colour = p_data[["point_colour"]] %||% p_data[["colour"]]
  p_data$colour = apply_colour_ramp(p_data[["colour"]], p_data[["colour_ramp"]])
  p_data$fill = p_data[["point_fill"]] %||% p_data[["fill"]]
  p_data$alpha = p_data[["point_alpha"]] %||% p_data[["alpha"]]
  # TODO: insert fatten_point deprecation warning
  p_data$size = p_data[["point_size"]] %||% (fatten_point * transform_size(p_data[["interval_size"]] %||% p_data[["size"]], size_domain, size_range))
  p_data
}

override_interval_aesthetics = function(self, i_data, size_domain, size_range) {
  i_data$colour = i_data[["interval_colour"]] %||% i_data[["colour"]]
  i_data$colour = apply_colour_ramp(i_data[["colour"]], i_data[["colour_ramp"]])
  i_data$alpha = i_data[["interval_alpha"]] %||% i_data[["alpha"]]
  # TODO: insert interval_size deprecation warning
  i_data$linewidth = transform_size(i_data[["linewidth"]] %||% i_data[["interval_size"]] %||% i_data[["size"]], size_domain, size_range)
  i_data$linetype = i_data[["interval_linetype"]] %||% i_data[["linetype"]]
  i_data
}

transform_size = function(size, size_domain, size_range) {
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
#' [geom_slabinterval()] is a flexible meta-geom that you can use directly or through a variety of "shortcut"
#' geoms that represent useful combinations of the various parameters of this geom. In many cases you will want to
#' use the shortcut geoms instead as they create more useful mnemonic primitives, such as eye plots,
#' half-eye plots, point+interval plots, or CCDF barplots.
#'
#' The *slab* portion of the geom is much like a ridge or "joy" plot: it represents the value of a function
#' scaled to fit between values on the `x` or `y` axis (depending on the value of `orientation`). Values of
#' the functions are specified using the `thickness` aesthetic and are scaled to fit into `scale`
#' times the distance between points on the relevant axis. E.g., if `orientation` is `"horizontal"`,
#' `scale` is `0.9`, and `y` is a discrete variable, then the `thickness` aesthetic specifies the
#' value of some function of `x` that is drawn for every `y` value and scaled to fit into `0.9` times
#' the distance between points on the `y` axis.
#'
#' For the *interval* portion of the geom, `x` and `y` aesthetics specify the location of the
#' point, and `ymin`/`ymax` or `xmin`/`xmax` (depending on the value of `orientation`)
#' specify the endpoints of the interval. A scaling factor for interval line width and point size is applied
#' through the `interval_size_domain`, `interval_size_range`, and `fatten_point` parameters.
#' These scaling factors are designed to give multiple uncertainty intervals reasonable
#' scaling at the default settings for [scale_size_continuous()].
#'
#' As a combination geom, this geom expects a `datatype` aesthetic specifying which part of the geom a given
#' row in the input data corresponds to: `"slab"` or `"interval"`. However, specifying this aesthetic
#' manually is typically only necessary if you use this geom directly; the numerous wrapper geoms will
#' usually set this aesthetic for you as needed, and their use is recommended unless you have a very custom
#' use case.
#'
#' Wrapper geoms include:
#'
#'  - [geom_pointinterval()]
#'  - [geom_interval()]
#'  - [geom_slab()]
#'
#' In addition, the [stat_slabinterval()] family of stats uses geoms from the
#' [geom_slabinterval()] family, and is often easier to use than using these geoms
#' directly. Typically, the `geom_*` versions are meant for use with already-summarized data (such as intervals) and the
#' `stat_*` versions are summarize the data themselves (usually draws from a distribution) to produce the geom.
#'
#' @eval rd_slabinterval_params()
#' @eval rd_slabinterval_aesthetics()
#' @inheritParams ggplot2::layer
#' @param ...  Other arguments passed to [layer()]. These are often aesthetics, used to set an aesthetic
#' to a fixed value, like `colour = "red"` or `linewidth = 3` (see **Aesthetics**, below). They may also be
#' parameters to the paired geom/stat.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' Setting this equal to `"dodge"` ([position_dodge()]) or `"dodgejust"` ([position_dodgejust()]) can be useful if
#' you have overlapping geometries.
#' @return A [ggplot2::Geom] representing a slab or combined slab+interval geometry which can
#' be added to a [ggplot()] object.
#' @author Matthew Kay
#' @seealso See [geom_lineribbon()] for a combination geom designed for fit curves plus probability bands.
#' See [geom_dotsinterval()] for a combination geom designed for plotting dotplots with intervals.
#' See [stat_slabinterval()] for families of stats
#' built on top of this geom for common use cases (like [stat_halfeye()]).
#' See `vignette("slabinterval")` for a variety of examples of use.
#' @examples
#'
#' # geom_slabinterval() is typically not that useful on its own.
#' # See vignette("slabinterval") for a variety of examples of the use of its
#' # shortcut geoms and stats, which are more useful than using
#' # geom_slabinterval() directly.
#'
#' @importFrom ggplot2 GeomSegment GeomPolygon
#' @importFrom rlang %||%
#' @name geom_slabinterval
NULL

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomSlabinterval = ggproto("GeomSlabinterval", AbstractGeom,
  default_aes = aes(
    # default datatype is slab (other valid value is "interval" for points/intervals)
    datatype = "slab",

    # shared aesthetics
    alpha = NULL,

    # shared point and interval aesthetics
    colour = NULL,
    colour_ramp = NULL,

    # shared slab and interval aesthetics
    linetype = NULL,

    # shared point and slab aesthetics
    fill = NULL,

    # point aesthetics
    shape = NULL,
    stroke = NULL,
    size = NULL,
    point_colour = NULL,      # falls back to colour
    point_fill = NULL,        # falls back to fill
    point_alpha = NULL,       # falls back to alpha
    point_size = NULL,        # falls back to size

    # interval aesthetics
    linewidth = NULL,         # falls back to interval_size (dep) then size
    interval_colour = NULL,   # falls back to colour
    interval_alpha = NULL,    # falls back to alpha
    interval_size = NULL,     # deprecated (use linewidth)
    interval_linetype = NULL, # falls back to linetype

    # slab aesthetics
    slab_size = NULL,         # deprecated
    slab_linewidth = NULL,    # falls back to slab_size (dep)
    slab_colour = NULL,       # no fallback
    slab_fill = NULL,         # falls back to fill
    slab_alpha = NULL,        # falls back to alpha
    slab_linetype = NULL,     # falls back to linetype
    fill_ramp = NULL,

    # scale and positioning aesthetics
    side = "topright",
    scale = 0.9,
    justification = NULL
  ),

  # default aesthetics as they will actually be set (here or in the key)
  # this is different from default_aes (above) so that we can identify what
  # aesthetics are *actually* being asked for when creating the key
  default_key_aes = aes(
    alpha = NA_real_,
    colour = "black",
    linetype = "solid",
    fill = "gray65",
    shape = 19,
    stroke = 0.75,
    size = 1,
    slab_size = 1,
    slab_colour = NA
  ),

  required_aes = c("x|y"),

  optional_aes = c(
    "ymin", "ymax", "xmin", "xmax", "width", "height", "thickness"
  ),

  # workaround (#84)
  override_slab_aesthetics = function(self, ...) override_slab_aesthetics(self, ...),
  override_point_aesthetics = function(self, ...) override_point_aesthetics(self, ...),
  override_interval_aesthetics = function(self, ...) override_interval_aesthetics(self, ...),

  default_params = list(
    orientation = NA,
    normalize = "all",
    fill_type = "segments",
    interval_size_domain = c(1, 6),
    interval_size_range = c(0.6, 1.4),
    fatten_point = 1.8,
    show_slab = TRUE,
    show_point = TRUE,
    show_interval = TRUE,
    na.rm = FALSE
  ),

  deprecated_params = union(c(
    "size_domain", "size_range"
  ), AbstractGeom$deprecated_params),

  orientation_options = defaults(list(
    main_is_orthogonal = TRUE, range_is_orthogonal = TRUE, group_has_equal = TRUE, main_is_optional = TRUE
  ), AbstractGeom$orientation_options),

  setup_data = function(self, data, params) {
    data = ggproto_parent(AbstractGeom, self)$setup_data(data, params)
    define_orientation_variables(params$orientation)

    # when we are missing a main aesthetic (e.g. the y aes in a horizontal orientation),
    # fill it in with 0 so that we can still draw stuff
    data[[y]] = data[[y]] %||% 0

    data$datatype = data[["datatype"]] %||% self$default_aes[["datatype"]]

    # figure out the bounding rectangles for each group
    # this is necessary so that the bounding box is correct for
    # positions to work (e.g. position_dodge, etc)
    data[[height]] = params[[height]] %||% data[[height]] %||%
      resolution(data[[y]], FALSE)

    # determine bounding boxes based on justification: position
    # the min/max bounds around y such that y is at the correct
    # justification relative to the bounds
    justification = get_justification(
      params$justification %||% data[["justification"]],
      params$side %||% data[["side"]] %||% self$default_aes$side,
      params$orientation
    )
    data[[ymin]] = data[[y]] - justification * data[[height]]
    data[[ymax]] = data[[y]] + (1 - justification) * data[[height]]

    data
  },

  # workaround (#84)
  draw_key = function(self, ...) draw_key_slabinterval_(self, ...),
  draw_key_slab = function(self, ...) draw_key_slab_(self, ...),
  draw_key_point = function(self, ...) draw_key_point_(self, ...),
  draw_key_interval = function(self, ...) draw_key_interval_(self, ...),

  # workaround (#84)
  draw_slabs = function(self, ...) draw_slabs(self, ...),
  draw_pointintervals = function(self, ...) draw_pointintervals(self, ...),

  draw_layer = function(self, data, params, layout, coord) {
    define_orientation_variables(params$orientation)

    # normalize thickness according to what groups the user wants to scale it within
    # must do this here: not setup_data, so it happens after the thickness scale
    # has been applied; and not draw_panel, because normalization may be applied
    # across panels.
    switch(params$normalize,
      all = {
        # normalize so max height across all data is 1
        # this preserves slabs across groups in slab plots
        data = normalize_thickness(data)
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
        data = ddply_(data, normalization_groups, normalize_thickness)
      },
      none = {
        # ensure thickness is a thickness-type vector so it is not normalized again
        data$thickness = normalize_thickness(as_thickness(data$thickness))
      },
      stop('`normalize` must be "all", "panels", "xy", groups", or "none", not "', params$normalize, '"')
    )

    ggproto_parent(AbstractGeom, self)$draw_layer(data, params, layout, coord)
  },

  draw_panel = function(self, data, panel_params, coord,
      orientation = self$default_params$orientation,
      show_slab = self$default_params$show_slab,
      show_point = self$default_params$show_point,
      show_interval = self$default_params$show_interval,
      na.rm = self$default_params$na.rm,
      ...
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

    data$justification = get_justification(data[["justification"]], data[["side"]], orientation)

    slab_grobs = if (show_slab && !is.null(data$thickness)) {
      # thickness values were provided, draw the slabs
      s_data = data[data$datatype == "slab",]
      if (nrow(s_data) > 0) {
        self$draw_slabs(s_data, panel_params, coord,
          orientation = orientation,
          na.rm = na.rm,
          ...
        )
      }
    }

    point_interval_grobs = if (show_interval) {
      self$draw_pointintervals(data[data$datatype == "interval",], panel_params, coord,
        orientation = orientation,
        show_point = show_point,
        na.rm = na.rm,
        ...
      )
    }

    ggname("geom_slabinterval",
      do.call(grobTree, c(list(), slab_grobs, point_interval_grobs))
    )
  }
)

#' @rdname geom_slabinterval
#' @export
geom_slabinterval = make_geom(GeomSlabinterval)


# side and justification calculations -------------------------------------

switch_side = function(side, orientation, topright, bottomleft, both) {
  switch(orientation,
    y = ,
    horizontal = switch(side,
      top = ,
      topright = ,
      topleft = ,
      right = topright,

      bottom = ,
      bottomleft = ,
      bottomright = ,
      left = bottomleft,

      both = both,

      stop0("Unknown side: ", deparse0(side))
    ),
    x = ,
    vertical = switch(side,
      right = ,
      topright = ,
      bottomright = ,
      top = topright,

      left = ,
      topleft = ,
      bottomleft = ,
      bottom = bottomleft,

      both = both,

      stop0("Unknown side: ", deparse0(side))
    ),
    stop0("Unknown orientation: ", deparse0(orientation))
  )
}

# vectorized version of switch_side
case_when_side = function(side, orientation, topright, bottomleft, both) {
  # must make sure side and orientation are the same length as ifelse returns
  # a result of the same length as the first arg only
  common_length = max(length(side), length(orientation))
  side = rep_len(side, length.out = common_length)
  orientation = rep_len(orientation, length.out = common_length)

  ifelse(
    orientation %in% c("y", "horizontal"),
    ifelse(
      side %in% c("top", "topright", "topleft", "right"), topright, ifelse(
      side %in% c("bottom", "bottomleft", "bottomright", "left"), bottomleft,
      both
    )),
    # orientation is "vertical" or "x"
    ifelse(
      side %in% c("right", "topright", "bottomright", "top"), topright, ifelse(
      side %in% c("left", "topleft", "bottomleft", "bottom"), bottomleft,
      both
    ))
  )
}

get_justification = function(justification, side, orientation) {
  if (is.null(justification)) {
    case_when_side(side, orientation,
      topright = 0,
      bottomleft = 1,
      both = 0.5
    )
  } else {
    justification
  }
}


# gradient helpers --------------------------------------------------------

#' groups slab data into contiguous components based on (usually) fill, colour, and alpha aesthetics,
#' interpolating values ymin/ymax values at the cutpoints, then returns the necessary data frame
#' (depending on `side`) that has top, bottom, or both sides to it
#' @param slab_data a data frame containing data for a "slab", which should at
#' least include `x`, `y`, `thickness`, and either `xmin`/`xmax` or `ymin`/`ymax`,
#' depending on `orientation`.
#' @param aesthetics the aesthetics to group the slabs by. Consecutive runs of
#' equal values of all of these aesthetics are grouped together. At cutpoints
#' between consecutive runs, the `x`/`ymin`/`ymax` (or `y`/`xmin`/`xmax`,
#' depending on `orientation`) values are interpolated so that slabs just touch.
#' @param orientation orientation of the slab
#' @param side side of the slab
#' @noRd
#' @importFrom dplyr lag lead
group_slab_data_by = function(
  slab_data,
  aesthetics = c("fill", "colour", "alpha"),
  orientation = "horizontal",
  side = "topright"
) {
  define_orientation_variables(orientation)

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
  topright = function() {
    slab_data[[y]] = slab_data[[ymax]]
    slab_data
  }
  bottomleft = function() {
    slab_data = slab_data[nrow(slab_data):1,]
    slab_data[[y]] = slab_data[[ymin]]
    slab_data
  }
  switch_side(side, orientation,
    topright = topright(),
    bottomleft = bottomleft(),
    both = bind_rows(topright(), bottomleft())
  )
}

# avoid NOTE on R < 4.1 for the use of linearGradient below
if (getRversion() < "4.1") globalVariables("linearGradient")

#' construct a linearGradient() that can be used as a fill based on the fill
#' and alpha aesthetics of the provided data
#' @noRd
make_gradient_fill = function(slab_data, orientation = "horizontal") {
  define_orientation_variables(orientation)

  x1 = paste0(x, "1")
  y1 = paste0(y, "1")
  x2 = paste0(x, "2")
  y2 = paste0(y, "2")

  x_min = min(slab_data[[x]])
  x_max = max(slab_data[[x]])

  gradient_args = list(
    colours = alpha(slab_data$fill, slab_data$alpha),
    stops = (slab_data[[x]] - x_min) / (x_max - x_min)
  )

  # x1 / x2 are relative to the bounds of the grob, hence 0 / 1
  gradient_args[[x1]] = 0
  gradient_args[[x2]] = 1
  gradient_args[[y1]] = 0.5
  gradient_args[[y2]] = 0.5

  do.call(linearGradient, gradient_args)
}

#' draw a polygon grob --- based on ggplot2::GeomPolygon$draw_panel(), but
#' allows for linearGradient fills
#' @noRd
draw_polygon = function(data, panel_params, coord, fill = NULL) {
  n = nrow(data)

  # NOTE: this condition should always be false given where draw_polygon()
  # is currently used (after group_slab_data_by(), which returns data in pairs)
  # but leaving it in for safety and setting nocov
  if (n == 1) return(zeroGrob())  # nocov

  munched = coord_munch(coord, data, panel_params)

  # Sort by group to make sure that colors, fill, etc. come in same order
  munched = munched[order(munched$group), ]

  # For gpar(), there is one entry per polygon (not one entry per point).
  # We'll pull the first value from each group, and assume all these values
  # are the same within each group.
  first_idx = !duplicated(munched$group)
  first_rows = munched[first_idx, ]

  ggname(
    "geom_polygon",
    polygonGrob(
      munched$x, munched$y, default.units = "native",
      id = munched$group,
      gp = gpar(
        col = first_rows$colour,
        fill = fill %||% alpha(first_rows[["fill"]], first_rows[["alpha"]]),
        lwd = first_rows$linewidth * .pt,
        lty = first_rows$linetype
      )
    )
  )
}

switch_fill_type = function(fill_type, segments, gradient) {
  if (getRversion() < "4.1.0" && fill_type == "gradient") {             # nocov start
    warning0(glue('
      fill_type = "gradient" is not supported in R < 4.1.0.
       - Falling back to fill_type = "segments".
       - See help("geom_slabinterval") for more information.
      '))
    fill_type = "segments"
  } else if (getRversion() < "4.2.0" && fill_type == "auto") {
    warning0(glue('
      fill_type cannot be auto-detected in R < 4.2.0.
       - Falling back to fill_type = "segments".
       - For best results, if you are using a graphics device that
         supports gradients, set fill_type = "gradient".
       - See help("geom_slabinterval") for more information.
      '))
    fill_type = "segments"
  } else if (fill_type == "auto") {
    if ("LinearGradient" %in% grDevices::dev.capabilities()$patterns) {
      fill_type = "gradient"
    } else {
      warning0(glue('
        fill_type = "gradient" is not supported by the current graphics device.
         - Falling back to fill_type = "segments".
         - If you believe your current graphics device *does* support
           fill_type = "gradient" but auto-detection failed, set that option
           explicitly and consider reporting a bug.
         - See help("geom_slabinterval") for more information.
        '))
      fill_type = "segments"
    }
  }                                                                     # nocov end

  switch(fill_type,
    segments = segments,
    gradient = gradient,
    stop0("Unknown fill_type: ", deparse0(fill_type), '\nShould be "segments" or "gradient"')
  )
}
