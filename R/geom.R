# Helper methods for creating geoms
#
# Author: mjskay
###############################################################################

# from ggstance:::ggname
ggname = function(prefix, grob) {
  grob$name = grobName(grob, prefix)
  grob
}


# default computed aesthetics ---------------------------------------------

# add default computed aesthetics to a layer --- useful for creating default aesthetics
# that are computed from the input data rather than default non-data-mapped aesthetics
add_default_computed_aesthetics = function(l, default_mapping) {
  ggproto(NULL, l,
    setup_layer = function(self, data, plot) {
      data = ggproto_parent(l, self)$setup_layer(data, plot)

      mapping = computed_mapping(self)
      if (is.null(mapping)) {
        mapping = list()
      }

      for (aesthetic in names(default_mapping)) {
        # we don't use exact matching here because if someone is using ggnewscale
        # then aesthetic "x" will be replaced with "x_new" and we don't want to
        # re-create the default "x" aesthetic mapping in that case.
        vars_in_mapping = all_names(quo_get_expr(default_mapping[[aesthetic]]))
        if (
          # only add the aesthetic if it isn't already set and if the variables it uses
          # are in the provided data and none of them are NA
          is.null(mapping[[aesthetic, exact = FALSE]]) &&
          (!isTRUE(self$inherit.aes) || is.null(computed_mapping(plot)[[aesthetic, exact = FALSE]])) &&
          all(vars_in_mapping %in% names(data)) &&
          !(any(is.na(data[,vars_in_mapping])))
        ) {
          mapping[[aesthetic]] = default_mapping[[aesthetic]]
        }
      }

      computed_mapping(self) = mapping

      data
    }
  )
}

#' the mapping property of layers changed to computed_mapping in ggplot 3.3.4
#' to avoid statefulness; this function encapsulates that change
#' see https://github.com/tidyverse/ggplot2/pull/4475
#' @importFrom utils packageVersion
#' @noRd
computed_mapping = function(x) {
  if (packageVersion("ggplot2") >= "3.3.3.9000") {
    x$computed_mapping
  } else {
    x$mapping
  }
}
`computed_mapping<-` = function(x, value) {
  if (packageVersion("ggplot2") >= "3.3.3.9000") {
    x$computed_mapping = value
  } else {
    x$mapping = value
  }
  x
}

# orientation detection ---------------------------------------------------

# detects the orientation of the geometry
#' @importFrom ggplot2 has_flipped_aes
get_flipped_aes = function(data, params, ...) {
  params$orientation =
    if (params$orientation %in% c("horizontal", "y")) "y"
    else if (params$orientation %in% c("vertical", "x")) "x"
    else if (is.na(params$orientation)) NA
    else stop("Unknown orientation: ", deparse0(params$orientation))

  has_flipped_aes(data, params, ...)
}

# detects the orientation of the geometry
get_orientation = function(flipped_aes) {
  if (flipped_aes) "y"
  else "x"
}


# defines "orientation" variables in the environment of the calling
# function (for convenience): these are variables (typically aesthetics)
# that differ depending on whether the geom's orientation is horizontal
# or vertical. They are named assuming a horizontal orientation.
globalVariables(c("height", "y", "ymin", "ymax", "yend", "x", "xmin", "xmax", "xend","x.range","y.range"))
define_orientation_variables = function(orientation) {
  f = parent.frame()

  if (orientation == "horizontal" || orientation == "y") {
    f$height = "height"

    f$y = "y"
    f$ymin = "ymin"
    f$ymax = "ymax"
    f$yend = "yend"
    f$y.range = "y.range"

    f$x = "x"
    f$xmin = "xmin"
    f$xmax = "xmax"
    f$xend = "xend"
    f$x.range = "x.range"
  } else if (orientation == "vertical" || orientation == "x") {
    f$height = "width"

    f$y = "x"
    f$ymin = "xmin"
    f$ymax = "xmax"
    f$yend = "xend"
    f$y.range = "x.range"

    f$x = "y"
    f$xmin = "ymin"
    f$xmax = "ymax"
    f$xend = "yend"
    f$x.range = "y.range"
  } else {
    stop("Unknown orientation: `", orientation, "`")
  }
}

#' Given a names list of aesthetic / aesthetic doc pairs, output a list of them
#' for use in docs. Used by rd_slabinterval_aesthetics
#' @noRd
rd_aesthetics = function(aes_docs, include_only) {
  aes_docs = aes_docs[intersect(names(aes_docs), include_only)]

  c(
    "\\itemize{",
    paste0("  \\item \\code{", names(aes_docs), "}: ", aes_docs),
    "}"
  )
}

#' Provides documentation of aesthetics for slabintervals
#' @noRd
rd_slabinterval_aesthetics = function(geom = GeomSlabinterval, geom_name = "geom_slabinterval", stat = NULL) {
  # build docs
  out = c(
    "@section Aesthetics:",
    "The slab+interval `stat`s and `geom`s have a wide variety of aesthetics that control
    the appearance of their three sub-geometries: the **slab**, the **point**, and
    the **interval**.\n"
  )

  # stat aesthetics
  pos_aes = list(
    x = 'x position of the geometry',
    y = 'y position of the geometry'
  )
  if (!is.null(stat)) {
    stat_aes = list(
      x = 'x position of the geometry (when orientation = `"vertical"`); or sample data to be summarized
       (when `orientation = "horizontal"`) except for `stat_dist_` geometries (which use only one of `x` or `y`
       at a time alond with the `dist` aesthetic).',
      y = 'y position of the geometry (when orientation = `"horizontal"`); or sample data to be summarized
       (when `orientation = "vertical"`) except for `stat_dist_` geometries (which use only one of `x` or `y`
       at a time alond with the `dist` aesthetic).',
      dist =
        'A name of a distribution (e.g. `"norm"`) or a \\pkg{distributional} object (e.g. [dist_normal()]).
       See **Details**.',
      args = 'Distribution arguments (`args` or `arg1`, ... `arg9`). See **Details**.'
    )
    out = c(out,
      "These `stat`s support the following aesthetics:",
      rd_aesthetics(stat_aes, stat$aesthetics()),
      paste0("In addition, in their default configuration (paired with [", geom_name, "()]) ",
        "the following aesthetics are supported by the underlying geom:\n")
    )
  } else {
    # do not include positional aesthetics with stats (since those are included).

    # positional aesthetics
    out = c(out, "**Positional aesthetics**", rd_aesthetics(pos_aes, geom$aesthetics()))
  }


  # slab aesthetics
  slab_aes = list(
    thickness =
      'The thickness of the slab at each `x` value (if `orientation = "horizontal"`) or
       `y` value (if `orientation = "vertical"`) of the slab.',
    side =
      'Which side to place the slab on. `"topright"`, `"top"`, and `"right"` are synonyms
       which cause the slab to be drawn on the top or the right depending on if `orientation` is `"horizontal"`
       or `"vertical"`. `"bottomleft"`, `"bottom"`, and `"left"` are synonyms which cause the slab
       to be drawn on the bottom or the left depending on if `orientation` is `"horizontal"` or
       `"vertical"`. `"topleft"` causes the slab to be drawn on the top or the left, and `"bottomright"`
       causes the slab to be drawn on the bottom or the right. `"both"` draws the slab mirrored on both
       sides (as in a violin plot).',
    scale =
      'What proportion of the region allocated to this geom to use to draw the slab. If `scale = 1`,
       slabs that use the maximum range will just touch each other. Default is `0.9` to leave some space.',
    justification =
      'Justification of the interval relative to the slab, where `0` indicates bottom/left
       justification and `1` indicates top/right justification (depending on `orientation`). If `justification`
       is `NULL` (the default), then it is set automatically based on the value of `side`: when `side` is
       `"top"`/`"right"` `justification` is set to `0`, when `side` is `"bottom"`/`"left"`
       `justification` is set to `1`, and when `side` is `"both"` `justification` is set to 0.5.',
    datatype =
      'When using composite geoms directly without a `stat` (e.g. [geom_slabinterval()]), `datatype` is used to
       indicate which part of the geom a row in the data targets: rows with `datatype = "slab"` target the
       slab portion of the geometry and rows with `datatype = "interval"` target the interval portion of
       the geometry. This is set automatically when using ggdist `stat`s.'
  )
  out = c(out, "**Slab-specific aesthetics**", rd_aesthetics(slab_aes, geom$aesthetics()))

  # interval-specific aesthetics
  int_aes = list(
    xmin = 'Left end of the interval sub-geometry (if `orientation = "horizontal"`).',
    xmax = 'Right end of the interval sub-geometry (if `orientation = "horizontal"`).',
    ymin = 'Lower end of the interval sub-geometry (if `orientation = "vertical"`).',
    ymax = 'Upper end of the interval sub-geometry (if `orientation = "vertical"`).'
  )
  out = c(out, "**Interval-specific aesthetics**", rd_aesthetics(int_aes, geom$aesthetics()))

  # interval-specific aesthetics
  point_aes = list(
    shape = 'Shape type used to draw the **point** sub-geometry.'
  )
  out = c(out, "**Point-specific aesthetics**", rd_aesthetics(point_aes, geom$aesthetics()))

  # color aesthetics
  color_aes = list(
    colour = '(or `color`) The color of the **interval** and **point** sub-geometries.
     Use the `slab_color`, `interval_color`, or `point_color` aesthetics (below) to
     set sub-geometry colors separately.',
    fill = 'The fill color of the **slab** and **point** sub-geometries. Use the `slab_fill`
     or `point_fill` aesthetics (below) to set sub-geometry colors separately.',
    alpha = 'The opacity of the **slab**, **interval**, and **point** sub-geometries. Use the `slab_alpha`,
     `interval_alpha`, or `point_alpha` aesthetics (below) to set sub-geometry colors separately.',
    colour_ramp = '(or `color_ramp`) A secondary scale that modifies the `color`
     scale to "ramp" to another color. See `scale_colour_ramp()` for examples.',
    fill_ramp = '(or `fill_ramp`) A secondary scale that modifies the `fill`
     scale to "ramp" to another color. See `scale_fill_ramp()` for examples.'
  )
  out = c(out, "**Color aesthetics**", rd_aesthetics(color_aes, geom$aesthetics()))

  # line aesthetics
  line_aes = list(
    size = 'Width of the outline around the **slab** (if visible). Also determines the width of
     the line used to draw the **interval** and the size of the **point**, but raw
     `size` values are transformed according to the `interval_size_domain`, `interval_size_range`,
     and `fatten_point` parameters of the `geom` (see above). Use the `slab_size`,
     `interval_size`, or `point_size` aesthetics (below) to set sub-geometry line widths separately
     (note that when size is set directly using the override aesthetics, interval and point
     sizes are not affected by `interval_size_domain`, `interval_size_range`, and `fatten_point`).',
    stroke = 'Width of the outline around the **point** sub-geometry.',
    linetype = 'Type of line (e.g., `"solid"`, `"dashed"`, etc) used to draw the **interval**
     and the outline of the **slab** (if it is visible). Use the `slab_linetype` or
     `interval_linetype` aesthetics (below) to set sub-geometry line types separately.'
  )
  out = c(out, "**Line aesthetics**", rd_aesthetics(line_aes, geom$aesthetics()))

  # slab override aesthetics
  slab_override_aes = list(
    slab_fill = 'Override for `fill`: the fill color of the slab.',
    slab_colour = '(or `slab_color`) Override for `colour`/`color`: the outline color of the slab.',
    slab_alpha = 'Override for `alpha`: the opacity of the slab.',
    slab_size = 'Override for `size`: the width of the outline of the slab.',
    slab_linetype = 'Override for `linetype`: the line type of the outline of the slab.',
    slab_shape = 'Override for `shape`: the shape of the dots used to draw the dotplot slab.'
  )
  out = c(out, "**Slab-specific color/line override aesthetics**", rd_aesthetics(slab_override_aes, geom$aesthetics()))

  # interval override aesthetics
  int_override_aes = list(
    interval_colour = '(or `interval_color`) Override for `colour`/`color`: the color of the interval.',
    interval_alpha = 'Override for `alpha`: the opacity of the interval.',
    interval_size = 'Override for `size`: the line width of the interval.',
    interval_linetype = 'Override for `linetype`: the line type of the interval.'
  )
  out = c(out, "**Interval-specific color/line override aesthetics**", rd_aesthetics(int_override_aes, geom$aesthetics()))

  # point override aesthetics
  point_override_aes = list(
    point_fill = 'Override for `fill`: the fill color of the point.',
    point_colour = '(or `point_color`) Override for `colour`/`color`: the outline color of the point.',
    point_alpha = 'Override for `alpha`: the opacity of the point.',
    point_size = 'Override for `size`: the size of the point.'
  )
  out = c(out, "**Point-specific color/line override aesthetics**", rd_aesthetics(point_override_aes, geom$aesthetics()))

  # undocumented aesthetics
  documented_aes = c(
    pos_aes, slab_aes, int_aes, point_aes, color_aes, line_aes, slab_override_aes, int_override_aes, point_override_aes
  )
  undocumented_aes = setdiff(geom$aesthetics(), names(documented_aes))
  if (length(undocumented_aes) > 0) {
    out = c(out,
      "**Other aesthetics** (these work as in standard `geom`s)",
      "\\itemize{",
      paste0("  \\item \\code{", undocumented_aes, "}"),
      "}"
    )
  }


  c(out,
    "See examples of some of these aesthetics in action in \\code{vignette(\"slabinterval\")}. ",
    "Learn more about the sub-geom override aesthetics (like \\code{interval_color}) in the \\link[ggdist]{scales} documentation. ",
    "Learn more about basic ggplot aesthetics in \\code{vignette(\"ggplot2-specs\")}. "
  )
}


# ggproto -----------------------------------------------------------------

#' Base ggproto classes for ggdist
#'
#' @seealso [ggproto]
#' @keywords internal
#' @name ggdist-ggproto
NULL
