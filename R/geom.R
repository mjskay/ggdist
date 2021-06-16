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

# provides aesthetic documentation for slabintervals
rd_slabinterval_aesthetics = function(geom = GeomSlabinterval, geom_name = "geom_slabinterval", stat = NULL) {
  stat_aesthetics = if (is.null(stat)) {
    "These geoms support the following aesthetics:"
  } else {
    c(
      "These stats support the following aesthetics:",
      "\\itemize{",
      paste0("  \\item \\code{", stat$aesthetics(), "}"),
      "}",
      paste0("In addition, in their default configuration (paired with [", geom_name, "()]) ",
        "the following aesthetics are supported by the underlying geom:")
    )
  }

  non_documented_aesthetics = setdiff(geom$aesthetics(), c("side", "scale", "justification"))

  geom_aesthetics = c(
    "\\itemize{",
      '  \\item \\code{side}: Which side to place the slab on. `"topright"`, `"top"`, and `"right"` are synonyms',
      '    which cause the slab to be drawn on the top or the right depending on if `orientation` is `"horizontal"`',
      '    or `"vertical"`. `"bottomleft"`, `"bottom"`, and `"left"` are synonyms which cause the slab',
      '    to be drawn on the bottom or the left depending on if `orientation` is `"horizontal"` or',
      '    `"vertical"`. `"topleft"` causes the slab to be drawn on the top or the left, and `"bottomright"`',
      '    causes the slab to be drawn on the bottom or the right. `"both"` draws the slab mirrored on both',
      '    sides (as in a violin plot).',
      '  \\item \\code{scale}: What proportion of the region allocated to this geom to use to draw the slab. If `scale = 1`,',
      '    slabs that use the maximum range will just touch each other. Default is `0.9` to leave some space.',
      '  \\item \\code{justification}: Justification of the interval relative to the slab, where `0` indicates bottom/left',
      '    justification and `1` indicates top/right justification (depending on `orientation`). If `justification`',
      '    is `NULL` (the default), then it is set automatically based on the value of `side`: when `side` is',
      '    `"top"`/`"right"` `justification` is set to `0`, when `side` is `"bottom"`/`"left"`',
      '    `justification` is set to `1`, and when `side` is `"both"` `justification` is set to',
      '    `0.5`.',
      paste0("  \\item \\code{", non_documented_aesthetics, "}"),
    "}"
  )

  c(
    "@section Aesthetics:",
    stat_aesthetics,
    geom_aesthetics,
    "See examples of some of these aesthetics in action in \\code{vignette(\"slabinterval\")}. ",
    "Learn more about the sub-geom aesthetics (like \\code{interval_color}) in the \\link[ggdist]{scales} documentation. ",
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
