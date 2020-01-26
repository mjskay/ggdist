# Helper methods for creating geoms
#
# Author: mjskay
###############################################################################

# from ggstance:::ggname
ggname = function(prefix, grob) {
  grob$name = grobName(grob, prefix)
  grob
}

# add default computed aesthetics to a layer --- useful for creating default aesthetics
# that are computed from the input data rather than default non-data-mapped aesthetics
add_default_computed_aesthetics = function(l, default_mapping) {
  ggproto(NULL, l,
    setup_layer = function(self, data, plot) {
      data = ggproto_parent(l, self)$setup_layer(data, plot)

      if (is.null(self$mapping)) {
        self$mapping = list()
      }

      for (aesthetic in names(default_mapping)) {
        # we don't use exact matching here because if someone is using ggnewscale
        # then aesthetic "x" will be replaced with "x_new" and we don't want to
        # re-create the default "x" aesthetic mapping in that case.
        vars_in_mapping = all_names(quo_get_expr(default_mapping[[aesthetic]]))
        if (
          # only add the aesthetic if it isn't already set and if the variables it uses
          # are in the provided data and none of them are NA
          is.null(self$mapping[[aesthetic, exact = FALSE]]) &&
          (!isTRUE(self$inherit.aes) || is.null(plot$mapping[[aesthetic, exact = FALSE]])) &&
          all(vars_in_mapping %in% names(data)) &&
          !(any(is.na(data[,vars_in_mapping])))
        ) {
          self$mapping[[aesthetic]] = default_mapping[[aesthetic]]
        }
      }

      data
    }
  )
}

# defines "orientation" variables in the environment of the calling
# function (for convenience): these are variables (typically aesthetics)
# that differ depending on whether the geom's orientation is horizontal
# or vertical. They are named assuming a horizontal orientation.
globalVariables(c("height", "y", "ymin", "ymax", "yend", "x", "xmin", "xmax", "xend","x.range","y.range"))
define_orientation_variables = function(orientation) {
  f = parent.frame()

  if (orientation == "horizontal") {
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
  } else if (orientation == "vertical") {
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

  geom_aesthetics = c(
    "\\itemize{",
      paste0("  \\item \\code{", geom$aesthetics(), "}"),
    "}"
  )

  c(
    "@section Aesthetics:",
    stat_aesthetics,
    geom_aesthetics,
    "See examples of some of these aesthetics in action in \\code{vignette(\"slabinterval\")}. ",
    "Learn more about the sub-geom aesthetics (like \\code{interval_color}) in the \\link[tidybayes]{scales} documentation. ",
    "Learn more about basic ggplot aesthetics in \\code{vignette(\"ggplot2-specs\")}. "
  )
}


#' Base ggproto classes for tidybayes
#'
#' @seealso [ggproto]
#' @keywords internal
#' @name tidybayes-ggproto
NULL
