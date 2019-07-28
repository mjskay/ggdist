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
        if (
          is.null(self$mapping[[aesthetic, exact = FALSE]]) && (!isTRUE(self$inherit.aes) ||
              is.null(plot$mapping[[aesthetic, exact = FALSE]]))
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


#' Base ggproto classes for tidybayes
#'
#' @seealso \link{ggproto}
#' @keywords internal
#' @name tidybayes-ggproto
NULL
