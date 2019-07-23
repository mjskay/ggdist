# Helper methods for creating geoms
#
# Author: mjskay
###############################################################################

# from ggstance:::ggname
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
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


#' Base ggproto classes for tidybayes
#'
#' @seealso \link{ggproto}
#' @keywords internal
#' @name tidybayes-ggproto
NULL
