# Helper methods for creating geoms
#
# Author: mjskay
###############################################################################

# from ggstance:::ggname
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

# applies some computed aesthetics to a layer, but allow them to be overridden by the
# layer or by the plot aesthetics.
apply_default_computed_aesthetics = function(self, plot, default_computed_aesthetics) {
  if (!"original_mapping" %in% names(self)) {
    self$original_mapping = self$mapping
  }
  self$mapping = self$original_mapping
  if (is.null(self$mapping)) {
    self$mapping = aes()
  }

  map2(names(default_computed_aesthetics), default_computed_aesthetics, function(name, value) {
    for (aesthetic in default_computed_aesthetics)
      if (!(name %in% names(plot$mapping)) &&
          !(name %in% names(self$mapping))
      ) {
        self$mapping[[name]] = value
      }
  })
}

#' Base ggproto classes for tidybayes
#'
#' @seealso \link{ggproto}
#' @keywords internal
#' @name tidybayes-ggproto
NULL
