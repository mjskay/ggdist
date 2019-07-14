# Helper methods for creating geoms
#
# Author: mjskay
###############################################################################

# from ggstance:::ggname
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

# create a new layer with some default computed aesthetics derived from the original data
layer_with_default_computed_aesthetics = function(old_layer, new_layer_name, default_computed_aesthetics) {
  new_setup_layer = function(self, data, plot) {
    apply_default_computed_aesthetics(self, plot, default_computed_aesthetics)
    ggproto_parent(old_layer, self)$setup_layer(data, plot)
  }

  ggplot2::ggproto(new_layer_name, old_layer,
    setup_layer = new_setup_layer)
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
