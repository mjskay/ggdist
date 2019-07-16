# Helper methods for creating geoms
#
# Author: mjskay
###############################################################################

# from ggstance:::ggname
ggname <- function(prefix, grob) {
  grob$name <- grobName(grob, prefix)
  grob
}

# provide default aesthetics for a mapping --- useful for creating default computed
# asethetics when creating a layer (i.e. aesthetics computed from the input data
# rather than default non-data-mapped aesthetics)
default_aes = function(mapping, ...) {
  if (is.null(mapping)) mapping = aes()
  modifyList(aes(...), mapping)
}

#' Base ggproto classes for tidybayes
#'
#' @seealso \link{ggproto}
#' @keywords internal
#' @name tidybayes-ggproto
NULL
