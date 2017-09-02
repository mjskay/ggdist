# Deprecated functions
#
# Author: mjskay
###############################################################################



#' Deprecated functions in tidybayes
#'
#' See `Details` for information on each deprecated function and suggested alternatives.
#'
#' \code{ggeye} is deprecated: I no longer think it is good practice to design
#' monolithic functions that output ggplot objects; instead, it is more flexible
#' to design geoms and stats that can be more easily used within a complete ggplot
#' workflow. \code{\link{geom_eyeh}} offers a horizontal eye plot geom that can
#' be used instad of \code{ggeye}.
#'
#' @format NULL
#' @usage NULL
#' @author Matthew Kay
#' @seealso \code{\link{geom_eyeh}}
#' @name tidybayes-deprecated
#' @import ggplot2
#' @export
ggeye = function(data = NULL, mapping = NULL, ...) {
  .Deprecated("geom_eyeh")
  ggplot(data = data, mapping = mapping) + geom_eye(...) + coord_flip()
}
