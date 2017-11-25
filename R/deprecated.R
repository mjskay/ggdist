# Deprecated functions
#
# Author: mjskay
###############################################################################



#' Deprecated functions in tidybayes
#'
#' See `Details` for information on each deprecated function and suggested alternatives.
#'
#' \itemize{
#'   \item \code{ggeye} is deprecated: I no longer think it is good practice to design
#'   monolithic functions that output ggplot objects; instead, it is more flexible
#'   to design geoms and stats that can used within a complete ggplot
#'   workflow. \code{\link{geom_eyeh}} offers a horizontal eye plot geom that can
#'   be used instad of \code{ggeye}.
#'
#'   \item \code{gather_lsmeans_samples} is a deprecated alias for \code{\link{gather_emmeans_samples}}.
#'   The new name (estimated marginal means) is more appropriate for Bayesian models than
#'   the old name (least-squares means).
#' }
#'
#' @format NULL
#' @usage NULL
#' @author Matthew Kay
#' @name tidybayes-deprecated
#' @import ggplot2
#' @export
ggeye = function(data = NULL, mapping = NULL, ...) {
  .Deprecated("geom_eyeh")
  ggplot(data = data, mapping = mapping) + geom_eye(...) + coord_flip()
}
