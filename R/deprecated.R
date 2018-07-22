# Deprecated functions
#
# Author: mjskay
###############################################################################



#' Deprecated functions in tidybayes
#'
#' See `Details` for information on each deprecated function and suggested alternatives.
#' Several deprecated versions of functions also use slightly different output
#' formats (e.g., they use names like \code{term} and \code{estimate} where new
#' functions use \code{.variable} and \code{.value}; or they set \code{.iteration} even
#' when iteration information is not available --- new functions always set \code{.draw}
#' but may not set \code{.iteration}), so be careful when upgrading to new function names.
#'
#' \itemize{
#'
#'   \item \code{spread_draws}, \code{extract_samples}, and \code{tidy_samples} are
#'   deprecated names for \code{\link{spread_draws}}. The spread/gather terminology
#'   better distinguishes the resulting data frame format, and \emph{draws} is more
#'   correct terminology than \emph{samples} for describing multiple realizations from
#'   a posterior distribution.
#'
#'   \item \code{gather_samples} is a deprecated name for \code{\link{gather_draws}}.
#'   \emph{draws} is more correct terminology than \emph{samples} for describing multiple
#'   realizations from a posterior distribution.
#'
#'   \item \code{unspread_draws} is a deprecated name for \code{\link{unspread_draws}}.
#'   \emph{draws} is more correct terminology than \emph{samples} for describing multiple
#'   realizations from a posterior distribution.
#'
#'   \item \code{ungather_samples} is a deprecated name for \code{\link{ungather_draws}}.
#'   \emph{draws} is more correct terminology than \emph{samples} for describing multiple
#'   realizations from a posterior distribution.
#'
#'   \item \code{gather_lsmeans_samples} and \code{gather_emmeans_samples} are deprecated aliases
#'   for \code{\link{gather_emmeans_draws}}. The new name (estimated marginal means) is more
#'   appropriate for Bayesian models than the old name (least-squares means), and reflects the
#'   naming of the newer \code{emmeans} package. It also uses the more correct \emph{draws}
#'   in place of \emph{samples}.
#'
#'   \item \code{ggeye} is deprecated: I no longer think it is good practice to design
#'   monolithic functions that output ggplot objects; instead, it is more flexible
#'   to design geoms and stats that can used within a complete ggplot
#'   workflow. \code{\link{geom_eyeh}} offers a horizontal eye plot geom that can
#'   be used instad of \code{ggeye}.
#'
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
