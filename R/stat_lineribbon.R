# A stat_summary with a geom_lineribbon
#
# Author: mjskay
###############################################################################


# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("...width.."))


#' Line + multiple probability ribbon stat for ggplot
#'
#' A combination of \code{\link{stat_summary}} and \code{\link{geom_lineribbon}} with sensible defaults.
#' While \code{geom_lineribbon} is intended for use on data frames that have already been summarized using
#' a \code{\link{point_interval}} function, \code{stat_lineribbon} is intended for use directly on data
#' frames of draws, and will perform the summarization using a \code{\link{point_interval}} function.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param geom Use to override the default connection between
#' \code{geom_lineribbon} and \code{stat_lineribbon}.
#' @param position The position adjustment to use for overlapping points on this layer.
#' @param ...  Other arguments passed to \code{\link{layer}}. They may also be arguments to the paired geom.
#' @param point_interval A function that when given a vector should
#'   return a data frame with variables \code{y}, \code{ymin}, \code{ymax}, and \code{.width}; or
#'   \code{x}, \code{xmin}, \code{xmax}, and \code{.width}. \strong{Either is acceptable}: output
#'   will be converted into the \code{y}-based aesthetics. See the \code{point_interval} family of functions.
#' @param fun.data Similar to \code{point_interval}, for compatibility with \code{stat_summary}.
#'   Note: if the summary function is passed using \code{fun.data}, \code{x}-based aesthetics
#'   are not converted to the correct form automatically.
#' @param .width The \code{.width} argument passed to \code{point_interval}.
#' @param .prob Deprecated. Use \code{.width} instead.
#' @param fun.args Other optional arguments passed to \code{fun.data}.
#' @param na.rm	If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing
#' values are silently removed.
#' @param show.legend Should this layer be included in the legends? \code{NA}, the default, includes if any aesthetics
#' are mapped. \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them. This is
#' most useful for helper functions that define both data and aesthetics and shouldn't inherit behavior from the
#' default plot specification, e.g. borders.
#' @seealso See \code{\link{geom_lineribbon}} for the geom version, intended for use on points and intervals that have
#' already been summarized using a \code{\link{point_interval}} function. See \code{\link{stat_pointinterval}} /
#' \code{\link{stat_pointintervalh}} for a similar stat intended for point summaries and intervals.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' tibble(x = 1:10) %>%
#'   group_by_all() %>%
#'   do(tibble(y = rnorm(100, .$x))) %>%
#'   ggplot(aes(x = x, y = y)) +
#'   stat_lineribbon() +
#'   scale_fill_brewer()
#'
#' @export
#' @export
stat_lineribbon <- function(mapping = NULL, data = NULL,
  geom = "lineribbon", position = "identity",
  ...,
  point_interval = median_qi,
  fun.data = NULL,
  .width = c(.5, .8, .95),
  .prob,
  fun.args = list(),
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  .width = .Deprecated_argument_alias(.width, .prob)

  # Probs are drawn on top of each other in order by geom_lineribbon, so we have to sort in decreasing order
  # to make sure the largest interval is not drawn last (over-writing all other intervals)
  .width %<>% sort()

  fun.data = fun.data %||% vertical_aes(point_interval)

  l = layer(
    data = data,
    mapping = mapping,
    #we can re-use StatPointinterval internally because it does exactly the same thing
    #we would have done for a StatLineribbon
    stat = StatPointinterval,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun.data = fun.data,
      .width = .width,
      fun.args = fun.args,
      na.rm = na.rm,
      ...
    )
  )

  #provide some default computed aesthetics
  default_computed_aesthetics = aes(
    group = stat(.width),
    fill = forcats::fct_rev(ordered(stat(.width)))
  )

  compute_aesthetics = l$compute_aesthetics
  l$compute_aesthetics = function(self, data, plot) {
    apply_default_computed_aesthetics(self, plot, default_computed_aesthetics)
    compute_aesthetics(data, plot)
  }

  map_statistic = l$map_statistic
  l$map_statistic = function(self, data, plot) {
    apply_default_computed_aesthetics(self, plot, default_computed_aesthetics)
    map_statistic(data, plot)
  }

  l
}
