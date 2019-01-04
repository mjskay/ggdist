# A stat_summary with a geom_pointinterval
#
# Author: mjskay
###############################################################################


# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("...width.."))


#' Point summary + multiple probability interval plots (ggplot stat)
#'
#' A combination of \code{\link{stat_summary}} / \code{\link{stat_summaryh}} and
#' \code{\link{geom_pointinterval}} / \code{\link{geom_pointintervalh}} with sensible defaults.
#' While the corresponding \code{geom}s are intended for use on
#' data frames that have already been summarized using a \code{\link{point_interval}}
#' function, these \code{stat}s are intended for use directly on data frames of draws, and
#' will perform the summarization using a \code{\link{point_interval}} function.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param geom Use to override the default connection between
#' \code{geom_pointinterval}/\code{geom_pointintervalh} and \code{stat_pointinterval}/\code{stat_pointintervalh}.
#' @param position The position adjustment to use for overlapping points on this layer.
#' @param ...  Other arguments passed to \code{\link{layer}}. They may also be arguments to the paired geom.
#' @param point_interval A function that when given a vector should
#'   return a data frame with variables \code{y}, \code{ymin}, \code{ymax}, and \code{.width}; or
#'   \code{x}, \code{xmin}, \code{xmax}, and \code{.width}. \strong{Either is acceptable}: output
#'   will be converted into the \code{y}-based aesthetics for \code{stat_pointinterval} and the
#'   \code{x}-based aesthetics for \code{stat_pointintervalh}. See the \code{point_interval} family of functions.
#' @param fun.data Similar to \code{point_interval}, for compatibility with \code{stat_summary}.
#'   Note: if the summary function is passed using \code{fun.data}, the \code{x} and \code{y}-based aesthetics
#'   are not converted to the correct form automatically.
#' @param .width The \code{.width} argument passed to \code{point_interval}.
#' @param .prob Deprecated. Use \code{.width} instead.
#' @param fun.args Other optional arguments passed to \code{fun.data}.
#' @param na.rm	If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing
#' values are silently removed.
#' @param show.legend Should this layer be included in the legends? Default is \code{c(size = FALSE)}, unlike most geoms,
#' to match its common use cases. \code{FALSE} hides all legends, \code{TRUE} shows all legends, and \code{NA} shows only
#' those that are mapped (the default for most geoms).
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them. This is
#' most useful for helper functions that define both data and aesthetics and shouldn't inherit behavior from the
#' default plot specification, e.g. borders.
#' @seealso See \code{\link{geom_pointinterval}} / \code{\link{geom_pointintervalh}} for the geom versions, intended
#' for use on points and intervals that have already been summarized using a \code{\link{point_interval}} function.
#' See \code{\link{stat_interval}} / \code{\link{stat_intervalh}} for a similar stat intended for intervals without
#' point summaries.
#' @examples
#'
#' library(magrittr)
#' library(ggplot2)
#'
#' data(RankCorr, package = "tidybayes")
#'
#' RankCorr %>%
#'   spread_draws(u_tau[i]) %>%
#'   ggplot(aes(y = i, x = u_tau)) +
#'   stat_pointintervalh(.width = c(.66, .95))
#'
#' RankCorr %>%
#'   spread_draws(u_tau[i]) %>%
#'   ggplot(aes(x = i, y = u_tau)) +
#'   stat_pointinterval(.width = c(.66, .95))
#'
#' @export
stat_pointinterval <- function(mapping = NULL, data = NULL,
  geom = "pointinterval", position = "identity",
  ...,
  point_interval = median_qi,
  fun.data = NULL,
  .width = c(.66, .95),
  .prob,
  fun.args = list(),
  na.rm = FALSE,
  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
  .width = .Deprecated_argument_alias(.width, .prob)

  fun.data = fun.data %||% vertical_aes(point_interval)

  l = layer(
    data = data,
    mapping = mapping,
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
  default_computed_aesthetics = aes(size = -...width..)  # nolint

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

#' @importFrom plyr defaults
StatPointinterval <- ggproto("StatPointinterval", StatSummary,
  compute_panel = function(data, scales, fun.data = median_qi, .width = c(.66, .95),
    fun.args = list(), na.rm = FALSE
  ) {

    fun.args = modifyList(list(.width = .width), fun.args)

    # Function that takes complete data frame as input
    fun.data = match.fun(fun.data)
    fun = function(df) {
      do.call(fun.data, c(list(quote(df$y)), fun.args))
    }

    summarise_by_x(data, fun)
  }
)
