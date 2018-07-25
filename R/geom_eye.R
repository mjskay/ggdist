# geom_eye for eye plots with intervals
#
# Author: mjskay
###############################################################################



#' Eye plots of densities with point and interval summaries (ggplot geom)
#'
#' Generates a combination \code{\link{geom_violin}} and \code{\link{stat_pointinterval}}
#' (for \code{geom_eye}) or \code{\link{geom_violinh}} and \code{\link{stat_pointintervalh}}
#' (for \code{geom_eyeh}) representing the density, point summary, and uncertainty intervals
#' for draws from a distribution. Useful for representing posteriors from Bayesian models;
#' in that context this is variously called an eye plot, a raindrop plot, or a violin plot
#' (though violin plot is also applied to plots of data, hence its use is not
#' preferred here).
#'
#' An eye plot is a compact visual summary of the distribution of a sample,
#' used (under various names and with subtle variations) to visualize posterior
#' distributions in Bayesian inference. This instantiation is a combination of
#' a violin plot, point summary, and one or more uncertainty intervals.
#'
#' The vertical form, \code{geom_eye}, is equivalent to  \code{geom_violin() + stat_pointinterval()}
#' with some reasonable defaults, including color choices and the use of median with 95\%
#' and 66\% quantile intervals.
#'
#' The horizontal form, \code{geom_eyeh()}, is equivalent to \code{geom_violinh() + stat_pointintervalh()}.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param position Passed to \code{\link{geom_violin}} / \code{\link{geom_violinh}}. The position adjustment
#' to use for overlapping points on this layer.
#' @param trim Passed to \code{\link{geom_violin}} / \code{\link{geom_violinh}}. If \code{TRUE} (default),
#' trim the tails of the violins to the range of the data. If \code{FALSE},
#' don't trim the tails.
#' @param scale Passed to \code{\link{geom_violin}} / \code{\link{geom_violinh}}. If "area" (default), all
#' violins have the same area (before trimming the tails).  If "count", areas
#' are scaled proportionally to the number of observations. If "width", all
#' violins have the same maximum width.
#' @param relative_scale A relative scaling factor to determine how much of the available
#' space densities are scaled to fill: if \code{1}, all available space is filled.
#' @param fill Passed to \code{\link{geom_violin}} / \code{\link{geom_violinh}}. Fill color of the violin.
#' @param violin.color Passed as the \code{color} argument of \code{\link{geom_violin}} / \code{\link{geom_violinh}}.
#' The default, \code{NA}, suppresses the violin outline. Set to another value to set the violin outline color
#' manually, or set to \code{NULL} if you want the outline color of the violin to be determined by the aesthetic
#' mapping.
#' @param ...  Currently unused.
#' @param point_interval A function that when given a vector should
#'   return a data frame with variables \code{y}, \code{ymin}, \code{ymax}, and \code{.width}; or
#'   \code{x}, \code{xmin}, \code{xmax}, and \code{.width}. \strong{Either is acceptable}: output
#'   will be converted into the \code{y}-based aesthetics for \code{geom_eye} and the
#'   \code{x}-based aesthetics for \code{geom_eyeh}. See the \code{point_interval} family of functions.
#' @param fun.data Similar to \code{point_interval}, for compatibility with \code{stat_summary}.
#'   Note: if the summary function is passed using \code{fun.data}, the \code{x} and \code{y}-based aesthetics
#'   are not converted to the correct form automatically.
#' @param fun.args Optional arguments passed to \code{fun.data}.
#' @param .width The \code{.width} argument passed to \code{point_interval}.
#' @param .prob Deprecated. Use \code{.width} instead.
#' @param size_domain The minimum and maximum of the values of the size aesthetic that will be translated into actual
#' sizes drawn according to \code{size_range} (see the documentation for that argument, below.)
#' @param size_range This geom scales the raw size aesthetic values, as they tend to be too thick when using the default
#' settings of \code{\link{scale_size_continuous}}, which give sizes with a range of \code{c(1, 6)}. The
#' \code{size_domain} value indicates the input domain of raw size values (typically this should be equal to the value
#' of the \code{range} argument of the \code{\link{scale_size_continuous}} function), and \code{size_range} indicates
#' the desired output range of the size values (the min and max of the actual sizes used to draw intervals).
#' @param fatten_point A multiplicative factor used to adjust the size of the point relative to the size of the
#' thickest line.
#' @param color Passed to \code{\link{stat_pointinterval}}. Color of the point
#' summary and uncertainty interval.
#' @param size Passed to \code{\link{stat_pointinterval}}. Line weight of the point
#' summary and uncertainty interval.
#' @author Matthew Kay
#' @seealso See \code{\link{geom_halfeyeh}}
#' for the non-mirrored density ("half eye") version. See \code{\link{geom_violin}} and \code{\link{stat_pointinterval}}
#' for the geoms these functions are based on.
#' @keywords manip
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
#'   geom_eyeh()
#'
#' RankCorr %>%
#'   spread_draws(u_tau[i]) %>%
#'   ggplot(aes(x = i, y = u_tau)) +
#'   geom_eye()
#'
#' @importFrom utils modifyList
#' @importFrom purrr discard
#' @importFrom rlang %||%
#' @import ggplot2
#' @export
geom_eye = function(
  #shared properties
  mapping = NULL, data = NULL,

  #violin properties
  position = "identity", trim = TRUE, scale = "area", relative_scale = 1, fill = NULL, violin.color = NA,

  ...,

  #stat_summaryh properties
  point_interval = median_qi,
  fun.data = NULL,
  fun.args = list(),
  .width = c(.66, .95),
  .prob,
  color = NULL, size = NULL, size_domain = NULL, size_range = NULL, fatten_point = NULL
) {
  .width = .Deprecated_argument_alias(.width, .prob)

  fun.data = fun.data %||% vertical_aes(point_interval)

  #build violin plot
  violin.args = list(
      mapping = mapping, data = data, position = position, trim = trim, scale = scale,
      relative_scale = relative_scale, fill = fill, color = violin.color
    ) %>%
    discard(is.null)
  violin = do.call(geom_grouped_violin, violin.args)

  #build interval annotations
  interval.args =
    list(mapping = mapping, position = position, data = data, fun.data = fun.data, fill = NA, .width = .width, fun.args = fun.args) %>%
    {if (!is.null(color)) modifyList(., list(color = color)) else .} %>%
    {if (!is.null(size)) modifyList(., list(size = size)) else .} %>%
    {if (!is.null(size_domain)) modifyList(., list(size_domain = size_domain)) else .} %>%
    {if (!is.null(size_range)) modifyList(., list(size_range = size_range)) else .} %>%
    {if (!is.null(fatten_point)) modifyList(., list(fatten_point = fatten_point)) else .}

  interval = do.call(stat_pointinterval, interval.args)

  # we return a list of geoms that can be added to a ggplot object, as in
  # > ggplot(...) + list(geom_a(), geom_b())
  # which is equivalent to
  # > ggplot(...) + geom_a() + geom_b()
  list(violin, interval)
}
