# geom_halfeyeh for horizontal half-eye plots with intervals
#
# Author: mjskay
###############################################################################



#' Half-eye plots of densities with point and interval summaries (ggplot geom)
#'
#' Generates a combination of a density and \code{\link{stat_pointintervalh}}
#' representing the density, point summary, and uncertainty intervals
#' for draws from a distribution. Useful for representing posteriors from Bayesian models;
#' in that context the mirrored version is variously called an eye plot, a raindrop plot,
#' violin plot; hence "half-eye" for this plot.
#'
#' A half-eye plot is a compact visual summary of the distribution of a sample,
#' used (under various names and with subtle variations) to visualize posterior
#' distributions in Bayesian inference. This instantiation is a combination of
#' a density plot, point summary, and one or more uncertainty intervals.
#'
#' \code{geom_halfeyeh()} is roughly equivalent to \code{geom_density_ridges() + stat_pointintervalh()}
#' with some reasonable defaults, including color choices and the use of median with 95\%
#' and 66\% quantile intervals.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param position The position adjustment
#' to use for overlapping points on this layer.
#' @param trim If \code{TRUE} (default),
#' trim the tails of the density to the range of the data. If \code{FALSE},
#' don't trim the tails.
#' @param scale If "area" (default), all
#' densities have the same area (before trimming the tails).  If "count", areas
#' are scaled proportionally to the number of observations. If "width", all
#' densities have the same maximum width/height.
#' @param relative_scale A relative scaling factor to determine how much of the available
#' space densities are scaled to fill: if \code{1}, all available space is filled.
#' @param fill Fill color of the density.
#' @param density.color Outline color of the density.
#' The default, \code{NA}, suppresses the density outline. Set to another value to set the density outline color
#' manually, or set to \code{NULL} if you want the outline color of the density to be determined by the aesthetic
#' mapping.
#' @param ...  Currently unused.
#' @param point_interval A function that when given a vector should
#'   return a data frame with variables \code{y}, \code{ymin}, \code{ymax}, and \code{.width}; or
#'   \code{x}, \code{xmin}, \code{xmax}, and \code{.width}. \strong{Either is acceptable}: output
#'   will be converted into the \code{x}-based aesthetics \code{geom_halfeyeh}.
#'   See the \code{point_interval} family of functions.
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
#' @param color Passed to \code{\link{stat_pointintervalh}}. Color of the point
#' summary and uncertainty interval.
#' @param size Passed to \code{\link{stat_pointintervalh}}. Line weight of the point
#' summary and uncertainty interval.
#' @author Matthew Kay
#' @seealso See \code{\link{geom_eye}} and \code{\link{geom_eyeh}} for the mirrored-density
#' (full "eye") versions. See \code{\link[ggridges]{geom_density_ridges}} and \code{\link{stat_summaryh}} for the geoms
#' this function is based on.
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
#'   geom_halfeyeh()
#'
#' @importFrom utils modifyList
#' @importFrom ggstance stat_summaryh geom_pointrangeh
#' @import ggplot2
#' @export
geom_halfeyeh = function(
  #shared properties
  mapping = NULL, data = NULL,

  #density properties
  position = "identity", trim = TRUE, scale = "area", relative_scale = 1, fill = NULL, density.color = NA,

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

  fun.data = fun.data %||% horizontal_aes(point_interval)

  #build violin plot
  density.args = list(
      mapping = mapping, data = data, position = position, trim = trim, scale = scale,
      relative_scale = relative_scale, side = "top", fill = fill, color = density.color
    ) %>%
    discard(is.null)
  dens = do.call(geom_grouped_violinh, density.args)

  #build interval annotations
  interval.args =
    list(mapping = mapping, position = position, data = data, fun.data = fun.data, fill = NA, .width = .width, fun.args = fun.args) %>%
    {if (!is.null(color)) modifyList(., list(color = color)) else .} %>%
    {if (!is.null(size)) modifyList(., list(size = size)) else .} %>%
    {if (!is.null(size_domain)) modifyList(., list(size_domain = size_domain)) else .} %>%
    {if (!is.null(size_range)) modifyList(., list(size_range = size_range)) else .} %>%
    {if (!is.null(fatten_point)) modifyList(., list(fatten_point = fatten_point)) else .}

  interval = do.call(stat_pointintervalh, interval.args)

  # we return a list of geoms that can be added to a ggplot object, as in
  # > ggplot(...) + list(geom_a(), geom_b())
  # which is equivalent to
  # > ggplot(...) + geom_a() + geom_b()
  list(dens, interval)
}
