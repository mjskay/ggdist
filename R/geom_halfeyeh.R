# geom_halfeyeh for horizontal half-eye plots with intervals
#
# Author: mjskay
###############################################################################



#' Half-eye plots of densities with point estimates and intervals (ggplot geom)
#'
#' Generates a combination geom_density_ridges and geom_pointrangeh (using stat_summaryh)
#' representing the density, point estimates, and credible interval. Useful
#' for representing posterior estimates from Bayesian samplers; in that context
#' the mirrored verison is variously called an eye plot, a raindrop plot, or a
#' violin plot; hence "half-eye" for this plot.
#'
#' A half-eye plot is a compact visual summary of the distribution of some samples,
#' used (under various names and with subtle variations) to visualize posterior
#' distributions in Bayesian inference. This instantiation is a combination of
#' a density plot, point estimate, and credible interval. \code{geom_halfeyeh()} is
#' equivalent to \code{geom_density_ridges() + stat_summaryh()} with some reasonable
#' defaults, including color choices and the use of mean with 95\% quantile
#' intervals.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param position Passed to \code{\link{geom_density_ridges}}. The position adjustment
#' to use for overlapping points on this layer.
#' @param trim If \code{TRUE} (default),
#' trim the tails of the density to the range of the data. If \code{FALSE},
#' don't trim the tails.
#' @param scale If "area" (default), all
#' densities have the same area (before trimming the tails).  If "count", areas
#' are scaled proportionally to the number of observations. If "width", all
#' densities have the same maximum width/height.
#' @param fill Fill color of the density.
#' @param density.color Outline color of the density.
#' The default, \code{NA}, suppresses the density outline. Set to another value to set the density outline color
#' manually, or set to \code{NULL} if you want the outline color of the density to be determined by the aesthetic
#' mapping.
#' @param ...  Currently unused.
#' @param fun.data A function that is given a vector and should
#'   return a data frame with variables \code{x}, \code{xmin} and \code{xmax}
#'   and \code{.prob}. See the \code{point_interval} family of functions.
#' @param point.interval Alias for \code{fun.data}
#' @param fun.args Optional arguments passed to \code{fun.data}.
#' @param .prob The \code{.prob} argument passed to \code{fun.data}.
#' @param size_domain The minimum and maximum of the values of the size aesthetic that will be translated into actual
#' sizes drawn according to \code{size_range} (see the documentation for that parameter, below.)
#' @param size_range This geom scales the raw size aesthetic values, as they tend to be too thick when using the default
#' settings of \code{\link{scale_size_continuous}}, which give sizes with a range of \code{c(1, 6)}. The
#' \code{size_domain} value indicates the input domain of raw size values (typically this should be equal to the value
#' of the \code{range} parameter of the \code{\link{scale_size_continuous}} function), and \code{size_range} indicates
#' the desired output range of the size values (the min and max of the actual sizes used to draw intervals).
#' @param fatten_point A multiplicative factor used to adjust the size of the point relative to the size of the
#' thickest line.
#' @param color Passed to \code{\link{stat_pointintervalh}}. Color of the point
#' estimate and credible interval.
#' @param size Passed to \code{\link{stat_pointintervalh}}. Line weight of the point
#' estimate and credible interval.
#' @author Matthew Kay
#' @seealso See \code{\link{geom_eye}} and \code{\link{geom_eyeh}} for the mirrored-density
#' (full "eye") versions. See \code{\link{geom_density_ridges}} and \code{\link{stat_summaryh}} for the geoms
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
#'   spread_samples(u_tau[i]) %>%
#'   ggplot(aes(y = i, x = u_tau)) +
#'   geom_halfeyeh()
#'
#' @importFrom utils modifyList
#' @importFrom ggstance stat_summaryh geom_pointrangeh
#' @importFrom ggridges geom_density_ridges
#' @import ggplot2
#' @export
geom_halfeyeh = function(
  #shared properties
  mapping = NULL, data = NULL,

  #density properties
  position = position_dodgev(), trim = TRUE, scale = "area", fill = NULL, density.color = NA,

  ...,

  #stat_summaryh properties
  point.interval = mean_qih,
  fun.data = point.interval,
  fun.args = list(),
  .prob = c(.95, .66),
  color = NULL, size = NULL, size_domain = NULL, size_range = NULL, fatten_point = NULL
) {

  #build violin plot
  density.args = list(
      mapping = mapping, data = data, position = position, trim = trim, scale = scale, side = "top",
      fill = fill, color = density.color
    ) %>%
    discard(is.null)
  dens = do.call(geom_grouped_violinh, density.args)

  #build interval annotations
  interval.args =
    list(mapping = mapping, data = data, fun.data = fun.data, fill = NA, .prob = .prob, fun.args = fun.args) %>%
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
