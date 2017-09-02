# geom_eye for eye plots with intervals
#
# Author: mjskay
###############################################################################


#' Eye plots (aka raindrop plots, aka violin plots) of densities with point
#' estimates and intervals for ggplot2
#'
#' Generates a combination \code{\link{geom_violin}} and \code{\link{stat_pointinterval}}
#' (for \code{geom_eye}) or \code{\link{geom_violinh}} and \code{\link{stat_pointintervalh}}
#' (for \code{geom_eyeh}) representing the density, point estimates, and uncertainty intervals
#' for some samples. Useful for representing posterior estimates from Bayesian samplers;
#' in that context this is variously called an eye plot, a raindrop plot, or a violin plot
#' (though violin plot is also applied to plots of data, hence its use is not
#' preferred here).
#'
#' An eye plot is a compact visual summary of the distribution of some samples,
#' used (under various names and with subtle variations) to visualize posterior
#' distributions in Bayesian inference. This instantiation is a combination of
#' a violin plot, point estimate, and credible interval.
#'
#' The vertical form, \code{geom_eye}, is equivalent to  \code{geom_violin() + stat_pointinterval()}
#' with some reasonable defaults, including color choices and the use of mean with 95\%
#' and 6\% quantile intervals.
#'
#' The horizontal form, \code{geom_eyeh()}, is equivalent to \code{geom_violinh() + stat_pointintervalh()}.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param stat Passed to \code{\link{geom_violin}} / \code{\link{geom_violinh}}. The statistical
#' transformation to use on the data for this layer.
#' @param position Passed to \code{\link{geom_violin}} / \code{\link{geom_violinh}}. The position adjustment
#' to use for overlapping points on this layer.
#' @param trim Passed to \code{\link{geom_violin}} / \code{\link{geom_violinh}}. If \code{TRUE} (default),
#' trim the tails of the violins to the range of the data. If \code{FALSE},
#' don't trim the tails.
#' @param scale Passed to \code{\link{geom_violin}} / \code{\link{geom_violinh}}. If "area" (default), all
#' violins have the same area (before trimming the tails).  If "count", areas
#' are scaled proportionally to the number of observations. If "width", all
#' violins have the same maximum width.
#' @param fill Passed to \code{\link{geom_violin}} / \code{\link{geom_violinh}}. Fill color of the violin.
#' @param ...  Currently unused.
#' @param fun.data A function that is given a vector and should
#'   return a data frame with variables \code{y}, \code{ymin} and \code{ymax}
#'   (\code{x}, \code{xmin} and \code{xmax} for \code{stat_pointintervalh}),
#'   and \code{.prob}. See the \code{point_interval} family of functions.
#' @param point.interval Alias for \code{fun.data}
#' @param fun.args Optional arguments passed to \code{fun.data}.
#' @param .prob The \code{.prob} argument passed to \code{fun.data}.
#' @param color Passed to \code{\link{stat_pointintervalh}}. Color of the point
#' estimate and credible interval.
#' @param size Passed to \code{\link{stat_pointintervalh}}. Line weight of the point
#' estimate and credible interval.
#' @author Matthew Kay
#' @seealso See \code{\link{geom_halfeyeh}}
#' for the non-mirrored density ("half eye") version. See \code{\link{geom_violin}} and \code{\link{stat_pointinterval}}
#' for the geoms these functions are based on.
#' @keywords manip
#' @examples
#'
#' library(magrittr)
#' library(ggplot2)
#' data(RankCorr)
#'
#' RankCorr %>%
#'   spread_samples(u_tau[i]) %>%
#'   ggplot(aes(y = i, x = u_tau)) +
#'   geom_eyeh()
#'
#' RankCorr %>%
#'   spread_samples(u_tau[i]) %>%
#'   ggplot(aes(x = i, y = u_tau)) +
#'   geom_eye()
#'
#' @importFrom utils modifyList
#' @import ggplot2
#' @export
geom_eye = function(
  #shared properties
  mapping = NULL, data = NULL,

  #violin properties
  stat = "ydensity", position = "dodge", trim = TRUE, scale = "area", fill = "gray65",

  ...,

  #stat_summaryh properties
  point.interval = mean_qi,
  fun.data = point.interval,
  fun.args = list(),
  .prob = c(.95, .66),
  color = NULL, size = NULL, fatten.interval = NULL, fatten.point = NULL
) {

  #build violin plot
  violin.args =
    list(mapping = mapping, data = data, stat = stat, position = position, trim = trim,
      scale = scale, color = NA, fill = fill, size = 0.5, alpha = 1.0, linetype = "solid"
    ) %>%
    {if (!is.null(fill)) modifyList(., list(fill = fill)) else .}

  violin = do.call(geom_violin, violin.args)

  #build interval annotations
  interval.args =
    list(mapping = mapping, data = data, fun.data = fun.data, fill = NA, .prob = .prob, fun.args = fun.args) %>%
    {if (!is.null(color)) modifyList(., list(color = color)) else .} %>%
    {if (!is.null(size)) modifyList(., list(size = size)) else .} %>%
    {if (!is.null(fatten.interval)) modifyList(., list(fatten.interval = fatten.interval)) else .} %>%
    {if (!is.null(fatten.point)) modifyList(., list(fatten.point = fatten.point)) else .}

  interval = do.call(stat_pointinterval, interval.args)

  # we return a list of geoms that can be added to a ggplot object, as in
  # > ggplot(...) + list(geom_a(), geom_b())
  # which is equivalent to
  # > ggplot(...) + geom_a() + geom_b()
  list(violin, interval)
}
