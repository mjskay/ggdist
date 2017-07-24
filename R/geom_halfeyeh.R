# geom_halfeyeh for horizontal half-eye plots with intervals
#
# Author: mjskay
###############################################################################



#' Horizontal half-eye plots of densities with point
#' estimates and intervals for ggplot2
#'
#' Generates a combination geom_joy and geom_pointrangeh (using stat_summaryh)
#' representing the density, point estimates, and credible interval. Useful
#' for representing posterior estimates from Bayesian samplers; in that context
#' the mirrored verison is variously called an eye plot, a raindrop plot, or a
#' violin plot; hence "half-eye" for this plot.
#'
#' A half-eye plot is a compact visual summary of the distribution of some samples,
#' used (under various names and with subtle variations) to visualize posterior
#' distributions in Bayesian inference. This instantiation is a combination of
#' a density plot, point estimate, and credible interval. \code{geom_halfeyeh()} is
#' equivalent to \code{geom_joy() + stat_summaryh()} with some reasonable
#' defaults, including color choices and the use of mean with 95\% quantile
#' intervals.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param stat Passed to \code{\link{geom_joy}}. The statistical
#' transformation to use on the data for this layer.
#' @param position Passed to \code{\link{geom_joy}}. The position adjustment
#' to use for overlapping points on this layer.
#' @param scale Passed to \code{\link{geom_joy}}. A scaling factor to scale the
#' height of the densities relative to the spacing between them. A value of 1
#' indicates that the maximum point of any density touches the baseline right
#' above, assuming even spacing between baselines.
#' @param rel_min_height Densities at heights below this cutoff will be removed.
#' The cutoff is measured relative to the overall maximum, so rel_min_height=0.01
#' would remove everything that is 1% or less than the highest point among all
#' densities. Default is 0, so nothing is removed.
#' @param fill Passed to \code{\link{geom_joy}}. Fill color of the density.
#' @param density.args Other arguments passed to \code{\link{geom_joy}}.
#' @param ...  Other arguments passed to \code{\link{geom_joy}}.
#' @param fun.data The function used to construct point estimates and
#' credible intervals from the data. Should be a function that takes a vector
#' of data as input and returns a data frame with columns \code{y},
#' \code{ymin}, and \code{ymax}, representing the point estimate, lower bound
#' of the credible interval, and upper bound of the credible interval. See
#' \code{\link{point_interval}} for examples; some
#' typical functions to consider here are \code{\link{mean_qi}} (mean
#' with quantile interval), \code{\link{median_qi}} (median with quantile
#' interval), and \code{\link{mode_hdi}} (mode with highest density interval).
#' Alias for \code{fun.data}.
#' @param fun.args Optional arguments passed to \code{fun.data}.
#' @param color Passed to \code{\link{stat_summaryh}}. Color of the point
#' estimate and credible interval.
#' @param size Passed to \code{\link{stat_summaryh}}. Line weight of the point
#' estimate and credible interval.
#' @param interval.args Other arguments passed to \code{\link{stat_summaryh}}.
#' @author Matthew Kay
#' @seealso See \code{\link{geom_eye}} and \code{\link{geom_eyeh}} for the mirrored-density
#' (full "eye") versions. See \code{\link{geom_joy}} and \code{\link{stat_summaryh}} for the geoms
#' this function is based on.
#' @keywords manip
#' @examples
#'
#' ##TODO
#'
#' @importFrom utils modifyList
#' @importFrom ggstance stat_summaryh geom_pointrangeh
#' @importFrom ggjoy geom_joy
#' @import ggplot2
#' @export
geom_halfeyeh = function(
  #shared properties
  mapping = NULL, data = NULL,

  #deity properties
  stat = "joy", position = "identity", scale = 0.9, rel_min_height = 0, fill = "gray65",
  density.args = list(), ...,

  #stat_summaryh properties
  fun.data = mean_qih, fun.args = list(),
  color = NULL, size = NULL,
  interval.args = list(geom = "pointrangeh", position = "identity")
) {

  #build density plot
  density.args =
    list(mapping = mapping, data = data, stat = stat, position = position, scale = scale,
      rel_min_height = rel_min_height, color = NA, ...) %>%
      {if (!is.null(fill)) modifyList(., list(fill = fill)) else .} %>%
    modifyList(density.args)
  joy = do.call(geom_joy, density.args)

  #build interval annotations
  interval.args =
    list(mapping = mapping, data = data, fun.data = fun.data, fill = NA, fun.args = fun.args) %>%
    {if (!is.null(color)) modifyList(., list(color = color)) else .} %>%
    {if (!is.null(size)) modifyList(., list(size = size)) else .} %>%
    modifyList(interval.args)
  interval = do.call(stat_summaryh, interval.args)

  # we return a list of geoms that can be added to a ggplot object, as in
  # > ggplot(...) + list(geom_a(), geom_b())
  # which is equivalent to
  # > ggplot(...) + geom_a() + geom_b()
  list(joy, interval)
}
