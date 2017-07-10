# geom_eyeh for eye plots with intervals
# 
# Author: mjskay
###############################################################################



#' Horizontal eye plots (aka raindrop plots, aka violin plots) of densities with point
#' estimates and intervals for ggplot2
#' 
#' Generates a combination geom_violinh and geom_pointrangeh (using stat_summaryh)
#' representing the density, point estimates, and credible interval. Useful
#' for representing posterior estimates from Bayesian samplers; in that context
#' this is variously called an eye plot, a raindrop plot, or a violin plot
#' (though violin plot is also applied to plots of data, hence its use is not
#' preferred here).
#' 
#' An eye plot is a compact visual summary of the distribution of some samples,
#' used (under various names and with subtle variations) to visualize posterior
#' distributions in Bayesian inference. This instantiation is a combination of
#' a violin plot, point estimate, and credible interval. \code{geom_eyeh()} is
#' equivalent to \code{geom_violinh() + stat_summaryh()} with some reasonable
#' defaults, including color choices and the use of mean with 95\% quantile
#' intervals.
#' 
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param stat Passed to \code{\link{geom_violinh}}. The statistical
#' transformation to use on the data for this layer.
#' @param position Passed to \code{\link{geom_violinh}}. The position adjustment
#' to use for overlapping points on this layer.
#' @param trim Passed to \code{\link{geom_violinh}}. If \code{TRUE} (default),
#' trim the tails of the violins to the range of the data. If \code{FALSE},
#' don't trim the tails.
#' @param scale Passed to \code{\link{geom_violinh}}. If "area" (default), all
#' violins have the same area (before trimming the tails).  If "count", areas
#' are scaled proportionally to the number of observations. If "width", all
#' violins have the same maximum width.
#' @param fill Passed to \code{\link{geom_violinh}}. Fill color of the violin.
#' @param violin.args Other arguments passed to \code{\link{geom_violinh}}.
#' @param ...  Other arguments passed to \code{\link{geom_violinh}}.
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
#' @param interval_function Deprecated. Use \code{point_interval} above.
#' @param color Passed to \code{\link{stat_summaryh}}. Color of the point
#' estimate and credible interval.
#' @param size Passed to \code{\link{stat_summaryh}}. Line weight of the point
#' estimate and credible interval.
#' @param interval.args Other arguments passed to \code{\link{stat_summaryh}}.
#' @author Matthew Kay
#' @seealso See \code{\link{geom_eye}} for the vertical version. See 
#' \code{\link{geom_violinh}} and \code{\link{stat_summaryh}} for the geoms 
#' this function is based on.
#' @keywords manip
#' @examples
#' 
#' ##TODO
#' 
#' @importFrom utils modifyList
#' @importFrom ggstance stat_summaryh geom_pointrangeh geom_violinh
#' @import ggplot2
#' @export
geom_eyeh = function(
    #shared properties
    mapping = NULL, data = NULL,
    
    #violin properties
    stat = "xdensity", position = "dodgev", trim = TRUE, scale = "area", fill = "gray65", 
    violin.args = list(), ...,
    
    #stat_summaryh properties
    fun.data = mean_qih, fun.args = list(),
    color = NULL, size = NULL, 
    interval.args = list(geom = "pointrangeh", position = "identity")
) {

    #build violin plot
    violin.args = 
        list(mapping = mapping, data = data, stat = stat, position = position, trim = trim,
            scale = scale, color = NA, fill = fill, size = 0.5, alpha = 1.0, linetype = "solid", ...) %>%
        {if (!is.null(fill)) modifyList(., list(fill=fill)) else .} %>%
        modifyList(violin.args)
    violin = do.call(geom_violinh, violin.args)

    #build interval annotations
    interval.args =
        list(mapping = mapping, data = data, fun.data = fun.data, fill = NA, fun.args = fun.args) %>%
        {if (!is.null(color)) modifyList(., list(color=color)) else .} %>%
        {if (!is.null(size)) modifyList(., list(size=size)) else .} %>%
        modifyList(interval.args)
    interval = do.call(stat_summaryh, interval.args)
    
    #we return a list of geoms that can be added to a ggplot object, as in 
    #ggplot(...) + list(geom_a(), geom_b()), which is equivalent to 
    #ggplot(...) + geom_a() + geom_b()
    list(violin, interval)
}


#' @import ggplot2
#' @export
ggeye = function(data = NULL, mapping = NULL, ...) {
    .Deprecated("geom_eyeh")
    ggplot(data=data, mapping=mapping) + geom_eye(...) + coord_flip()
}
