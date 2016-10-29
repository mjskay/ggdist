# geom_eye and ggeye for eye plots with intervals
# 
# Author: mjskay
###############################################################################



#' Eye plots (aka raindrop plots, aka violin plots) of densities with point
#' estimates and intervals for ggplot2
#' 
#' Generates a combination geom_violin and geom_pointrange (from stat_summary)
#' representing the y density, point estimates, and credible interval. Useful
#' for representing posterior estimates from Bayesian samplers; in that context
#' this is variously called an eye plot, a raindrop plot, or a violin plot
#' (though violin plot is also applied to plots of data, hence its use is not
#' preferred here).
#' 
#' An eye plot is a compact visual summary of the distribution of some samples,
#' used (under various names and with subtle variations) to visualize posterior
#' distributions in Bayesian inference. This instantiation is a combination of
#' a violin plot, point estimate, and credible interval. \code{geom_eye()} is
#' equivalent to \code{geom_violin() + stat_summary()} with some reasonable
#' defaults, including color choices and the use of median with 95\% quantile
#' intervals.
#' 
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param stat Passed to \code{\link{geom_violin}}. The statistical
#' transformation to use on the data for this layer.
#' @param position Passed to \code{\link{geom_violin}}. The position adjustment
#' to use for overlapping points on this layer.
#' @param trim Passed to \code{\link{geom_violin}}. If \code{TRUE} (default),
#' trim the tails of the violins to the range of the data. If \code{FALSE},
#' don't trim the tails.
#' @param scale Passed to \code{\link{geom_violin}}. If "area" (default), all
#' violins have the same area (before trimming the tails).  If "count", areas
#' are scaled proportionally to the number of observations. If "width", all
#' violins have the same maximum width.
#' @param fill Passed to \code{\link{geom_violin}}. Fill color of the violin.
#' @param violin_args Other arguments passed to \code{\link{geom_violin}}.
#' @param ...  Other arguments passed to \code{\link{geom_violin}}.
#' @param interval_function The function used to construct point estimates and
#' credible intervals from the data. Should be a function that takes a vector
#' of data as input and returns a data frame with columns \code{y},
#' \code{ymin}, and \code{ymax}, representing the point estimate, lower bound
#' of the credible interval, and upper bound of the credible interval. Some
#' typical functions to consider here are \code{\link{mean_cl_normal}} (mean
#' with student-t interval), \code{\link{median_hilow}} (median with quantile
#' interval), and \code{\link{mode_hdi}} (mode with highest density interval).
#' Alias for \code{fun.data}.
#' @param fun.data Passed to \code{\link{stat_summary}}. See
#' \code{interval_function} above.
#' @param color Passed to \code{\link{stat_summary}}. Color of the point
#' estimate and credible interval. Alias for \code{colour}.
#' @param colour Passed to \code{\link{stat_summary}}. Color of the point
#' estimate and credible interval. Alias for \code{color}.
#' @param size Passed to \code{\link{stat_summary}}. Line weight of the point
#' estimate and credible interval.
#' @param interval_args Other arguments passed to \code{\link{stat_summary}}.
#' @author Matthew Kay
#' @seealso See \code{\link{ggeye}} for a shortcut combining a call to
#' \code{\link{ggplot}} with \code{geom_eye}. See \code{\link{geom_violin}} and
#' \code{\link{stat_summary}} for the geoms this function is based on.
#' @keywords manip
#' @examples
#' 
#' ##TODO
#' 
#' @importFrom utils modifyList
#' @import ggplot2
#' @export
geom_eye = function(
    #shared properties
    mapping = NULL, data = NULL,
    
    #violin properties
    stat = "ydensity", position = "dodge", trim = TRUE, scale = "area", fill = "skyblue", 
    violin_args = list(), ...,
    
    #stat_summary properties
    interval_function="median_hilow", fun.data=interval_function, 
    color = NULL, size = NULL, 
    interval_args = list(geom = "pointrange", position = "identity")
) {

    #build violin plot
    violin_args = 
        list(mapping=mapping, data=data, stat=stat, position=position, trim=trim, scale=scale, color=NA, 
            fill=fill, size=0.5, alpha=1.0, linetype = "solid", ...) %>%
        {if (!is.null(fill)) modifyList(., list(fill=fill)) else .} %>%
        modifyList(violin_args)
    violin = do.call(geom_violin, violin_args)

    #build interval annotations
    interval_args =
        list(mapping=mapping, data=data, fun.data=fun.data, fill=NA) %>%
        {if (!is.null(color)) modifyList(., list(color=color)) else .} %>%
        {if (!is.null(size)) modifyList(., list(size=size)) else .} %>%
        modifyList(interval_args)
    interval = do.call(stat_summary, interval_args)
    
    #we return a list of geoms that can be added to a ggplot object, as in 
    #ggplot(...) + list(geom_a(), geom_b()), which is equivalent to 
    #ggplot(...) + geom_a() + geom_b()
    list(violin, interval)
}



#' Shortcut for eye plots with ggplot2
#' 
#' This function is a shortcut for ggplot() + geom_eye() + coord_flip.
#' 
#' \code{ggeye(data, mapping, ...)} is a shortcut for \code{ggplot(data,
#' mapping) + geom_eye(...) + coord_flip()}.
#' 
#' @param data Passed to \code{\link{ggplot}}. Default data set.
#' @param mapping Passed to \code{\link{ggplot}}. The aesthetic mapping,
#' usually constructed with \code{\link{aes}} or \code{\link{aes_string}}.
#' @param ...  Other arguments passed to \code{\link{geom_eye}}.
#' @author Matthew Kay
#' @seealso See \code{\link{geom_eye}} for more information.
#' @keywords manip
#' @examples
#' 
#' ##TODO
#' 
#' @import ggplot2
#' @export
ggeye = function(data=NULL, mapping=NULL, ...) {
    ggplot(data=data, mapping=mapping) + geom_eye(...) + coord_flip()
}

#function for use with stat_summary that returns the mode and
#highest density interval of the data


#' Mode and highest-density interval
#' 
#' Returns mode and highest-density interval of x, for use with geom_eye or
#' stat_summary.
#' 
#' Uses \code{\link[modeest]{parzen}} and \code{\link[coda]{HPDinterval}} to
#' generate mode and HDI of \code{x}.
#' 
#' \code{mode_hdi} returns its results in a format compatible with
#' \code{\link{stat_summary}}; it may be used for the \code{fun.data} argument
#' of that function and the \code{interval_function} argument of
#' \code{\link{geom_eye}}.
#' 
#' @param x A vector of data.
#' @param ...  Other arguments passed to \code{\link[coda]{HPDinterval}}.
#' @return A \code{data.frame} with the following columns:
#' 
#' \item{y}{ The mode of \code{x}. } \item{ymin}{ The lower bound of the
#' highest-density interval of \code{x}. } \item{ymax}{ The upper bound of the
#' highest-density interval of \code{x}. }
#' @author Matthew Kay
#' @seealso See \code{\link{geom_eye}} for more information.
#' @keywords manip
#' @examples
#' 
#' ##TODO
#' 
#' @importFrom coda HPDinterval mcmc
#' @importFrom LaplacesDemon Mode
#' @export
mode_hdi = function(x, ...) {
    interval = HPDinterval(mcmc(x), ...)
    data.frame(
        ymin = interval[,"lower"],
        ymax = interval[,"upper"],
        y = Mode(x)
    ) 
}
