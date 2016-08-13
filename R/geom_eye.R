# geom_eye and ggeye for eye plots with intervals
# 
# Author: mjskay
###############################################################################

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

ggeye = function(data=NULL, mapping=NULL, ...) {
    ggplot(data=data, mapping=mapping) + geom_eye(...) + coord_flip()
}

#function for use with stat_summary that returns the mode and
#highest density interval of the data
mode_hdi = function(x, ...) {
    interval = coda::HPDinterval(coda::mcmc(x), ...)
    data.frame(
        ymin = interval[,"lower"],
        ymax = interval[,"upper"],
        y = modeest::parzen(x, ...)
    ) 
}
