# geom_eye and ggeye for eye plots with intervals
# 
# Author: mjskay
###############################################################################

geom_eye = function(
    mapping = NULL, data = NULL,    #shared properties 
    stat = "ydensity", position = "dodge", trim = TRUE, scale = "area", fill = NULL, violin_args = list(), ..., #violin
    interval_function="median_hilow", fun.data=interval_function, color = NULL, colour = color, interval_args = list(geom = "pointrange", position = "identity") #stat_summary   
) {

    #build violin plot
    violin_args = 
        list(mapping=mapping, data=data, stat=stat, position=position, trim=trim, scale=scale, colour=NA, ...) %>%
        {if (!is.null(fill)) modifyList(., list(fill=fill)) else .} %>%
        modifyList(violin_args)
    violin = do.call(geom_violin, violin_args)
    #custom defaults (I don't like the white background or outline on the violin)
    violin$geom$default_aes = function(.) aes(weight=1, colour=NA, fill="skyblue", size=0.5, alpha=1.0, linetype = "solid")

    #build interval annotations
    interval_args =
        list(mapping=mapping, data=data, fun.data=fun.data, fill=NA) %>%
        {if (!is.null(colour)) modifyList(., list(colour=colour)) else .} %>%
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
