# mean_qi
# 
# Author: mjskay
###############################################################################

mean_qi = function(data, name=mu, prob=.95) {
    name = as.name(substitute(name))
    lower_prob = (1 - prob)/2
    upper_prob = (1 + prob)/2
    dots = list(
        eval(bquote(~quantile(.(name), .(lower_prob)))),
        eval(bquote(~quantile(.(name), .(upper_prob)))),
        #mean has to go last because otherwise the variable named `name` is
        #redefined to be the point estimate, so the quantile estimates are screwed up.
        eval(bquote(~mean(.(name))))
    )
    names(dots) = c(
        paste0(as.character(name), ".lower"),
        paste0(as.character(name), ".upper"),
        as.character(name)
    )
    summarise_(data, .dots = dots)
}
