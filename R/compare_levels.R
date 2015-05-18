# compare_levels
# 
# Author: mjskay
###############################################################################

#COMPARISON TYPES
comparison_types = within(list(), {
    ordered = function(x) {
        l = levels(x)
        lapply(1:(length(l) - 1), function(i) c(l[[i]], l[[i + 1]]))
    }
    
    control = function(x) {
        l = levels(x)
        lapply(l[-1], function(j) c(l[[1]], j))
    }
    
    pairwise = function(x) {
        combn(levels(x), 2, simplify=FALSE)
    }
    
    default = function(x) {
        if (is.ordered(x)) ordered(x)
        else pairwise(x)
    }
})


## Given samples (long-format data frame resulting from extract_samples)
## generate comparisons of variable by levels of by by applying fun
compare_levels = function(samples, variable, by, fun=`-`, comparison=default) {
    eval(bquote(compare_levels_(samples, 
                .(deparse(substitute(variable))), 
                .(deparse(substitute(by))), 
                .(substitute(fun)),
                .(substitute(comparison))
    )))
}

compare_levels_ = function(samples, variable, by, fun=`-`, comparison=default) {
    samples_wide = spread_(samples, by, variable) 
    
    # determine a pretty function name
    fun_language = substitute(fun)
    fun_name = if (is.name(fun_language)) deparse(fun_language) else ":"

    #get a version of the samples data frame without columns representing
    #the levels we are comparing by (these columns will be included
    #alongside the comparison results for reference)
    by_levels = levels(samples[[by]])
    samples_wide_no_levels = select(samples_wide, -one_of(by_levels))

    #get list of pairs of levels to compare
    comparison = substitute(comparison)
    if (is.character(comparison)) as.name(comparison) else comparison
    comparison_levels = eval(bquote(.(comparison)(samples[[by]])), 
        comparison_types)
    
    #make comparisons
    ldply(comparison_levels, function (levels.) {
            comparison = data.frame(
                by = paste(levels.[[2]], fun_name, levels.[[1]]),
                variable = fun(samples_wide[[levels.[[2]]]], samples_wide[[levels.[[1]]]])
            )
            names(comparison) = c(by, variable)
            cbind(samples_wide_no_levels, comparison)
        })
}
