# compare_levels
# 
# Author: mjskay
###############################################################################

#COMPARISON TYPES
comparison_types = within(list(), {
    ordered = function(x) {
        l = levels(x)
        lapply(2:length(l), function(i) c(l[[i]], l[[i - 1]]))
    }
    
    control = function(x) {
        l = levels(x)
        lapply(l[-1], function(j) c(j, l[[1]]))
    }
    
    pairwise = function(x) {
        #reverse combn so that the control level (first level) is second for
        #consistency with control() and ordered()
        lapply(combn(levels(x), 2, simplify=FALSE), rev)
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
    #drop unused levels from by factor
    samples[[by]] = factor(samples[[by]])

    #get wide version of samples that we can use to generate comparisons easily
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
    if (is.character(comparison)) comparison = as.name(comparison)
    comparison_function = eval(comparison, comparison_types)
    comparison_levels = 
        if (is.list(comparison_function)) comparison_function
        else comparison_function(samples[[by]]) 
    
    #make comparisons
    ldply(comparison_levels, function (levels.) {
            comparison = if (is.language(levels.)) {
                #user-supplied quoted expressions are evaluated within the data frame
                data.frame(
                    by = deparse(levels.),
                    variable = eval(levels., samples_wide)
                )
            }
            else {
                #otherwise, levels should be pairs of strings representing levels
                data.frame(
                    by = paste(levels.[[1]], fun_name, levels.[[2]]),
                    variable = fun(samples_wide[[levels.[[1]]]], samples_wide[[levels.[[2]]]])
                )
            }
            names(comparison) = c(by, variable)
            cbind(samples_wide_no_levels, comparison)
        })
}
