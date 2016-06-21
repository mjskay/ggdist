# tidy_link and tidy_sim
# 
# Author: mjskay
###############################################################################

tidy_link = function(data, fit, name=mu, ...) {
    name = as.character(substitute(name))
    tidy_link_sim_(link, data, fit, name, ...)
}

tidy_sim = function(data, fit, name=mu, ...) {
    name = as.character(substitute(name))
    tidy_link_sim_(sim, data, fit, name, ...)
}

tidy_link_sim_ = function(fun, data, fit, name, ...) {
    if (missing(data)) {
        data = as.data.frame(fit@data)
    }
    
    #get link/sim results as a matrix
    #those functions don't always play well with tbl_dfs, so convert `data` 
    #to a data.frame on the way in
    l = fun(fit, as.data.frame(data), ...)
    
    if (is.list(l)) {
        #if there are multiple link definitions in the model, link() will return
        #a list of matrices (indexed by the link parameter name) instead of a single matrix
        l = l[name]
    }
    
    #tidy it up: make it long format with a ".sample" column
    tidy_l = data.frame(
        .sample = 1:nrow(l),
        .result = as.vector(l)
    ) %>%
        #add the predictors back in for each sample
        group_by(.sample) %>%
        do(cbind(., data)) %>%
        #group by the predictors
        group_by_(.dots=names(data)) %>%
        #rename the result column to what the user specified
        rename_(.dots = setNames(list(~.result), name))
    
    tidy_l
}
