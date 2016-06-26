# tidy_link and tidy_sim
# 
# Author: mjskay
###############################################################################

#return the parameter name for the first link in the given model (or
#the only one, if there is only one)
first_link_name = function(fit) UseMethod("first_link_name", fit)
first_link_name.map = function(fit) fit@links[[1]][[1]]
first_link_name.map2stan = function(fit) fit@formula_parsed$lm[[1]]$parameter

tidy_link = function(data, fit, ...) {
    tidy_link_sim_(link, data, fit, 
        #if there is only a single link in the model, link() will
        #return a matrix instead of a list of matrices, so we must give the name
        #of the link variable here so that tidy_link_sim_ can include the name
        #in the results
        single_response_name = first_link_name(fit), 
        ...)
}

tidy_sim = function(data, fit, name=y, ...) {
    name = as.character(substitute(name))
    tidy_link_sim_(sim, data, fit, single_response_name = name, ...)
}

tidy_link_sim_ = function(fun, data, fit, 
    single_response_name, #name used when only one response is returned
    ...) {
    
    if (missing(data)) {
        data = as.data.frame(fit@data)
    }
    
    # get link/sim results as a matrix or list of matrices
    # those functions don't always play well with tbl_dfs, so convert `data`
    # to a data.frame on the way in
    responses = fun(fit, as.data.frame(data), ...)
    
    if (!is.list(responses)) {
        #if there is only one link definition in the model, or if we are using sim(), 
        #only a single matrix will be returned, instead of a list of matrices indexed 
        #by the link parameter name. Thus we must convert it into a list
        responses = list(responses)
        names(responses) = single_response_name
    }
    
    #tidy it up: make it long format with a ".sample" column and a single column for each response
    cbind(
        .sample = 1:nrow(responses[[1]]), 
        #flatten each response matrix into a single vector; the sample index above
        #will be repeated to fit the length of the flattened vector, thus becomming
        #equivalent to the row index in the original matrix
        as.data.frame(lapply(responses, function(r) as.vector(r)))
    ) %>%
        #add the predictors back in for each sample
        group_by(.sample) %>%
        do(cbind(., data)) %>%
        #group by the predictors so that functions like mean_qi() "just work" on the
        #resulting data frame
        group_by_(.dots = names(data))
}
