# tidy_link
# 
# Author: mjskay
###############################################################################

tidy_link = function(data, fit, name=mu, ...) {
    name = as.character(substitute(name))
    
    if (missing(data)) {
        data = as.data.frame(fit@data)
    }
    
    #get link results as a matrix
    l = link(fit, data, ...)
    
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
