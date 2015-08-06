# extract_samples
# 
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in global_variables.R
globalVariables(c("..", ".variable", ".value"))


extract_samples = function(model, variable_spec) {
    extract_samples_(model, lazy(variable_spec))
}
extract_samples_ = function(model, variable_spec) {
    #parse a variable spec in the form variable_name[index_name_1, index_name_2, ..] | wide_index
    spec = lazy_eval(variable_spec, data=list(
        `[` = function(variable_names, ...) {
            #helper function to translate variable names into a list
            translate_variable_names = function(quoted_variable_names) { 
                switch(class(quoted_variable_names),
                    name = as.character(quoted_variable_names),
                    call = unlist(lapply(quoted_variable_names[-1], translate_variable_names)),
                    `(` = translate_variable_names(quoted_variable_names[[2]])
                )
            }
            list(
                translate_variable_names(substitute(variable_names)),
                as.character(substitute(list(...))[-1]),
                NA)
        },
        `|` = function(spec, by) c(
            spec[1:2],
            as.character(substitute(by))
            )
        ))
    variable_names = spec[[1]]
    index_names = if (identical(spec[[2]], "")) NULL else spec[[2]]
    wide_index_name = spec[[3]]
    
    #extract the samples into a long data frame
    samples = extract_samples_long_(model, variable_names, index_names)
    
    #convert variable and/or indices back into usable data types
    constructors = attr(model, "constructors")
    if (is.null(constructors)) constructors = list()
    for (column_name in c(variable_names, index_names)) {
        if (column_name %in% names(constructors)) {
            #we have a data type constructor for this index, convert it
            samples[[column_name]] = constructors[[column_name]](samples[[column_name]])
        }
    }
    
    #spread a column into wide format if requested (only if one variable, because
    #we can't spread multiple keys simultaneously for the same value)
    if (!is.na(wide_index_name)) {
        #wide index requested by name
        if (length(variable_names) != 1) {
            stop("Cannot extract samples of multiple variables in wide format.")
        }
        samples %>%
            spread_(wide_index_name, variable_names)
    }
    else if (!is.null(samples$..)) {
        #a column named ".." is present, use it to form a wide version of the data
        #with numbered names based on the variable name
        if (length(variable_names) != 1) {
            stop("Cannot extract samples of multiple variables in wide format.")
        }
        samples %>%
            mutate(.. = factor(.., labels=variable_names)) %>%
            spread_("..", variable_names)
    }
    else {
        #no wide column => just return long version
        samples
    }
}


## Extract a sample from an MCMC chain for a variable with the given named indices into a long-format data frame.
## For example, imagine a variable b[i,v] with i in [1..100] and v in [1..3]. An MCMC sample returned from JAGS 
## (for example) would have columns with names like "b[1,1]", "b[2,1]", etc. 
##
## extract_samples_long_(mcmcChain, "b", c("i", "v")) would return a data frame with:
##		column ".sample": values in [1..nrow(mcmcChain)]
## 		column "i":		  values in [1..100]
##		column "v":		  values in [1..3]
##      column "b":       sample number ".sample" of "b[i,v]" in mcmcChain 
##
extract_samples_long_ = function(model, variable_names, index_names) UseMethod("extract_samples_long_")
extract_samples_long_.default = function(model, variable_names, index_names) {
    if (is.null(index_names)) {
        #no indices, just return the samples with a sample index added
        model %>% as.data.frame() %>%
            mutate(.sample = 1:nrow(.)) %>%
            select_(.dots = c(".sample", variable_names))
    }
    else {
        #determine what variable to extract
        variable_regex = paste0("^(", paste(variable_names, collapse="|"), ")\\[")
        variable_names_index = stri_detect_regex(dimnames(model)[[2]], variable_regex)

        #rename columns to drop trailing "]" to eliminate extraneous last column
        #when we do separate(), below
        dimnames(model)[[2]] = stri_sub(dimnames(model)[[2]], to=-2)

        #subset and convert to data frame
        model[,variable_names_index] %>%
            #make long format with a sample index
            melt(varnames=c(".sample",".variable"), value.name=".value",
                as.is=TRUE  #don't convert strings to factors here since we're just going to apply separate to them
            ) %>%
            #next, split indices in variable names into columns
            separate(.variable, c(".variable", index_names), sep="(\\,|\\[|\\])", 
                convert=TRUE #converts indices to numerics
            ) %>%
            #now, make the value of each variable a column
            spread(.variable, .value)
    }
}
extract_samples_long_.stanfit = function(model, ...) {
    extract_samples_long_(as.matrix(model), ...)
}
extract_samples_long_.mcmc = function(model, ...) {
    extract_samples_long_(as.matrix(model), ...)
}
extract_samples_long_.mcmc.list = function(model, ...) {
    extract_samples_long_(as.matrix(model), ...)
}
extract_samples_long_.runjags = function(model, ...) {
    requireNamespace("runjags")
    extract_samples_long_(coda::as.mcmc.list(model), ...)
}
