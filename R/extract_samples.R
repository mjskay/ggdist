# extract_samples
# 
# Author: mjskay
###############################################################################


## Extract a sample from an MCMC chain for a variable with the given named indices into a long-format data frame.
## For example, imagine a variable b[i,v] with i in [1..100] and v in [1..3]. An MCMC sample returned from JAGS 
## (for example) would have columns with names like "b[1,1]", "b[2,1]", etc. 
##
## extract_samples(mcmcChain, ~ b[i,v]) would return a data frame with:
##		column ".sample": value in [1..nrow(mcmcChain)]
## 		column "i":		  value in [1..20]
##		column "v":		  value in [1..3]
##      column "b":       value of "b[i,v]" for sample number ".sample" in mcmcChain 
##
## The shorthand ".." can be used to specify one column that should be put into a wide format. For example:
##
## extract_samples(mcmcChain, ~ b[i,..]) would return a data frame with:
##		column ".sample": value in [1..nrow(mcmcChain)]
## 		column "i":		  value in [1..20]
##      column "b1":       value of "b[i,1]" for sample number ".sample" in mcmcChain 
##      column "b2":       value of "b[i,2]" for sample number ".sample" in mcmcChain 
##      column "b3":       value of "b[i,3]" for sample number ".sample" in mcmcChain 
##
## prototypes optionally specifies a list or data.frame. Each entry in prototypes with the same name
## as the variable or an index in varible_spec is a used as a prototype for that variable or index -- 
## i.e., its type is taken to be the expected type of that variable or index. 
## Those types are used to translate numeric values of variables back into useful values (usually levels 
## of a factors). 
##
## The most common use of prototypes is to automatically translate indices that correspond to levels of a factor
## in the original data back into levels of that factor. Names in prototypes that are not in 
## found in variable_spec are ignored.
##
## The usual use of prototypes is to pass in the data frame from which the original data came. 
## Supported types of prototypes are factor, ordered, and logical. For example:
##
## 		if data_types$v is a factor, the v column in the returned samples is translated into 
##      a factor using factor(v, labels=levels(index_types$v), ordered=is.ordered(index_types$v))
##
## 		if data_types$v is a logical, the v column is translated into a logical using as.logical(v)
##
extract_samples = function(model, variable_spec) {
    #first, extract the samples into a data frame
    variable_name = as.character(variable_spec[[2]][[2]])
    index_names = as.character(variable_spec[[2]][-1:-2])
    samples = extract_samples_long_(model,
        variable_name, 
        index_names)
    
    #convert data back into usable data types
    constructors = attr(model, "constructors")
    if (is.null(constructors)) constructors = list()
    for (column_name in c(variable_name, index_names)) {
        if (column_name %in% names(constructors)) {
            #we have a data type constructor for this index, convert it
            samples[[column_name]] = constructors[[column_name]](samples[[column_name]])
        }
    }
    
    #spread a column into wide format if requested
    if (is.null(samples$..)) {
        samples
    }
    else {
        #a column named ".." is present, use it to form a wide version of the data
        samples %>%
            mutate(.. = factor(.., labels=variable_name)) %>%
            spread_("..", variable_name)
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
extract_samples_long_ = function(model, variable_name, index_names) {
    ldply(colnames(model), function(colname) {
        #parse column into variable name and indices
        colname_parts = strsplit(colname,"\\,|\\]|\\[")[[1]]
        if (colname_parts[1] == variable_name) {	#this is the variable we want
            #get the values of the indices 
            indices = as.list(as.numeric(colname_parts[-1]))
            names(indices) = index_names
            #get the values of this variable in each sample
            values = list(model[,colname])
            names(values) = variable_name
            #put it all together
            data.frame(.sample=1:nrow(model), indices, values)
        }
    })
}
