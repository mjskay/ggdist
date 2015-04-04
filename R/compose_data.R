# Composing data
# 
# Author: mjskay
###############################################################################


#basic data list
data_list = function(...) {
    x = list(...)
    class(x) = c("data_list", "list")
    x
}
c.data_list = function(x, ..., recursive=FALSE) {
    class(x) = class(x)[-which(class(x) == "data_list")]
    x = c(x, ..., recursive=recursive)
    class(x) = c("data_list", class(x))
    x
}
print.data_list = function(x, ...) {
    cat("data_list:\n\n")
    class(x) = class(x)[-which(class(x) == "data_list")]
    print(x, ...) 
}

#conversion into data list from basic data types
as.data_list = function(object, name="", ...) UseMethod("as.data_list")
as.data_list.default = function(object, name="", ...) {
    warning(deparse(name), " has unsupported type ", deparse(class(object)), " and was dropped.")
    data_list()
}
as.data_list.numeric = function(object, name="", ...) {
    data = data_list(object)
    if (name == "") {	#name unspecified, warn
        warning("No name provided for value ", deparse(object, nlines=1))
    }
    names(data) = name
    data
}
as.data_list.logical = function(object, name="", ...) {
    as.data_list(as.numeric(object), name, ...)
}
as.data_list.factor = function(object, name="", .n_name=function(name) paste0("n_", name), ...) {
    data = as.data_list(as.numeric(object), name, .n_name=.n_name, ...)
    if (any(table(object) == 0)) {
        warning("Some levels of factor ", deparse(name), " are unused. This may cause issues if you are using it as an index in a model.")
    }
    data[[.n_name(name)]] = length(levels(object))
    data
}
as.data_list.list = function(object, name="", ...) {
    #go through list and translate variables
    data = data_list()
    for (i in 1:length(object)) {
        data = c(data, as.data_list(object[[i]], names(object)[[i]], ...))
    }
    data
}
as.data_list.data.frame = function(object, name="", .n_name=function(name) paste0("n_", name), ...) {
    #first, translate all variables in the data frame
    data = as.data_list.list(object, name, .n_name, ...)
    #then add "n" column and return final list
    n_name = if (name == "") "n" else .n_name(name)
    data[[n_name]] = nrow(object)
    data
}
as.data_list.data_list = function(object, name="", ...) {
    object
}

## Compose data into a list suitable to be passed into an MCMC sampler (JAGS, BUGS, etc).
##
## Translates each argument into list elements using as.data_list, and then concatenates
## all resulting list elements together.
## Translates a data.frame into a list suitable for use in an MCMC sampler. Does this as follows:
##
## Translates elements as follows:
##
## 		- numerics are included as-is
##		- logicals are translated into numeric using as.numeric
##		- factors are translated into numeric using as.numeric, and an additional
##		  column named .n_name(argument_name) is added with the number of levels in the factor.
##		- lists are translated by translating all elements of the list (recursively)
##		  and adding them to the result.
##		- data.frames are translated by translating every column of the data.frame and
##		  adding them to the result.
##		  A variable named "n" (or .n_name(argument_name) if the data.frame is passed as
##		  a named argument argument_name) is also added with the number of rows in the
##		  data frame.
##		- all other types are dropped (and a warning given)
##
## If you wish to add support for additional types not described above, provide an implementation
## of as.data_list for the type. See as.data_list.numeric, as.data_list.logical, etc for examples.
##
compose_data = function(..., .n_name=function(name) paste0("n_", name)) {
    #translate arguments into a data_list
    objects = list(...)
    if (is.null(names(objects))) {
        #when no named arguments are supplied, we must translate NULL into empty string
        #so that the name argument to as.data_list is still valid
        names(objects) = rep("", length(objects))
    }
    as.data_list(objects, .n_name)
}
