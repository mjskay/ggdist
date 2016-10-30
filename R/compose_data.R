# Composing data
# 
# Author: mjskay
###############################################################################

# naming function
#' @export
n_prefix = function(prefix) function(name) if(name == "") prefix else paste0(prefix, "_", name)

#basic data list


#' A data_list for input into a Bayesian sampler
#' 
#' Used by \code{\link{compose_data}} to create lists of data suitable for
#' input into a Bayesian sampler.  This typically should not be called directly
#' (instead use \code{\link{compose_data}}) unless you need to implement your
#' own conversion routines for a data type not already supported.
#' 
#' This function creates a list with class \code{c("data_list", "list")}
#' instead of \code{c("list")}, but otherwise acts like the \code{\link{list}}
#' function.  It is used by \code{\link{as.data_list}} to create data lists for
#' input into Bayesian samplers. See the implementations of
#' \code{as.data_list.numeric}, \code{as.data_list.logical}, etc for examples.
#' 
#' @param ...  Items to include in the list.
#' @return An object of class \code{c("data_list", "list")}
#' @author Matthew Kay
#' @seealso \code{\link{compose_data}}, \code{\link{extract_samples}}.
#' @keywords manip
#' @export
data_list = function(...) {
    x = list(...)
    class(x) = c("data_list", "list")
    x
}
#' @export
c.data_list = function(x, ..., recursive=FALSE) {
    class(x) = class(x)[-which(class(x) == "data_list")]
    x = c(x, ..., recursive=recursive)
    class(x) = c("data_list", class(x))
    x
}
#' @export
print.data_list = function(x, ...) {
    cat("data_list:\n\n")
    class(x) = class(x)[-which(class(x) == "data_list")]
    print(x, ...) 
}

#conversion into data list from basic data types


#' Convert data into a data_list
#' 
#' Used by \code{\link{compose_data}} to create lists of data suitable for
#' input into a Bayesian sampler.  This typically should not be called directly
#' (instead use \code{\link{compose_data}}) unless you need to implement your
#' own conversion routines for a data type not already supported (see
#' `Details`).
#' 
#' This function recursively translates its first argument into list elements,
#' concatenating all resulting lists together. By default this means that:
#' \itemize{ \item numerics are included as-is.  \item logicals are translated
#' into numeric using \code{\link{as.numeric}}.  \item factors are translated
#' into numeric using \code{\link{as.numeric}}, and an additional element named
#' \code{.n_name(name)} is added with the number of levels in the factor.
#' \item lists are translated by translating all elements of the list
#' (recursively) and adding them to the result.  \item data.frames are
#' translated by translating every column of the data.frame and adding them to
#' the result.  A variable named \code{"n"} (or \code{.n_name(name)} if
#' \code{name} is not \code{""}) is also added containing the number of rows in
#' the data frame.  \item all other types are dropped (and a warning given) }
#' If you wish to add support for additional types not described above, provide
#' an implementation of \code{\link{as.data_list}} for the type. See the
#' implementations of \code{as.data_list.numeric}, \code{as.data_list.logical},
#' etc for examples.
#' 
#' as.data_list.logical as.data_list.factor as.data_list.list
#' as.data_list.data.frame as.data_list.data_list
#' @param object The object to convert (see `Details`).
#' @param name The name of the element in the returned list corresponding to
#' this object.
#' @param scalar_as_array If \code{TRUE}, returns single scalars as an
#' 1-dimensional array with one element. This is used by
#' \code{as.data_list.data.frame} to ensure that columns from a data frame with
#' only one row are still returned as arrays instead of scalars.
#' @param .n_name A function that is used to form index variables (a variable
#' whose value is the length of a factor or a data frame in \code{...}). For
#' example, if a factor with \code{name = "foo"} (having three levels) is
#' passed in, the list returned will include an element named
#' \code{.n_name("foo")}, which by default would be "n_foo", containing the
#' value 3.
#' @param ...  Additional arguments passed to other implementations of
#' \code{as.data_list}.
#' @return An object of class \code{c("data_list", "list")}, where each element
#' is a translated variable as described above.
#' @author Matthew Kay
#' @seealso \code{\link{compose_data}}, \code{\link{extract_samples}}.
#' @keywords manip
#' @examples
#' 
#' ##TODO
#' 
#' @export
as.data_list = function(object, name="", ...) UseMethod("as.data_list")
#' @rdname as.data_list
#' @export
as.data_list.default = function(object, name="", ...) {
    warning(deparse(name), " has unsupported type ", deparse(class(object)), " and was dropped.")
    data_list()
}
#' @rdname as.data_list
#' @export
as.data_list.numeric = function(object, name="", 
        scalar_as_array=FALSE,  #treat single scalar values as array of length 1 
        ...) {
    data = data_list(if (scalar_as_array) as.array(object) else object)
    if (name == "") {	#name unspecified, warn
        warning("No name provided for value ", deparse(object, nlines=1))
    }
    names(data) = name
    data
}
#' @rdname as.data_list
#' @export
as.data_list.logical = function(object, name="", ...) {
    as.data_list(as.numeric(object), name, ...)
}
#' @rdname as.data_list
#' @export
as.data_list.factor = function(object, name="", .n_name = n_prefix("n"), ...) {
    data = as.data_list(as.numeric(object), name = name, .n_name = .n_name, ...)
    if (any(table(object) == 0)) {
        warning("Some levels of factor ", deparse(name), " are unused. This may cause issues if you are using it as an index in a model.")
    }
    data[[.n_name(name)]] = length(levels(object))
    data
}
#' @rdname as.data_list
#' @export
as.data_list.list = function(object, name="", ...) {
    #go through list and translate variables
    data = data_list()
    for (i in 1:length(object)) {
        data = c(data, as.data_list(object[[i]], names(object)[[i]], ...))
    }
    data
}
#' @rdname as.data_list
#' @export
as.data_list.data.frame = function(object, name="", .n_name = n_prefix("n"), ...) {
    #first, translate all variables in the data frame
    data = as.data_list.list(object, 
        name = name,
        .n_name = .n_name, 
        scalar_as_array = TRUE,     #when converting from a data frame with only one row, convert 
                                    #single scalars to arrays of length 1
        ...)
    #then add "n" column and return final list
    n_name = .n_name(name)
    data[[n_name]] = nrow(object)
    data
}
#' @rdname as.data_list
#' @export
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


#' Compose data for input into a Bayesian sampler
#' 
#' Compose data into a list suitable to be passed into an MCMC sampler (JAGS,
#' BUGS, etc).
#' 
#' 
#' This function recursively translates each argument into list elements using
#' \code{\link{as.data_list}}, concatenating all resulting lists together. By
#' default this means that: 
#' \itemize{
#'      \item numerics are included as-is.
#'      \item logicals are translated into numeric using \code{\link{as.numeric}}.
#'      \item factors are translated into numeric using \code{\link{as.numeric}}, 
#'          and an additional element named \code{.n_name(argument_name)} is added 
#'          with the number of levels in the factor. The default \code{.n_name}
#'          function prefixes \code{"n_"} before the factor name; e.g. a factor
#'          named \code{foo} will have an element named \code{n_foo} added containing
#'          the number of levels in \code{foo}.
#'      \item lists are translated by translating all elements of the list
#'          (recursively) and adding them to the result.
#'      \item data.frames are translated by translating every column of the data.frame
#'          and adding them to the result.  A variable named \code{"n"} (or
#'          \code{.n_name(argument_name)} if the data.frame is passed as a named
#'          argument \code{argument_name}) is also added containing the number of rows
#'          in the data frame.
#'      \item all other types are dropped (and a warning given)
#' }
#' 
#' If you wish to add support for additional types not described above,
#' provide an implementation of \code{\link{as.data_list}} for the type. See
#' the implementations of \code{as.data_list.numeric},
#' \code{as.data_list.logical}, etc for examples.
#' 
#' @param ...  Data to be composed into a list suitable for being passed into
#' Stan, JAGS, etc. Named arguments will have their name used as the \code{name}
#' argument to \code{as.data_list} when translated; unnamed arguments that are
#' not lists or data frames will have their bare value (passed through
#' \code{make.names}) used as the \code{name} argument to \code{as.data_list}.
#' @param .n_name A function that is used to form index variables (a variable
#' whose value is number of levels in a factor or the length of a data frame in
#' \code{...}). For example, if a data frame with 20 rows and a factor \code{"foo"}
#' (having 3 levels) is passed to \code{compose_data}, the list returned by
#' \code{compose_data} will include an element named \code{.n_name("foo")}, which
#' by default would be "n_foo", containing the value 3, and a column named "n"
#' containing the value 20.
#' @return An object of class \code{c("data_list", "list")}, where each element
#' is a translated variable as described above.
#' @author Matthew Kay
#' @seealso \code{\link{as.data_list}}, \code{\link{extract_samples}}.
#' @keywords manip
#' @examples
#' 
#' ##TODO
#' 
#' @export
compose_data = function(..., .n_name = n_prefix("n")) {
    #translate argument names / values into a list
    objects = list(...)
    if (is.null(names(objects))) {
        #when no named arguments are supplied, translate NULL into empty string
        #so that the naming code below works
        names(objects) = rep("", length(objects))
    }
    #give a name to any unnamed argument based on its unevaluated value
    names_from_arg_values = make.names(sapply(as.list(substitute(list(...)))[-1], deparse))
    unnamed_indices = which(names(objects) == "" & !sapply(objects, is.list))
    names(objects)[unnamed_indices] = names_from_arg_values[unnamed_indices]
    #convert into data list 
    data = as.data_list(objects, .n_name = .n_name)
    #as a hack for now, we strip the "data_list" type in the end for compatibility
    #with runjags, which incorrectly checks for class(data) == "list" instead of
    #using is.list
    class(data) = "list"
    data
}
