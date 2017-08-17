# Composing data
#
# Author: mjskay
###############################################################################


#' A data_list for input into a Bayesian sampler
#'
#' Used by \code{\link{compose_data}} to create lists of data suitable for
#' input into a Bayesian sampler.  This typically should not be called directly
#' (instead use \code{\link{compose_data}}) unless you need to implement your
#' own conversion routines for a data type not already supported.
#'
#' This function creates a list with class \code{c("data_list", "list")}
#' instead of \code{c("list")}, but otherwise acts like the \code{\link{list}}
#' function.  It is used by \code{\link{as_data_list}} to create data lists for
#' input into Bayesian samplers. See the implementations of
#' \code{as_data_list.numeric}, \code{as_data_list.logical}, etc for examples.
#'
#' @param ...  Items to include in the list.
#' @return An object of class \code{c("data_list", "list")}
#' @author Matthew Kay
#' @seealso \code{\link{compose_data}}, \code{\link{spread_samples}}, \code{\link{gather_samples}}.
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
  x = c(x, ..., recursive = recursive)
  class(x) = c("data_list", class(x))
  x
}
#' @export
print.data_list = function(x, ...) {
  cat("data_list:\n\n")
  class(x) = class(x)[-which(class(x) == "data_list")]
  print(x, ...)
}


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
#' an implementation of \code{\link{as_data_list}} for the type. See the
#' implementations of \code{as_data_list.numeric}, \code{as_data_list.logical},
#' etc for examples.
#'
#' as_data_list.logical as_data_list.factor as_data_list.list
#' as_data_list.data.frame as_data_list.data_list
#' @param object The object to convert (see `Details`).
#' @param name The name of the element in the returned list corresponding to
#' this object.
#' @param scalar_as_array If \code{TRUE}, returns single scalars as an
#' 1-dimensional array with one element. This is used by
#' \code{as_data_list.data.frame} to ensure that columns from a data frame with
#' only one row are still returned as arrays instead of scalars.
#' @param .n_name A function that is used to form index variables (a variable
#' whose value is the length of a factor or a data frame in \code{...}). For
#' example, if a factor with \code{name = "foo"} (having three levels) is
#' passed in, the list returned will include an element named
#' \code{.n_name("foo")}, which by default would be "n_foo", containing the
#' value 3.
#' @param ...  Additional arguments passed to other implementations of
#' \code{as_data_list}.
#' @return An object of class \code{c("data_list", "list")}, where each element
#' is a translated variable as described above.
#' @author Matthew Kay
#' @seealso \code{\link{compose_data}}, \code{\link{spread_samples}}, \code{\link{gather_samples}}.
#' @keywords manip
#' @examples
#'
#' ##TODO
#'
#' @export
as_data_list = function(object, name = "", ...) UseMethod("as_data_list")
#' @rdname as_data_list
#' @export
as_data_list.default = function(object, name = "", ...) {
  warning(deparse0(name), " has unsupported type ", deparse0(class(object)), " and was dropped.")
  data_list()
}
#' @rdname as_data_list
#' @export
as_data_list.numeric = function(object, name = "",
  scalar_as_array = FALSE,  #treat single scalar values as array of length 1
  ...) {
  data = data_list(if (scalar_as_array) as.array(object) else object)
  if (name == "") {
    warning("No name provided for value ", deparse0(object))
  }
  names(data) = name
  data
}
#' @rdname as_data_list
#' @export
as_data_list.logical = function(object, name = "", ...) {
  as_data_list(as.numeric(object), name = name, ...)
}
#' @rdname as_data_list
#' @export
as_data_list.factor = function(object, name = "", .n_name = n_prefix("n"), ...) {
  data = as_data_list(as.numeric(object), name = name, .n_name = .n_name, ...)
  if (any(table(object) == 0)) {
    warning("Some levels of factor ", deparse0(name),
      " are unused. This may cause issues if you are using it as an index in a model.")
  }
  data[[.n_name(name)]] = length(levels(object))
  data
}
#' @rdname as_data_list
#' @export
as_data_list.character = function(object, name="", ...) {
  as_data_list(as.factor(object), name = name, ...)
}
#' @rdname as_data_list
#' @export
as_data_list.list = function(object, name="", ...) {
  #go through list and translate variables
  data = data_list()
  for (i in 1:length(object)) {
    data = c(data, as_data_list(object[[i]], name = names(object)[[i]], ...))
  }
  data
}
#' @rdname as_data_list
#' @export
as_data_list.data.frame = function(object, name="", .n_name = n_prefix("n"), ...) {
  #first, translate all variables in the data frame
  data = as_data_list.list(object,
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
#' @rdname as_data_list
#' @export
as_data_list.data_list = function(object, name="", ...) {
  object
}

#' Compose data for input into a Bayesian sampler
#'
#' Compose data into a list suitable to be passed into an MCMC sampler (JAGS,
#' BUGS, etc).
#'
#'
#' This function recursively translates each argument into list elements using
#' \code{\link{as_data_list}}, merging all resulting lists together. By
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
#'      \item \code{NULL} values are dropped. Setting a named argument to \code{NULL}
#'          can be used to drop that item from the resulting list (if an unwanted
#'          element was added to the list by a previous argument, such as a column
#'          from a data frame that is not needed in the model).
#'      \item all other types are dropped (and a warning given)
#' }
#'
#' As in functions like \code{\link{mutate}}, each expression is evaluated in an
#' environment containing the data list built up so far.
#'
#' For example, this means that if the first argument to \code{compose_data}
#' is a data frame, subsequent arguments can include direct references to columns
#' from that data frame. This allows you, for example, to easily use
#' \code{\link{x_at_y}} to generate indices for nested models.
#'
#' If you wish to add support for additional types not described above,
#' provide an implementation of \code{\link{as_data_list}} for the type. See
#' the implementations of \code{as_data_list.numeric},
#' \code{as_data_list.logical}, etc for examples.
#'
#' @param ...  Data to be composed into a list suitable for being passed into
#' Stan, JAGS, etc. Named arguments will have their name used as the \code{name}
#' argument to \code{as_data_list} when translated; unnamed arguments that are
#' not lists or data frames will have their bare value (passed through
#' \code{make.names}) used as the \code{name} argument to \code{as_data_list}.
#' Each argument is evaluated using \code{eval_tidy} in an environment that
#' includes all list items composed so far.
#' @param .n_name A function that is used to form index variables (a variable
#' whose value is number of levels in a factor or the length of a data frame in
#' \code{...}). For example, if a data frame with 20 rows and a factor \code{"foo"}
#' (having 3 levels) is passed to \code{compose_data}, the list returned by
#' \code{compose_data} will include an element named \code{.n_name("foo")}, which
#' by default would be "n_foo", containing the value 3, and a column named "n"
#' containing the value 20. See \code{\link{n_prefix}}.
#' @return A list where each element is a translated variable as described above.
#' @author Matthew Kay
#' @seealso \code{\link{as_data_list}}, \code{\link{spread_samples}},
#' \code{\link{gather_samples}}, \code{\link{x_at_y}}
#' @keywords manip
#' @examples
#'
#' library(magrittr)
#'
#' df = data.frame(
#'   plot = factor(paste0("p", rep(1:8, times = 2))),
#'   site = factor(paste0("s", rep(1:4, each = 2, times = 2)))
#' )
#'
#' df %>%
#'   compose_data()
#'
#' # turns site into a nested index: site[p] gives the site for plot p
#' df %>%
#'   compose_data(site = x_at_y(site, plot))
#'
#' @importFrom rlang quos eval_tidy
#' @export
compose_data = function(..., .n_name = n_prefix("n")) {
  #translate argument names / values into a list
  exprs = quos(...)
  given_names = names(exprs)
  given_names[is.null(given_names)] = ""
  forced_names = names(quos(..., .named = TRUE))

  #convert objects into a data list one by one, evaluating each argument in the
  #environment of the previous lists (to allow the user to refer to previously composed elements)
  data = list()
  for (i in 1:length(exprs)) {
    object_to_compose = eval_tidy(exprs[[i]], data)

    # lists and data frames don't use names unless they were provided explicitly
    name = if (is.list(object_to_compose)) given_names[[i]] else forced_names[[i]]

    if (is.null(object_to_compose)) {
      data[[name]] = NULL
    } else {
      data %<>% modifyList(as_data_list(object_to_compose, name = name, .n_name = .n_name))
    }
  }

  data
}


#' Prefix function generator for composing index columns
#'
#' Generates a function for generating names of index columns for factors in
#' \code{\link{compose_data}} by prefixing a character vector to the original
#' column name.
#'
#' @param prefix Character vector to be prepended to column names by
#' \code{\link{compose_data}} to create index columns. Typically something
#' like \code{"n"} (that is the default used in the \code{.n_name} argument
#' of \code{\link{compose_data}}).
#'
#' Returns a function. The function returned takes a character vector, \code{name}
#' and returns \code{paste0(prefix, "_", name)}, unless \code{name} is empty, in
#' which case it will return \code{prefix}.
#'
#' \code{n_prefix("n")} is the default method that \code{\link{compose_data}} uses to
#' generate column names for factor indices. Under this method, given a data frame
#' \code{df} with a factor column \code{"foo"} containing 5 levels, the results of
#' \code{compose_data(df)} will include an element named \code{"n"} (the result of
#' \code{n_prefix("n")("")}) equal to the number of rows in \code{df} and an element
#' named \code{"n_foo"} (the result of \code{n_prefix("n")("foo")}) equal to the
#' number of levels in \code{df$foo}.
#'
#' @seealso The \code{.n_name} argument of \code{\link{compose_data}}
#'
#' @export
n_prefix = function(prefix) {
  function(name) if (name == "") prefix else paste0(prefix, "_", name)
}
