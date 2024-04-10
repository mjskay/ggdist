# Automatic partial function application
#
# Author: mjskay
###############################################################################



#' Automatic partial function application in ggdist
#'
#' @description
#'
#' Several \pkg{ggdist} functions support *automatic partial application*: when called,
#' if all of their required arguments have not been provided, the function returns a
#' modified version of itself that uses the arguments passed to it so far as defaults.
#' Technically speaking, these functions are essentially "Curried" with respect to
#' their required arguments, but I think "automatic partial application" gets
#' the idea across more clearly.
#'
#' Functions supporting automatic partial application include:
#'
#' - The [point_interval()] family, such as [median_qi()], [mean_qi()],
#'   [mode_hdi()], etc.
#'
#' - The `smooth_` family, such as [smooth_bounded()], [smooth_unbounded()],
#'   [smooth_discrete()], and [smooth_bar()].
#'
#' - The `density_` family, such as [density_bounded()], [density_unbounded()] and
#'   [density_histogram()].
#'
#' - The [align] family.
#'
#' - The [breaks] family.
#'
#' - The [bandwidth] family.
#'
#' - The [blur] family.
#'
#' Partial application makes it easier to supply custom parameters to these
#' functions when using them inside other functions, such as geoms and stats.
#' For example, smoothers for [geom_dots()] can be supplied in one of three
#' ways:
#'
#' - as a suffix: `geom_dots(smooth = "bounded")`
#' - as a function: `geom_dots(smooth = smooth_bounded)`
#' - as a partially-applied function with options:
#'   `geom_dots(smooth = smooth_bounded(kernel = "cosine"))`
#'
#' Many other common arguments for \pkg{ggdist} functions work similarly; e.g.
#' `density`, `align`, `breaks`, `bandwidth`, and `point_interval` arguments.
#'
#' These function families (except [point_interval()]) also support passing
#' [waiver]s to their optional arguments: if [waiver()] is passed to any
#' of these arguments, their default value (or the most
#' recently-partially-applied non-[waiver] value) is used instead.
#'
#' Use the [auto_partial()] function to create new functions that support
#' automatic partial application.
#'
#' @examples
#' set.seed(1234)
#' x = rnorm(100)
#'
#' # the first required argument, `x`, of the density_ family is the vector
#' # to calculate a kernel density estimate from. If it is not provided, the
#' # function is partially applied and returned as-is
#' density_unbounded()
#'
#' # we could create a new function that uses half the default bandwidth
#' density_half_bw = density_unbounded(adjust = 0.5)
#' density_half_bw
#'
#' # we can overwrite partially-applied arguments
#' density_quarter_bw_trimmed = density_half_bw(adjust = 0.25, trim = TRUE)
#' density_quarter_bw_trimmed
#'
#' # when we eventually call the function and provide the required argument
#' # `x`, it is applied using the arguments we have "saved up" so far
#' density_quarter_bw_trimmed(x)
#'
#' @name auto_partial
#' @aliases automatic-partial-functions
NULL


#' Create a partially-applied version of the surrounding function
#'
#' Called from within a function, returns a modified version of the same
#' function with the arguments that were supplied replacing the defaults.
#' Can be called multiple times
#' @noRd
#' @importFrom rlang as_quosure enquos0 eval_tidy expr get_expr
partial_self = function(name = NULL, waivable = TRUE) {
  f = sys.function(-1L)
  call_expr = match.call(f, sys.call(-1L), TRUE, parent.frame(2L))
  f_quo = as_quosure(call_expr[[1]], parent.frame(2L))
  provided_args = lapply(call_expr[-1], as_quosure, env = parent.frame(2L))
  name = name %||% deparse0(get_expr(call_expr[[1]]))

  waivable_arg_names = if (waivable) {
    f_args = formals(f)
    is_required_arg = map_lgl_(f_args, rlang::is_missing)
    names(f_args)[!is_required_arg]
  }

  partial_f = function(...) {
    new_args = enquos0(...)
    if (waivable) {
      is_waivable = rlang::names2(new_args) %in% waivable_arg_names
      is_waived = is_waivable
      is_waived[is_waivable] = map_lgl_(new_args[is_waivable], function(arg_expr) {
        inherits(eval_tidy(arg_expr), "waiver")
      })
      new_args = new_args[!is_waived]
    }
    all_args = defaults(new_args, provided_args)
    eval_tidy(expr((!!f_quo)(!!!all_args)))
  }

  attr(partial_f, "provided_args") = provided_args
  attr(partial_f, "name") = name
  class(partial_f) = c("ggdist_partial_function", "function")
  partial_f
}


# waiver ------------------------------------------------------------------

#' A waived argument
#'
#' A flag indicating that the default value of an argument should be used.
#'
#' @details
#' A [waiver()] is a flag passed to a function argument that indicates the
#' function should use the default value of that argument. It is used in two
#' cases:
#'
#' - \pkg{ggplot2} functions use it to distinguish between "nothing" (`NULL`)
#'    and a default value calculated elsewhere ([waiver()]).
#'
#' - \pkg{ggdist} turns \pkg{ggplot2}'s convention into a standardized method of
#'    argument-passing: any named argument with a default value in an
#'    [automatically partially-applied function][auto_partial] can be passed
#'    [waiver()] when calling the function. This will cause the default value
#'    (or the most recently partially-applied value) of that argument to be used
#'    instead.
#'
#'    **Note:** due to historical limitations, [waiver()] cannot currently be
#'    used on arguments to the [point_interval()] family of functions.
#'
#' @seealso [auto_partial()], [ggplot2::waiver()]
#' @examples
#' f = auto_partial(function(x, y = "b") {
#'   c(x = x, y = y)
#' })
#'
#' f("a")
#'
#' # uses the default value of `y` ("b")
#' f("a", y = waiver())
#'
#' # partially apply `f`
#' g = f(y = "c")
#' g
#'
#' # uses the last partially-applied value of `y` ("c")
#' g("a", y = waiver())
#' @importFrom ggplot2 waiver
#' @export
waiver = ggplot2::waiver

#' waiver-coalescing operator
#' @noRd
`%|W|%` = function(x, y) {
  if (inherits(x, "waiver")) y
  else x
}

is_waiver = function(x) {
  if (typeof(x) == "promise") {
    expr = promise_expr(x)
    identical(expr, quote(waiver())) ||
      (is.symbol(expr) && is_waiver(get0(as.character(expr), promise_env(x))))
  } else {
    inherits(x, "waiver")
  }
}


# promise lists -----------------------------------------------------------------

new_promise_list = function(x = list()) {
  class(x) = "autopartial_promise_list"
  x
}

#' construct a list of promises
#' @param ... unevaluated expressions, possibly with names
#' @returns a list of promises
#' @noRd
promise_list = function(...) {
  dot_arg_promise_list()
}

#' @export
`[.autopartial_promise_list` = function(x, ...) {
  new_promise_list(NextMethod())
}

#' @export
print.autopartial_promise_list = function(x, ...) {
  cat0("<promise_list>:\n")
  print(lapply(x, format_promise), ...)
  invisible(x)
}

#' @export
format.autopartial_promise_list = function(x, ...) {
  vapply(x, format_promise, character(1))
}

#' @export
print.autopartial_formatted_promise = function(x, ...) {
  cat0(x, "\n")
  invisible(x)
}

format_promise = function(x) {
  expr = promise_expr(x)
  env = promise_env(x)
  structure(
    paste0("<promise: ", deparse0(expr), ", ", format(env), ">"),
    class = c("autopartial_formatted_promise", "character")
  )
}


# promises ----------------------------------------------------------------

#' Find a promise by name.
#' @param name the name of a promise as a string or symbol
#' @param env the environment to search
#' @returns One of:
#' - If `name` refers to a promise, the promise with the given name from the
#' given environment. Promises whose code is itself a promise (possibly
#' recursively) are unwrapped so that the code referred to by the returned
#' promise is not also a promise.
#' - If `name` does not refer to a promise, it is returned as a normal object.
#' @noRd
find_promise = find_promise_

promise_expr = function(x) {
  if (typeof(x) == "promise") {
    do.call(substitute, list(unwrap_promise_(x)))
  } else {
    x
  }
}

promise_env = function(x) {
  if (typeof(x) == "promise") {
    promise_env_(x)
  } else {
    NULL
  }
}


# auto_partial ------------------------------------------------------------

#' @rdname auto_partial
#' @param .f A function
#' @param ... arguments to be partially applied to `.f`
#' @returns A modified version of `.f` that will automatically be partially
#' applied if all of its required arguments are not given.
#' @examples
#' # create a custom automatically partially applied function
#' f = auto_partial(function(x, y, z = 3) (x + y) * z)
#' f()
#' f(1)
#' g = f(y = 2)(z = 4)
#' g
#' g(1)
#'
#' # pass waiver() to optional arguments to use existing values
#' f(z = waiver())(1, 2)  # uses default z = 3
#' f(z = 4)(z = waiver())(1, 2)  # uses z = 4
#' @export
auto_partial = function(.f, ...) {
  f_expr = substitute(.f)
  name = if (is.symbol(f_expr)) as.character(f_expr)
  args = match_function_args(.f, promise_list(...))
  new_auto_partial(.f, args = args, name = name)
}

#' Partial function application
#'
#' Partially apply a function once.
#' @param .f a function
#' @param ... arguments to be partially applied to `.f`
#' @noRd
partial_ = function(.f, ...) {
  f_expr = substitute(.f)
  name = if (is.symbol(f_expr)) as.character(f_expr)
  args = match_function_args(.f, promise_list(...))
  new_auto_partial(.f, args = args, name = name, required_arg_names = character())
}

#' Low-level constructor for automatically partially-applied functions
#'
#' Construct a version of the function `f` that is partially applied when called
#' unless all required arguments have been supplied.
#' @param f function to automatically partially-apply
#' @param args a named list of promises representing arguments, such as
#' returned by `promise_list()`.
#' @param required_arg_names character vector of the names of required arguments
#' in `f`. When all of these have been supplied, the function will be evaluated.
#' The default, `find_required_arg_names(f)`, considers all arguments without a
#' default value in the function definition to be required. Pass `NULL` or
#' `character()` to get tradition (non-automatic) partial application.
#' @param name the name of the function as a string. Used for printing purposes
#' only.
#' @param waivable if `TRUE`, if you pass `waiver()` to an argument to this
#' function, whatever value that argument already has will be used instead.
#' @returns a function that when called will be partially applied until all of
#' the arguments in `required_arg_names` have been supplied.
#' @noRd
new_auto_partial = function(
  f,
  args = promise_list(),
  required_arg_names = find_required_arg_names(f),
  name = NULL,
  waivable = TRUE
) {
  # we use these weird names to avoid clashing with argument names in f,
  # because partial_f will have a signature containing the same formals as f,
  # so if those formals include the names f, args, etc, things would break
  `>f` = f
  `>args` = args
  `>required_arg_names` = required_arg_names
  `>name` = name %||% "."
  `>waivable` = waivable

  partial_f = function() {
    new_args = arg_promise_list()
    if (`>waivable`) new_args = remove_waivers(new_args)
    `>args` = update_args(`>args`, new_args)

    if (all(`>required_arg_names` %in% names(`>args`))) {
      # a simpler version of the below would be something like:
      # > do.call(`>f`, args, envir = parent.frame())
      # however this would lead to the function call appearing as the
      # full function body in things like match.call(). So instead
      # we construct a calling environment in which the function is
      # defined under a readable name.
      call_env = new.env(parent = parent.frame())
      call_env[[`>name`]] = `>f`
      do.call(`>name`, `>args`, envir = call_env)
    } else {
      new_auto_partial(`>f`, `>args`, `>required_arg_names`, `>name`, `>waivable`)
    }
  }
  partial_formals = formals(f)
  # update expressions in formals to match provided args
  updated_formal_names = intersect(names(partial_formals), names(args))
  partial_formals[updated_formal_names] = lapply(args[updated_formal_names], promise_expr)
  # move any required args that have been applied to the end. this allows
  # f(1)(2)(3)... to be equivalent to f(1, 2, 3, ...) if positions 1, 2, 3, ...
  # correspond to required arguments.
  is_updated_required = names(partial_formals) %in% intersect(updated_formal_names, required_arg_names)
  partial_formals = c(partial_formals[!is_updated_required], partial_formals[is_updated_required])
  formals(partial_f) = partial_formals

  attr(partial_f, "f") = f
  attr(partial_f, "args") = args
  attr(partial_f, "name") = name
  attr(partial_f, "waivable") = waivable
  class(partial_f) = c("autopartial_function", "function")
  partial_f
}

#' @export
print.autopartial_function = function(x, ..., width = getOption("width")) {
  cat0("<auto_partial", if (attr(x, "waivable")) " with waivers", ">:\n")

  name = attr(x, "name") %||% "."
  cat0(name, " = ")

  f = attr(x, "f")
  f_string = utils::capture.output(print(f, width = width - 2, ...))
  cat(f_string, sep = "\n  ")

  cat0(format(as.call(c(
    list(as.name(name)),
    lapply(attr(x, "args"), promise_expr)
  ))))

  invisible(x)
}

#' Given a function and a list of arguments, return a modified version of the
#' argument list where named arguments have been matched according to R's
#' argument-matching rules.
#' @param f a function
#' @param args a list of arguments, such as returned by [promise_list()].
#' Should not contain a `...` argument.
#' @returns a standardized list of arguments (i.e. where all arguments with
#' names are named and in order) that can be supplied to `f`, or an error
#' if `args` is not a valid argument list for `f`.
#' @noRd
match_function_args = function(f, args) {
  # use match.call to figure out the names of arguments and the
  # argument order
  args_i = seq_along(args)
  names(args_i) = names(args)
  call =  as.call(c(list(quote(f)), args_i))
  args_i_call = match.call(f, call)[-1]
  args_i = as.integer(as.list(args_i_call))

  # fill in names and re-order the args
  names(args)[args_i] = names(args_i_call)
  args[args_i]
}

#' A variation on match.call that uses code from promises
#' @noRd
match_call_with_promises = function(which = sys.parent()) {
  arg_promises = arg_promise_list(which)
  arg_exprs = lapply(arg_promises, promise_expr)
  f_expr = sys.call(which)[[1]]
  as.call(c(list(f_expr), arg_exprs))
}

#' Update a list of arguments, overwriting named arguments in `old_args`
#' with values that appear in `new_args`. Positional arguments from both
#' argument lists are kept in the same order as they appear.
#' @param old_args a list of arguments
#' @param new_args a list of arguments
#' @returns a list of arguments
#' @noRd
update_args = function(old_args, new_args) {
  if (is.null(names(old_args))) names(old_args) = rep("", length(old_args))
  if (is.null(names(new_args))) names(new_args) = rep("", length(new_args))

  old_names = names(old_args)
  old_names = old_names[nzchar(old_names)]
  new_names = names(new_args)
  updated_names = intersect(old_names, new_names)
  old_args[updated_names] = new_args[updated_names]

  c(old_args, new_args[!names(new_args) %in% updated_names])
}

#' Return the names of required arguments for function `f`
#' @param f A function
#' @returns character vector of argument names
#' @noRd
find_required_arg_names = function(f) {
  args = formals(f)
  is_missing = vapply(args, is_missing_arg, logical(1))
  setdiff(names(args)[is_missing], "...")
}

#' Is `x` a missing argument?
#' @param an argument
#' @returns `TRUE` if `x` represents a missing argument
#' @noRd
is_missing_arg = function(x) {
  missing(x) || identical(x, quote(expr = ))
}

#' Retrieve a list of promises from arguments in
#' the surrounding function
#' @param which the frame number to get call information from.
#' @returns A list of promises for arguments to the
#' calling function
#' @noRd
arg_promise_list = function(which = sys.parent()) {
  named_arg_promises = named_arg_promise_list(which)
  dot_arg_promises = dot_arg_promise_list(which)
  arg_promises = c(named_arg_promises, dot_arg_promises)
  # TODO: this line might be redundant / maybe just return arg_promises
  new_promise_list(match_function_args(sys.function(which), arg_promises))
}

#' Retrieve a list of promises from named arguments in
#' the surrounding function
#' @param which the frame number to get call information from.
#' @returns A named list of promises for arguments to the
#' calling function
#' @noRd
named_arg_promise_list = function(which = sys.parent()) {
  f = sys.function(which)
  call = match.call(f, sys.call(which), envir = sys.frame(which - 1L))
  arg_names = intersect(names(call[-1]), names(formals(f)))
  env = sys.frame(which)
  promises = lapply(arg_names, find_promise, env)
  names(promises) = arg_names
  new_promise_list(promises)
}

#' Retrieve a list of promises from `...` arguments in
#' the surrounding function
#' @param which the frame number to get call information from.
#' @returns A list of (possibly named) promises for arguments to the
#' calling function
#' @noRd
dot_arg_promise_list = function(which = sys.parent()) {
  env = sys.frame(which)
  dots = env$...
  if (missing(dots)) {
    new_promise_list()
  } else {
    new_promise_list(dots_to_list_(dots))
  }
}

#' Remove waivers from an argument list
#' @param args a named list of promises
#' @returns A modified version of `args` with any waived arguments (i.e.
#' promises for which [is_waiver()] returns `TRUE`) removed
#' @noRd
remove_waivers = function(args) {
  waived = vapply(args, is_waiver, logical(1))
  args[!waived]
}

cat0 = function(...) {
  cat(..., sep = "")
}
