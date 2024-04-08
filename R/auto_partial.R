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

#' @rdname auto_partial
#' @param f A function
#' @param name A character string giving the name of the function, to be used
#' when printing.
#' @param waivable logical: if `TRUE`, optional arguments that get
#' passed a [waiver()] will keep their default value (or whatever
#' non-`waiver` value has been most recently partially applied for that
#' argument).
#' @returns A modified version of `f` that will automatically be partially
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
#' @importFrom rlang new_function expr
auto_partial = function(f, name = NULL, waivable = TRUE) {
  f_body = body(f)
  # must ensure the function body is a { ... } block, not a single expression,
  # so we can splice it in later with !!!f_body
  if (!inherits(f_body, "{")) {
    f_body = expr({
      !!f_body
    })
  }
  f_args = formals(f)

  # find the required arguments
  is_required_arg = map_lgl_(f_args, rlang::is_missing)
  required_arg_names = names(f_args)[is_required_arg]
  required_arg_names = required_arg_names[required_arg_names != "..."]

  # build a logical expression testing to see if any required args are missing
  any_required_args_missing = Reduce(
    function(x, y) expr(!!x || !!y),
    lapply(required_arg_names, function(arg_name) expr(missing(!!as.symbol(arg_name))))
  )

  partial_self_f = if (identical(environment(f), environment(partial_self))) {
    # when auto_partial is called from within the ggdist namespace, don't inline
    # the partial self function
    quote(partial_self)
  } else {
    # when auto_partial is called from outside the ggdist namespace, we need to
    # inline the partial_self function so that it is guaranteed to be found
    partial_self
  }

  partial_self_if_missing_args = if (length(required_arg_names) > 0) {
    expr({
      if (!!any_required_args_missing) return((!!partial_self_f)(!!name, waivable = !!waivable))
    })
  }

  # build an expression to apply waivers to optional args
  process_waivers = if (waivable) {
    optional_args = f_args[!is_required_arg]
    map2_(optional_args, names(optional_args), function(arg_expr, arg_name) {
      arg_sym = as.symbol(arg_name)
      expr(if (inherits(!!arg_sym, "waiver")) assign(!!arg_name, !!arg_expr))
    })
  }

  new_f = new_function(
    f_args,
    expr({
      !!!partial_self_if_missing_args
      # no idea why, but covr::package_coverage() fails if the next line doesn't
      # have { } around it. It is not necessary for normal execution. Must have
      # something to do with how covr adds hooks for tracing execution.
      { !!!process_waivers }
      !!!f_body
    }),
    env = environment(f)
  )
  attr(new_f, "srcref") = attr(f, "srcref")

  new_f
}

#' @importFrom rlang get_expr
#' @export
print.ggdist_partial_function = function(x, ...) {
  f_sym = as.name(attr(x, "name"))
  f_args = lapply(attr(x, "provided_args"), get_expr)

  cat(sep = "\n",
    "<partial_function>: ",
    paste0("  ", format(as.call(c(
      list(f_sym),
      f_args
    ))))
  )

  invisible(x)
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
  dots = environment()$...
  if (missing(dots)) {
    new_promise_list()
  } else {
    new_promise_list(dots_to_list_(dots))
  }
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

#' Find a promise by name
#' @param name the name of a promise as a string or symbol
#' @param env the environment to search
#' @returns The promise with the given name from the given environment
#' @noRd
find_promise = find_var_

promise_expr = function(x) {
  do.call(substitute, list(x))
}

promise_env = function(x) {
  if (typeof(x) == "promise"){
    promise_env_(x)
  } else {
    NULL
  }
}

promise_peak_value = function(x) {
  eval(promise_expr(x), promise_env(x))
}


# auto_partial ------------------------------------------------------------

auto_partial_ = function(.f, ..., .name = NULL, .waivable = FALSE) {
  if (is.null(.name)) {
    f_expr = substitute(.f)
    if (is.symbol(f_expr)) {
      .name = as.character(f_expr)
    }
  }
  args = match_function_args(.f, promise_list(...))
  new_auto_partial(.f, args = args, name = .name, waivable = .waivable)
}

#' Construct an automatically partially-applied function
#'
#' @param f function to automatically partially-apply
#' @param args a named list of promises representing arguments, such as
#' returned by `promise_list()`
#' @param required_arg_names the names of required arguments in `f`
#' @param name the name of the function.
#' @param waivable whether or not arguments to the function are checked for
#' [waiver()]s.
#' @returns a function that when called will be partially applied if any of the
#' arguments in `required_arg_names` have not yet been supplied yet.
#' @noRd
new_auto_partial = function(
  f,
  args = promise_list(),
  required_arg_names = find_required_arg_names(f),
  name = NULL,
  waivable = FALSE
) {
  # we use these weird names to avoid clashing with argument names in f,
  # because partial_f will have a signature containing the same formals as f,
  # so if those formals include the names f, args, etc, things would break
  `>f` = f
  `>args` = args
  `>required_arg_names` = required_arg_names
  `>name` = name
  `>waivable` = waivable

  partial_f = function() {
    named_arg_promises = named_arg_promise_list()
    dot_arg_promises = dot_arg_promise_list()
    arg_promises = c(named_arg_promises, dot_arg_promises)
    new_args = match_function_args(sys.function(), arg_promises)
    if (`>waivable`) new_args = remove_waivers(new_args, `>required_arg_names`)
    `>args` = update_args(`>args`, new_args)

    if (all(`>required_arg_names` %in% names(`>args`))) {
      do.call(`>f`, `>args`)
    } else {
      new_auto_partial(`>f`, `>args`, `>required_arg_names`, `>name`, `>waivable`)
    }
  }
  partial_formals = formals(f)
  updated_formal_names = intersect(names(partial_formals), names(args))
  partial_formals[updated_formal_names] = lapply(args[updated_formal_names], promise_expr)
  formals(partial_f) = partial_formals

  attr(partial_f, "f") = f
  attr(partial_f, "args") = args
  attr(partial_f, "name") = name
  attr(partial_f, "waivable") = waivable
  class(partial_f) = "autopartial_function"
  partial_f
}

#' @export
print.autopartial_function = function(x, ..., width = getOption("width")) {
  cat0("<auto_partial", if (attr(x, "waivable")) " with waivers", ">:\n")

  name = attr(x, "name") %||% "."
  cat0(name, " = ")

  f = attr(x, "f")
  f_string = capture.output(print(f, width = width - 4, ...))
  cat(f_string, sep = "\n    ")

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

#' Retrieve a list of promises from named arguments in
#' the surrounding function
#' @returns A named list of promises for arguments to the
#' calling function
#' @noRd
named_arg_promise_list = function() {
  f = sys.function(sys.parent())
  call = match.call(f, sys.call(sys.parent()), envir = parent.frame(2L))
  arg_names = intersect(names(call[-1]), names(formals(f)))
  env = parent.frame()
  promises = lapply(arg_names, find_promise, env)
  names(promises) = arg_names
  new_promise_list(promises)
}

#' Retrieve a list of promises from `...` arguments in
#' the surrounding function
#' @returns A list of (possibly named) promises for arguments to the
#' calling function
#' @noRd
dot_arg_promise_list = function() {
  env = parent.frame()
  if (exists("...", env)) {
    evalq(promise_list(...), env)
  } else {
    new_promise_list()
  }
}

#' Remove waivers from an argument list
#' @param args a named list of promises
#' @param required_arg_names names of required arguments, which will not be
#' checked for waivers
#' @returns A modified version of `args` with any waived arguments (i.e.
#' promises for which [is_promise_waived()] returns `TRUE`) removed
#' @noRd
remove_waivers = function(args, required_arg_names) {
  keep = rep(TRUE, length(args))
  waivable = !(names(args) %in% required_arg_names)
  keep[waivable] = !vapply(args[waivable], is_promise_waived, logical(1))
  args[keep]
}

#' Is a promise a waiver?
#'
#' Returns `TRUE` if a promise is a simple expression referring to a
#' `waiver()`. Does not evaluate functions or other compound expressions,
#' but will evaluate symbols to check if they point to a `waiver()` object
#' @returns `TRUE` if `x` is a promise for the expression `waiver()` or
#' for a symbol pointing to a `waiver()`.
#' @noRd
is_promise_waived = function(x) {
  expr = promise_expr(x)
  identical(expr, quote(waiver())) ||
    (is.symbol(expr) && inherits(get0(expr, promise_env(x)), "waiver"))
}

cat0 = function(...) {
  cat(..., sep = "")
}
