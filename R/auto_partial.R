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
#' recently-partially-applied non-`waiver` value) is used instead.
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
    is_required_arg = vapply(f_args, rlang::is_missing, FUN.VALUE = logical(1))
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
  is_required_arg = vapply(f_args, rlang::is_missing, FUN.VALUE = logical(1))
  required_arg_names = names(f_args)[is_required_arg]
  required_arg_names = required_arg_names[required_arg_names != "..."]

  # build an expression to apply waivers to optional args
  if (waivable) {
    optional_args = f_args[!is_required_arg]
    process_waivers = map2_(optional_args, names(optional_args), function(arg_expr, arg_name) {
      arg_sym = as.symbol(arg_name)
      expr(if (inherits(!!arg_sym, "waiver")) !!arg_sym = !!arg_expr)
    })
  } else {
    process_waivers = list()
  }

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

  if (length(required_arg_names) == 0) {
    partial_self_if_missing_args = list()
  } else {
    partial_self_if_missing_args = list(expr(
      if (!!any_required_args_missing) return((!!partial_self_f)(!!name, waivable = !!waivable))
    ))
  }

  new_f = new_function(
    f_args,
    expr({
      !!!process_waivers
      !!!partial_self_if_missing_args
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

#' @export
ggplot2::waiver
