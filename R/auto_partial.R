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
#' @importFrom rlang as_quosure enquos eval_tidy expr get_expr
partial_self = function(name = NULL) {
  f = sys.function(-1L)
  call = match.call(f, sys.call(-1L))
  f_quo = as_quosure(call[[1]], parent.frame(2L))
  default_args = lapply(call[-1], as_quosure, env = parent.frame(2L))
  name = name %||% deparse0(get_expr(call[[1]]))

  partial_f = function(...) {
    new_args = enquos(...)
    args = defaults(new_args, default_args)
    eval_tidy(expr((!!f_quo)(!!!args)))
  }

  attr(partial_f, "default_args") = default_args
  attr(partial_f, "name") = name
  class(partial_f) = c("ggdist_partial_function", "function")
  partial_f
}

#' @rdname auto_partial
#' @param f A function
#' @param name A character string giving the name of the function, to be used
#' when printing.
#' @returns A modified version of `f` that will automatically be partially
#' applied if all of its required arguments are not given.
#' @export
#' @importFrom rlang new_function expr
#' @examples
#' # create a custom automatically partially applied function
#' f = auto_partial(function(x, y, z = 3) (x + y) * z)
#' f()
#' f(1)
#' g = f(y = 2)(z = 4)
#' g
#' g(1)
auto_partial = function(f, name = NULL) {
  f_body = body(f)
  # must ensure the function body is a { ... } block, not a single expression,
  # so we can splice it in later with !!!f_body
  if (!inherits(f_body, "{")) f_body = expr({ !!f_body })
  args = formals(f)

  # find the required arguments
  required_args = args[vapply(args, rlang::is_missing, FUN.VALUE = logical(1))]
  required_arg_names = names(required_args)
  required_arg_names = required_arg_names[required_arg_names != "..."]

  # no required arguments => function will always fully evaluate when called
  if (length(required_arg_names) == 0) return(f)

  # build a logical expression testing to see if any required args are missing
  any_required_args_missing = Reduce(
    function(x, y) expr(!!x || !!y),
    lapply(required_arg_names, function(arg_name) expr(missing(!!as.name(arg_name))))
  )

  partial_self_f = if (identical(environment(f), environment(partial_self))) {
    # when auto_partial is called from within the ggdist namespace, don't inline
    # the partial self function
    quote(partial_self)
  } else {
    # when auto_partial is called from within the ggdist namespace, we need to
    # inline the partial_self function so that it is guaranteed to be found
    partial_self
  }

  new_f = new_function(
    args,
    expr({
      if (!!any_required_args_missing) return((!!partial_self_f)(!!name))

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
  function_sym = as.name(attr(x, "name"))
  args = lapply(attr(x, "default_args"), get_expr)

  cat(sep = "\n",
    "<partial_function>: ",
    paste0("  ", format(as.call(c(
      list(function_sym),
      args
    ))))
  )

  invisible(x)
}
