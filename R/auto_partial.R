# Automatic partial function application
#
# Author: mjskay
###############################################################################



#' Automatic partial function application in ggdist
#'
#' @description
#'
#' Several \pkg{ggdist} functions support *automatic partial application*: when called,
#' if their first (primary) argument is not provided, the function returns a
#' modified version of itself that uses the arguments passed to it as defaults.
#'
#' Functions supporting partial application include:
#'
#' - The [point_interval()] family, such as [median_qi()], [mean_qi()],
#'   [mode_hdi()], etc.
#'
#' - The *smooth* family, such as [smooth_density()], [smooth_discrete()],
#'   [smooth_bar()].
#'
#' Partial application makes it easier to supply custom parameters to these
#' functions when using them inside other functions, such as geoms and stats.
#'
#' @examples
#' # TODO
#'
#' @name automatic-partial-functions
NULL


#' Create a partially-applied version of the surrounding function
#'
#' Called from within a function, returns a modified version of the same
#' function with the arguments that were supplied replacing the defaults.
#' Can be called multiple times
#' @noRd
#' @importFrom rlang as_quosure enquos eval_tidy expr get_expr
partial_self = function(function_name = NULL) {
  f = sys.function(-1L)
  call = match.call(f, sys.call(-1L))
  f_quo = as_quosure(call[[1]], parent.frame(2L))
  default_args = lapply(call[-1], as_quosure, env = parent.frame(2L))
  function_name = function_name %||% deparse0(get_expr(call[[1]]))

  partial_f = function(...) {
    new_args = enquos(...)
    args = defaults(new_args, default_args)
    eval_tidy(expr((!!f_quo)(!!!args)))
  }

  attr(partial_f, "default_args") = default_args
  attr(partial_f, "name") = function_name
  class(partial_f) = c("ggdist_partial_function", "function")
  partial_f
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
