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
#' - The `density_` family, such as [density_auto()], [density_bounded()] and
#'   [density_unbounded()].
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
#' The `density` argument to [stat_slabinterval()] works similarly with the
#' `density_` family of functions.
#'
#' @examples
#' set.seed(1234)
#' x = rnorm(100)
#'
#' # the first required argument, `x`, of the density_ family is the vector
#' # to calculate a kernel density estimate from. If it is not provided, the
#' # function is partially applied and returned as-is
#' density_auto()
#'
#' # we could create a new function that uses half the default bandwidth
#' density_half_bw = density_auto(adjust = 0.5)
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
