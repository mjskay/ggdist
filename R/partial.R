# Partial function application support
#
# Author: mjskay
###############################################################################



#' Partial function application in ggdist
#'
#' @description
#'
#' Several \pkg{ggdist} functions support *partial application*: when called,
#' if their first (primary) argument is not provided, the function returns a
#' modified version of itself that uses the arguments passed to it as defaults.
#'
#' Functions supporting partial application include:
#'
#' - The [point_interval()] family, such as [median_qi()], [mean_qi()],
#'   [mode_hdi()], etc.
#'
#' - The *density* family, such as [density_unbounded()], [density_bounded()],
#'   etc.
#'
#' Partial application makes it easier to supply custom parameters to these
#' functions when using them inside other functions, such as geoms and stats.
#'
#' @examples
#'
#' set.seed(1234)
#' x = rbeta(1000, 4, 4)
#'
#' # point_interval() without a first argument results in partial application
#' pi_50 = point_interval(.width = .5)
#' pi_50
#'
#' # the partially-applied function remembers its new `.width` argument an
#' # can be applied to data
#' pi_50(x)
#'
#' # it can also be partially-applied again, modifying old argument values or
#' # setting new ones
#' pi_50_80_mean = pi_50(.width = c(.5, .8), .point = mean)
#' pi_50_80_mean
#'
#' pi_50_80_mean(x)
#'
#' # partially-applied functions can be useful to supply options to stats
#' # and geoms...
#' data.frame(x = x) %>%
#'   ggplot(aes(x)) +
#'   stat_halfeye(point_interval = point_interval(.width = .9))
#'
#' # ... although for some options (like .width) it is simpler to supply the
#' # option directly to the stat, which is passed down to point_interval:
#' data.frame(x = x) %>%
#'   ggplot(aes(x)) +
#'   stat_halfeye(.width = .9)
#'
#' @name partial-functions
NULL


#' Create a partially-applied version of the surrounding function
#'
#' Called from within a function, returns a modified version of the same
#' function with the arguments that were supplied replacing the defaults.
#' Can be called multiple times
#' @noRd
#' @importFrom rlang as_quosure enquos eval_tidy expr
partial_self = function(function_name = NULL) {
  f = sys.function(-1L)
  call = match.call(f, sys.call(-1L))
  f_quo = as_quosure(call[[1]], parent.frame(2L))
  default_args = lapply(call[-1], as_quosure, env = parent.frame(2L))
  function_name = function_name %||% deparse0(call[[1]])

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

#' Provide default arguments to a (possibly) partially-applied function
#'
#' @param f a function or a `"partial_function"`
#' @param ... parameters to set on the partially-applied function only
#'   if they have not already been partially-applied.
#' @noRd
merge_partial = function(f, ...) {
  args = force(list(...))
  if (inherits(f, "partial_function")) {
    args = args[setdiff(names(args), names(attr(f, "default_args")))]
    if (length(args) > 0) {
      do.call(f, args)
    } else{
      f
    }
  } else {
    function(...) {
      args = c(list(...), args)
      do.call(f, args)
    }
  }
}

#' @importFrom rlang get_expr
#' @export
print.ggdist_partial_function = function(x, ...) {
  function_sym = as.name(attr(x, "name"))
  args = lapply(attr(x, "default_args"), get_expr)

  cat(sep = "\n",
    "Partial function: ",
    paste0("  ", format(as.call(c(
      list(function_sym),
      args
    ))))
  )

  invisible(x)
}
