# Helper methods for working with distributions
#
# Author: mjskay
###############################################################################



# pdf, cdf, and quantile functions ----------------------------------------

#' Helper function to create pdf/cdf/quantile functions
#' @param dist The distribution, either as a string (given the suffix to a
#' p/q/d/r function set), \pkg{distributional} object, or `rvar()`
#' @param prefix For strings, one of `"p"`, `"q"`, `"d"`, or `"r"`
#' @param fun For \pkg{distributional} objects and `rvar`s, the function to apply (e.g.
#' `pdf()`, `cdf()`, `quantile()`, or `generate()`).
#' @noRd
distr_function = function(dist, prefix, fun) {
  UseMethod("distr_function")
}
#' @export
distr_function.default = function(dist, prefix, fun) {
  stop0("The `dist` aesthetic does not support objects of type ", deparse0(class(dist)))
}
#' @export
distr_function.character = function(dist, prefix, fun) {
  match.fun(paste0(prefix, dist))
}
#' @export
distr_function.factor = function(dist, prefix, fun) {
  distr_function(as.character(dist), prefix, fun)
}
#' @export
distr_function.distribution = function(dist, prefix, fun) {
  if (length(dist) > 1) stop(
    "distributional objects should never have length > 1 here.\n",
    "Please report this bug at https://github.com/mjskay/ggdist/issues"
  )
  distr_function.dist_default(dist[[1]], prefix, fun)
}
#' @export
distr_function.dist_default = function(dist, prefix, fun) {
  # eat up extra args as they are ignored anyway
  # (and can cause problems, e.g. with cdf())
  function(x, ...) unlist(fun(dist, x))
}
#' @export
distr_function.rvar = distr_function.distribution

distr_pdf = function(dist) {
  # handle constant distributions
  if (distr_is_sample(dist) && distr_is_constant(dist)) {
    draws = distr_get_sample(dist)
    return(function(x, ...) ifelse(x == draws[[1]], Inf, 0))
  }

  distr_function(dist, "d", density)
}

#' @importFrom distributional cdf
distr_cdf = function(dist) {
  distr_function(dist, "p", cdf)
}

distr_quantile = function(dist) {
  distr_function(dist, "q", quantile)
}

#' @importFrom distributional generate
distr_random = function(dist) {
  distr_function(dist, "r", generate)
}


# point_interval ----------------------------------------------------------

#' Apply a point_interval to a distribution
#' @noRd
distr_point_interval = function(dist, args = list(), point_interval, ...) {
  UseMethod("distr_point_interval")
}
#' @export
distr_point_interval.default = function(dist, args = list(), point_interval, .simple_names = TRUE, ...) {
  point_interval(dist, .simple_names = .simple_names, ...)
}
#' @importFrom distributional dist_wrap
#' @export
distr_point_interval.character = function(dist, args = list(), point_interval, .width = .95, ...) {
  quantile_fun = distr_quantile(dist)

  map_dfr_(.width, function(w) {
    quantile_args = c(list(c(0.5, (1 - w)/2, (1 + w)/2)), args)
    quantiles = do.call(quantile_fun, quantile_args)
    data.frame(
      .value = quantiles[[1]],
      .lower = quantiles[[2]],
      .upper = quantiles[[3]],
      .width = w
    )
  })
}
#' @export
distr_point_interval.factor = function(dist, args = list(), point_interval, ...) {
  distr_point_interval(as.character(dist), args, point_interval, ...)
}
#' @export
distr_point_interval.distribution = function(dist, args = list(), point_interval, ...) {
  if (distr_is_sample(dist, args)) {
    distr_point_interval(distr_get_sample(dist, args), args, point_interval, ...)
  } else {
    NextMethod()
  }
}


# other distribution helpers ----------------------------------------------

#' Is a distribution discrete?
#' @noRd
distr_is_discrete = function(dist, args = list()) {
  if (inherits(dist, "rvar")) {
    is.integer(posterior::draws_of(dist))
  } else {
    withr::with_seed(1, {
      random_fun = distr_random(dist)
      one_value_from_dist = do.call(random_fun, c(list(1), args))
      is.integer(one_value_from_dist)
    })
  }
}

#' Is a distribution sample based?
#' @noRd
distr_is_sample = function(dist, args = list()) {
  inherits(dist, c("rvar", "dist_sample")) ||
    (
      inherits(dist, c("distribution")) &&
      length(dist) == 1 &&
      inherits(vctrs::field(dist, 1), "dist_sample")
    )
}

#' Get all samples from a sample-based distribution
#' @noRd
distr_get_sample = function(dist, args = list()) {
  if (inherits(dist, "rvar")) {
    posterior::draws_of(dist)
  } else if (inherits(dist, "distribution")) {
    vctrs::field(vctrs::field(dist, 1), 1)
  } else if (inherits(dist, "dist_sample")) {
    vctrs::field(dist, 1)
  }
}

#' Is a distribution a constant?
#' @noRd
distr_is_constant = function(dist, args = list()) {
  if (distr_is_sample(dist, args)) {
    x = distr_get_sample(dist, args)
    length(unique(x)) == 1
  } else {
    quantile_fun = distr_quantile(dist)
    lower = do.call(quantile_fun, c(list(.Machine$double.eps), args))
    upper = do.call(quantile_fun, c(list(1 - .Machine$double.neg.eps), args))
    isTRUE(lower == upper)
  }
}

#' Is x a distribution-like object? i.e. a distributional::distribution or
#' a posterior::rvar
#' @noRd
is_dist_like = function(x) {
  inherits(x, c("distribution", "rvar"))
}


# transforming density functions ------------------------------------------

# return the derivative of a transformation function from the scales package at
# the given y values. First attempts to find that analytical derivative, which
# works on most pre-defined transformation functions in scales; if that fails,
# uses numerical derivative
#' @importFrom stats numericDeriv D
f_deriv_at_y = function(f, y) {
  tryCatch({
    # attempt to find analytical derivative by pulling out the expression
    # for the transformation from the transformation function. Because all
    # scales functions are defined (currently) as simple wrappers around
    # single expressions (with no { ... }), we can be pretty naive here and
    # just try to pull out that single expression
    f_list = as.list(f)
    y_name = names(f_list)[[1]]
    f_expr = f_list[[length(f_list)]]
    f_deriv_expr = D(f_expr, y_name)

    # apply the analytical derivative to the y values
    # must do this within the environment of the transformation function b/c
    # some functions are defined as closures with other variables needed to
    # fully define the transformation (e.g. log10_trans has a `base` variable
    # equal to 10, which if left undefined this would not work)
    args = list(y)
    names(args) = y_name
    eval(f_deriv_expr, args, environment(f))
  }, error = function(e) {
    # if analytical approach fails, use numerical approach
    # need to convert y to numeric in case it's an integer (numericDeriv doesn't like ints)
    y = as.numeric(y)
    diag(attr(numericDeriv(quote(f(y)), "y"), "gradient"))
  })
}

# return a version of the provided density function f_X(...)
# transformed according to transformation trans
transform_pdf = function(f_X, y, trans, g_inverse_at_y = trans$inverse(y), ...) {
  # based on the fact that for Y = g(X),
  # f_Y(y) = f_X(g^−1(y)) * | g^-1'(y) |

  g_inverse = trans$inverse
  g_inverse_deriv_at_y = f_deriv_at_y(g_inverse, y)

  f_X(g_inverse_at_y, ...) * abs(g_inverse_deriv_at_y)
}

