# Helper methods for working with distributions
#
# Author: mjskay
###############################################################################



# pdf, cdf, and quantile functions ----------------------------------------

#' Helper function to create pdf/cdf/quantile functions
#' @param dist The distribution, either as a string (given the suffix to a
#' p/q/d/r function set), \pkg{distributional} object, or [rvar()]
#' @param prefix For strings, one of `"p"`, `"q"`, `"d"`, or `"r"`
#' @param fun For \pkg{distributional} objects and `rvar`s, the function to apply (e.g.
#' [`pdf`], [`cdf`], [`quantile`], or [`generate`]).
#' @noRd
distr_function = function(dist, fun, ...) {
  UseMethod("distr_function")
}
#' @export
distr_function.default = function(dist, fun, ...) {
  stop0("The `dist` aesthetic does not support objects of type ", deparse0(class(dist)))
}
#' @export
distr_function.list = function(dist, fun, ...) {
  check_dist_length_1(dist)

  distr_function(dist[[1]], fun, ...)
}
#' @importFrom stats stepfun
#' @export
distr_function.distribution = function(dist, fun, ..., categorical_okay = FALSE) {
  check_dist_length_1(dist)

  if (fun == "quantile" && categorical_okay && distr_is_factor_like(dist)) {
    # for categorical distributions --- but only when requested --- treat
    # them as ordinal so we can generate values in their bins. This is used
    # for stat_dots to put dots in bins approximately proportional to bin probs.
    levels = distr_levels(dist)
    probs = distr_probs(dist)
    Finv = stepfun(c(0, cumsum(probs)), c(1, seq_along(probs), length(probs)))
    return(function(x, ...) levels[Finv(x)])
  }
  # eat up extra args as they are ignored anyway
  # (and can cause problems, e.g. with cdf())
  # TODO: at least until #114 / distributional/#72
  fun = match.fun(fun)
  function(x, ...) unlist(fun(dist[[1]], x))
}
#' @export
distr_function.rvar = function(dist, fun, ...) {
  check_dist_length_1(dist)

  fun = match.fun(fun)
  function(x, ...) unlist(fun(dist[[1]], x, ...))
}
#' @export
distr_function.rvar_factor = function(dist, fun, ...) {
  if (!inherits(dist, "rvar_ordered") && fun %in% c("cdf", "quantile")) {
    # cdf and quantile are undefined on unordered dists, so just return NA
    function(x, ...) {
      rep_len(NA_real_, length(x))
    }
  } else if (fun %in% c("density", "cdf")) {
    # for density and cdf we must translate numeric input to factor levels
    f = force(NextMethod())
    levels = levels(dist)
    function(x, ...) {
      # only x values > 0 are valid; values <= 0 are 0s
      gt_0 = x > 0
      x_gt_0_levels = levels[x[gt_0]]
      f = numeric(length(x))
      f[gt_0] = f(x_gt_0_levels, ...)
      f
    }
  } else {
    NextMethod()
  }
}

distr_pdf = function(dist, ...) {
  distr_function(dist, "density", ...)
}

#' @importFrom distributional cdf
distr_cdf = function(dist, ...) {
  distr_function(dist, "cdf", ...)
}

#' @param categorical_okay if TRUE, categorical dists are treated as ordinal
#' in order to generate values in bins (e.g. for use with stat_dots)
#' @noRd
distr_quantile = function(dist, ..., categorical_okay = FALSE) {
  distr_function(dist, "quantile", ..., categorical_okay = categorical_okay)
}

#' @importFrom distributional generate
distr_random = function(dist, ...) {
  distr_function(dist, "generate", ...)
}


# point_interval ----------------------------------------------------------

#' Apply a point_interval to a distribution
#' @noRd
distr_point_interval = function(dist, point_interval, trans, ...) {
  UseMethod("distr_point_interval")
}
#' @export
distr_point_interval.list = function(dist, point_interval, trans, ...) {
  check_dist_length_1(dist)

  distr_point_interval(dist[[1]], point_interval, trans, ...)
}
#' @importFrom distributional dist_transformed
#' @export
distr_point_interval.distribution = function(dist, point_interval, trans, ...) {
  if (distr_is_sample(dist)) {
    x = distr_get_sample(dist)
    if (is.ordered(x)) x = as.numeric(x)
    # cannot calculate intervals on categorical distributions
    else if (is.factor(x)) x = rep(NA_real_, length(x))

    t_dist = distr_set_sample(dist, trans$transform(x))
  } else {
    t_dist = dist_transformed(dist, trans$transform, trans$inverse)
  }
  point_interval(t_dist, .simple_names = TRUE, ...)
}
#' @export
distr_point_interval.rvar = distr_point_interval.distribution

# other distribution helpers ----------------------------------------------

#' Is a distribution discrete?
#' @noRd
distr_is_discrete = function(dist) {
  if (inherits(dist, "rvar_factor")) {
    return(TRUE)
  }
  if (inherits(dist, "rvar")) {
    return(is.integer(posterior::draws_of(dist)))
  }
  if (is_distribution(dist) && inherits(vec_data(dist)[[1]], "dist_mixture")) {
    check_dist_length_1(dist)

    # special case: discrete mixtures can't be reliably detected by the
    # method below, so we do it by asking if all components of the mixture are discrete
    dists = vec_restore(vec_data(dist)[[1]]$dist, dist_missing())
    is_discrete = map_lgl_(dists, distr_is_discrete)
    return(all(is_discrete))
  }

  withr::with_seed(1, {
    one_value_from_dist = distr_random(dist)(1)
    is.integer(one_value_from_dist) || is.logical(one_value_from_dist) || is.character(one_value_from_dist)
  })
}

#' Is a distribution logical?
#' @noRd
distr_is_logical = function(dist) {
  if (inherits(dist, "rvar")) {
    return(is.logical(posterior::draws_of(dist)))
  }
  if (is_distribution(dist) && inherits(vec_data(dist)[[1]], "dist_mixture")) {
    check_dist_length_1(dist)

    # special case: logical mixtures can't be reliably detected by the
    # method below, so we do it by asking if all components of the mixture are logical
    dists = vec_restore(vec_data(dist)[[1]]$dist, dist_missing())
    is_logical = map_lgl_(dists, distr_is_logical)
    return(all(is_logical))
  }

  withr::with_seed(1, {
    one_value_from_dist = distr_random(dist)(1)
    is.logical(one_value_from_dist)
  })
}

#' Is a distribution a non-numeric discrete dist? e.g. character, factor
#' @noRd
distr_is_factor_like = function(dist) {
  inherits(dist, "rvar_factor") || if (inherits(dist, "distribution")) {
    is_factor_like = map_lgl_(vctrs::vec_data(dist), function(d) {
      inherits(d, c("dist_categorical", "ggdist__wrapped_categorical")) ||
        (inherits(d, c("dist_sample", "ggdist__weighted_sample")) && inherits(distr_get_sample(d), c("character", "factor"))) ||
        is.character(vctrs::field(support(vec_restore(list(d), dist_missing())), "x")[[1]])
    })
    length(dist) > 0 && all(is_factor_like)
  } else {
    FALSE
  }
}

#' For factor-like distributions, get their levels
#' @noRd
distr_levels = function(dist) {
  if (inherits(dist, "rvar_factor")) {
    levels(dist)
  } else if (inherits(dist, "distribution")) {
    levels = lapply(vec_data(dist), distr_levels)
    unique(do.call(c, levels))
  } else if (inherits(dist, "dist_categorical")) {
    as.character(dist[["x"]] %||% seq_along(dist[["p"]]))
  } else if (inherits(dist, "ggdist__wrapped_categorical")) {
    distr_levels(dist[["wrapped_dist"]])
  } else if (inherits(dist, "dist_sample")) {
    s = distr_get_sample(dist)
    if (is.factor(s)) {
      levels(s)
    } else {
      unique(s)
    }
  } else {
    warning("Don't know how to determine the levels of distribution: ", format(dist))
    NULL
  }
}

#' For categorical distributions, get their probabilities
#' @noRd
distr_probs = function(dist) {
  if (inherits(dist, "distribution") && length(dist) == 1) {
    distr_probs(vec_data(dist)[[1]])
  } else if (inherits(dist, "dist_categorical")) {
    dist[["p"]]
  } else if (inherits(dist, "ggdist__wrapped_categorical")) {
    distr_probs(dist[["wrapped_dist"]])
  } else {
    warning("Don't know how to determine the category probabilities of distribution: ", format(dist))
    NULL
  }
}

#' Is a distribution multivariate?
#' @noRd
distr_is_multivariate = function(dist) {
  if (inherits(dist, "rvar")) {
    length(dist) > 1
  } else {
    withr::with_seed(1, {
      one_value_from_dist = distr_random(dist)(1)
      length(one_value_from_dist) > 1
    })
  }
}

#' Is a distribution sample based?
#' @noRd
distr_is_sample = function(dist) {
  inherits(dist, c("rvar", "dist_sample", "ggdist__weighted_sample")) ||
    (
      inherits(dist, c("distribution")) &&
      length(dist) == 1 &&
      inherits(vctrs::field(dist, 1), c("dist_sample", "ggdist__weighted_sample"))
    )
}

#' Get all samples from a sample-based distribution
#' @noRd
distr_get_sample = function(dist) {
  if (inherits(dist, "rvar")) {
    posterior::draws_of(dist)
  } else if (inherits(dist, "distribution")) {
    vctrs::field(dist, 1)[["x"]]
  } else if (inherits(dist, c("dist_sample", "ggdist__weighted_sample"))) {
    dist[["x"]]
  }
}

#' Modify samples from a sample-based distribution
#' @noRd
distr_set_sample = function(dist, value) {
  if (inherits(dist, "rvar")) {
    posterior::draws_of(dist) = value
  } else if (inherits(dist, "distribution")) {
    old_class = oldClass(dist)
    dist = unclass(dist)
    dist[[1]] = distr_set_sample(dist[[1]], value)
    class(dist) = old_class
  } else if (inherits(dist, c("dist_sample", "ggdist__weighted_sample"))) {
    dist[["x"]] = value
  }
  dist
}

#' Get all weights from a weighted sample-based distribution
#' @noRd
distr_get_sample_weights = function(dist) {
  if (inherits(dist, "distribution")) {
    distr_get_sample_weights(vctrs::field(dist, 1))
  } else {
    weights(dist)
  }
}

#' Is a distribution a constant?
#' @noRd
distr_is_constant = function(dist) {
  if (distr_is_sample(dist)) {
    x = distr_get_sample(dist)
    return(length(unique(x)) == 1)
  }
  if (is_distribution(dist) && inherits(vec_data(dist)[[1]], "dist_mixture")) {
    check_dist_length_1(dist)

    # special case: discrete constant distributions can't be reliably detected by the
    # method below, so we do it by asking if all components of the mixture are constant
    # and equal
    dists = vec_restore(vec_data(dist)[[1]]$dist, dist_missing())
    is_constant = map_lgl_(dists, distr_is_constant)
    if (all(is_constant)) {
      means = mean(dists)
      return(all(means == means[[1]]))
    }
    return(FALSE)
  }

  quantile_fun = distr_quantile(dist)
  lower = quantile_fun(.Machine$double.eps)
  upper = quantile_fun(1 - .Machine$double.neg.eps)
  isTRUE(lower == upper)
}

#' Is a distribution missing / NA (or equivalent)?
#' @noRd
distr_is_missing = function(dist) {
  is.null(dist) || anyNA(dist) || identical(dist, list(NULL))
}

#' Is x a distribution-like object? i.e. a distributional::distribution or
#' a posterior::rvar
#' @noRd
is_dist_like = function(x) {
  inherits(x, c("distribution", "rvar"))
}


# wrapped categorical distribution ----------------------------------------------------

#' A wrapped categorical distribution with a different level set
#' @noRd
.dist_wrapped_categorical = function(wrapped_dist, new_levels) {
  distributional::new_dist(
    wrapped_dist = unclass(wrapped_dist),
    new_levels = list(new_levels),
    class = "ggdist__wrapped_categorical"
  )
}

#' @export
density.ggdist__wrapped_categorical = function(x, at, ...) {
  gt_0 = at > 0
  at_gt_0_levels = x[["new_levels"]][at[gt_0]]
  if (is.null(x[["wrapped_dist"]]$x)) {
    # TODO: hack: when x (levels) are missing from the wrapped dist, then the levels are
    # actually the numeric indices of the probability vector, so convert
    # to numeric
    at_gt_0_levels = as.numeric(at_gt_0_levels)
  }
  f = numeric(length(at))
  f[gt_0] = density(x[["wrapped_dist"]], at_gt_0_levels, ...)
  f
}

#' @export
cdf.ggdist__wrapped_categorical = function(x, q, ...) {
  rep_len(NA_real_, length(q))
}

#' @export
quantile.ggdist__wrapped_categorical = function(x, p, ...) {
  rep_len(NA_real_, length(p))
}

#' @export
generate.ggdist__wrapped_categorical = function(x, ...) {
  generate(x[["wrapped_dist"]], ...)
}


# weighted sample distribution ----------------------------------------------------

#' A sample distribution with weights
#' @noRd
.dist_weighted_sample = function(x, weights) {
  x = vec_cast(x, list_of(x[[1]]))
  weights = vec_cast(weights, list_of(numeric()))

  weight_is_null = vapply(weights, is.null, logical(1))
  stopifnot(all(lengths(x) == lengths(weights) | weight_is_null))

  # only allow univariate samples since that's all we should ever end
  # up with via mappings in ggplot
  stopifnot(all(lengths(lapply(x, dim)) <= 1))

  distributional::new_dist(
    x = x,
    weights = weights,
    class = "ggdist__weighted_sample"
  )
}

#' @export
format.ggdist__weighted_sample <- function(x, ...){
  paste0("weighted_sample[", length(x[["x"]]), "]")
}

#' @export
density.ggdist__weighted_sample = function(x, at, ..., na.rm = TRUE) {
  d = density_bounded(x[["x"]], weights = x[["weights"]], trim = TRUE, ..., na.rm = na.rm)
  approx(d$x, d$y, xout = at, yleft = 0, yright = 0, ties = "ordered")$y
}

#' @export
cdf.ggdist__weighted_sample = function(x, q, ..., na.rm = TRUE) {
  F_x = weighted_ecdf(x[["x"]], weights = x[["weights"]], ..., na.rm = na.rm)
  F_x(q)
}

#' @export
quantile.ggdist__weighted_sample = function(x, p, ..., na.rm = TRUE, names = FALSE) {
  weighted_quantile(x[["x"]], p, weights = x[["weights"]], ..., na.rm = na.rm, names = names)
}

#' @export
generate.ggdist__weighted_sample = function(x, times, ...) {
  i = sample.int(length(x[["x"]]), times, replace = TRUE, prob = x[["weights"]])
  x[["x"]][i]
}

#' @export
mean.ggdist__weighted_sample = function(x, ...) {
  if (is.null(x[["weights"]])){
    mean(x[["x"]])
  } else {
    weighted.mean(x[["x"]], x[["weights"]])
  }
}

#' @importFrom distributional variance
#' @export
variance.ggdist__weighted_sample = function(x, ...) {
  if (is.null(x[["weights"]])){
    variance(x[["x"]])
  } else {
    weighted_var(x[["x"]], x[["weights"]])
  }
}

#' @importFrom stats weights
#' @export
weights.ggdist__weighted_sample = function(object, ...) {
  object[["weights"]]
}


# transforming density functions ------------------------------------------

# return the derivative of the inverse of a transformation from the scales package at
# the given y values. First attempts to find that analytical derivative, which
# works on most pre-defined transformation functions in scales; if that fails,
# uses numerical derivative
#' @importFrom stats D
inverse_deriv_at_y = function(trans, y) {
  if (!is.null(trans$d_inverse)) {
    # use the function for the derivative if it was supplied (it is optional
    # and so may not be present)
    trans$d_inverse(y)
  } else tryCatch({
    # attempt to find analytical derivative by pulling out the expression
    # for the transformation from the transformation function. Because many
    # scale functions are defined as simple wrappers around
    # single expressions (with no { ... }), we can be pretty naive here and
    # just try to pull out that single expression
    f = trans$inverse
    f_list = as.list(f)
    y_name = names(f_list)[[1]]
    f_expr = f_list[[length(f_list)]]
    f_deriv_expr = D(f_expr, y_name)

    # apply the analytical derivative to the y values
    # must do this within the environment of the transformation function b/c
    # some functions are defined as closures with other variables needed to
    # fully define the transformation
    args = list(y)
    names(args) = y_name
    eval(f_deriv_expr, args, environment(f))
  }, error = function(e) {
    # if analytical approach fails, use numerical approach.
    # we use this (slightly less quick) approach instead of numDeriv::grad()
    # because numDeriv::grad() errors out if any data point fails while this
    # will return `NA` for those points
    vapply(y, numDeriv::jacobian, func = trans$inverse, numeric(1))
  })
}

# return a version of the provided density function f_X(...)
# transformed according to transformation trans
transform_pdf = function(f_X, y, trans, g_inverse_at_y = trans$inverse(y), ...) {
  # based on the fact that for Y = g(X),
  # f_Y(y) = f_X(g^−1(y)) * | g^-1'(y) |

  g_inverse_deriv_at_y = inverse_deriv_at_y(trans, y)

  f_X(g_inverse_at_y, ...) * abs(g_inverse_deriv_at_y)
}


# helpers -----------------------------------------------------------------

#' @importFrom cli cli_abort
check_dist_length_1 = function(dist) {
  if (length(dist) > 1) cli_abort(c(
    "Distribution-like objects ({.pkg distributional} objects, {.fun posterior::rvar} objects,
    or lists of either) should never have length > 1 here.",
    ">" = "Please report this bug at {.url https://github.com/mjskay/ggdist/issues},
      along with a {.href [reprex](https://reprex.tidyverse.org/)} and the output of
      {.code rlang::last_trace()}."
  ))
}
