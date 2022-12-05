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
  if (length(dist) > 1) stop(
    "lists of distributions should never have length > 1 here.\n",
    "Please report this bug at https://github.com/mjskay/ggdist/issues"
  )
  distr_function(dist[[1]], fun, ...)
}
#' @export
distr_function.distribution = function(dist, fun, ..., categorical_okay = FALSE) {
  if (length(dist) > 1) stop(
    "distributional objects should never have length > 1 here.\n",
    "Please report this bug at https://github.com/mjskay/ggdist/issues"
  )

  if (fun == "quantile" && categorical_okay && distr_is_factor_like(dist)) {
    # for categorical distributions --- but only when requested --- treat
    # them as ordinal so we can generate values in their bins. This is used
    # for stat_dots to put dots in bins approximately proportional to bin probs.
    levels = distr_levels(dist)
    probs = distr_probs(dist)
    Finv = stats::stepfun(c(0, cumsum(probs)), c(1, seq_along(probs), length(probs)))
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
  if (length(dist) > 1) stop(
    "rvars should never have length > 1 here.\n",
    "Please report this bug at https://github.com/mjskay/ggdist/issues"
  )

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
distr_point_interval.NULL = function(dist, point_interval, trans, ...) {
  data.frame()
}
#' @export
distr_point_interval.numeric = function(dist, point_interval, trans, ...) {
  point_interval(trans$transform(dist), .simple_names = TRUE, ...)
}
#' @export
distr_point_interval.factor = function(dist, point_interval, trans, ...) {
  # cannot calculate intervals on categorical distributions
  distr_point_interval(NA_real_, point_interval, trans, ...)
}
#' @export
distr_point_interval.ordered = function(dist, point_interval, trans, ...) {
  distr_point_interval(as.numeric(dist), point_interval, trans, ...)
}
#' @export
distr_point_interval.list = function(dist, point_interval, trans, ...) {
  if (length(dist) > 1) stop(
    "lists of distributions should never have length > 1 here.\n",
    "Please report this bug at https://github.com/mjskay/ggdist/issues"
  )
  distr_point_interval(dist[[1]], point_interval, trans, ...)
}
#' @importFrom distributional dist_transformed
#' @export
distr_point_interval.distribution = function(dist, point_interval, trans, ...) {
  if (distr_is_sample(dist)) {
    distr_point_interval(distr_get_sample(dist), point_interval, trans, ...)
  } else {
    t_dist = dist_transformed(dist, trans$transform, trans$inverse)
    point_interval(t_dist, .simple_names = TRUE, ...)
  }
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
    if (length(dist) > 1) stop(
      "lists of distributions should never have length > 1 here.\n",
      "Please report this bug at https://github.com/mjskay/ggdist/issues"
    )
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
    if (length(dist) > 1) stop(
      "lists of distributions should never have length > 1 here.\n",
      "Please report this bug at https://github.com/mjskay/ggdist/issues"
    )
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
        (inherits(d, "dist_sample") && inherits(distr_get_sample(d), c("character", "factor"))) ||
        is.character(vctrs::field(support(vec_restore(list(d), dist_missing())), "x")[[1]])
    })
    all(is_factor_like)
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
  inherits(dist, c("rvar", "dist_sample")) ||
    (
      inherits(dist, c("distribution")) &&
      length(dist) == 1 &&
      inherits(vctrs::field(dist, 1), "dist_sample")
    )
}

#' Get all samples from a sample-based distribution
#' @noRd
distr_get_sample = function(dist) {
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
distr_is_constant = function(dist) {
  if (distr_is_sample(dist)) {
    x = distr_get_sample(dist)
    return(length(unique(x)) == 1)
  }
  if (is_distribution(dist) && inherits(vec_data(dist)[[1]], "dist_mixture")) {
    if (length(dist) > 1) stop(
      "lists of distributions should never have length > 1 here.\n",
      "Please report this bug at https://github.com/mjskay/ggdist/issues"
    )
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


# custom distributions ----------------------------------------------------

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
    # if analytical approach fails, use numerical approach.
    # we use this (slightly less quick) approach instead of numDeriv::grad()
    # because numDeriv::grad() errors out if any data point fails while this
    # will return `NA` for those points
    vapply(y, numDeriv::jacobian, func = f, numeric(1))
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

