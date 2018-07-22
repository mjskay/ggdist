# [add_]fitted_samples
#
# Author: mjskay
###############################################################################

#' @rdname add_predicted_samples
#' @export
add_fitted_samples = function(newdata, model, var = "estimate", ..., n = NULL, re_formula = NULL,
  category = "category", dpar = FALSE, scale = c("response", "linear")
) {
  fitted_samples(model, newdata, var, ..., n = n, re_formula = re_formula,
    category = category, dpar = dpar, scale = scale)
}

#' @rdname add_predicted_samples
#' @export
fitted_samples = function(model, newdata, var = "estimate", ..., n = NULL, re_formula = NULL,
  category = "category", dpar = FALSE, scale = c("response", "linear")
) {
  UseMethod("fitted_samples")
}

#' @rdname add_predicted_samples
#' @export
fitted_samples.default = function(model, newdata, ...) {
  stop(paste0("Models of type ", deparse0(class(model)), " are not currently supported by `fitted_samples`"))
}

#' @rdname add_predicted_samples
#' @export
fitted_samples.stanreg = function(model, newdata, var = "estimate", ..., n = NULL, re_formula = NULL,
  category = "category", dpar = FALSE, scale = c("response", "linear")
) {
  transform = match.arg(scale) == "response"

  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is needed for `fitted_samples` to support `stanreg` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(list(...)), "[add_]fitted_samples", re_formula = "re.form", scale = "transform"
  )

  samples = fitted_predicted_samples_brmsfit_(rstanarm::posterior_linpred, model, newdata, var, ...,
    category = category, re.form = re_formula, transform = transform, is_brms = FALSE
  )
  # posterior_linpred, unlike posterior_predict, does not have a "draws" argument for some reason
  if (!is.null(n)) {
    iterations = sample(samples$.iteration, n)
    samples[samples[[".iteration"]] %in% iterations,]
  } else {
    samples
  }
}

#' @rdname add_predicted_samples
#' @importFrom rlang is_true is_false is_empty
#' @importFrom purrr map
#' @export
fitted_samples.brmsfit = function(model, newdata, var = "estimate", ..., n = NULL, re_formula = NULL,
  category = "category", dpar = FALSE, scale = c("response", "linear")
) {
  scale = match.arg(scale)

  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `fitted_samples` to support `brmsfit` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(list(...)), "[add_]fitted_samples", n = "nsamples", dpar = "dpars"
  )

  # get the names of distributional regression parameters to include
  dpars = if (is_true(dpar)) {
    names(brms::parse_bf(model$formula)$dpar)
  } else if (is_false(dpar)) {
    NULL
  } else {
    dpar
  }
  if (is_empty(dpars)) {
    # the above conditions might return an empty vector, which does not play well with the code below
    # (if there are no dpars, it is expected that dpars is NULL)
    dpars = NULL
  }

  # missing names default to the same name used for the parameter in the model
  if (is.null(names(dpars))) {
    names(dpars) = dpars
  } else {
    missing_names = is.na(names(dpars)) | names(dpars) == ""
    names(dpars)[missing_names] = dpars[missing_names]
  }


  # get the samples for the primary parameter first so we can stick the other estimates onto it
  samples = fitted_predicted_samples_brmsfit_(
    fitted, model, newdata, var, ...,
    category = category, nsamples = n, re_formula = re_formula, dpar = NULL, scale = scale
  )


  for (i in seq_along(dpars)) {
    varname = names(dpars)[[i]]
    dpar_fitted_samples = fitted_predicted_samples_brmsfit_(
      fitted, model, newdata, var = ".estimate", ...,
      category = category, nsamples = n, re_formula = re_formula, dpar = dpars[[i]], scale = scale
    )

    if (nrow(dpar_fitted_samples) == nrow(samples)) {
      samples[[varname]] = dpar_fitted_samples[[".estimate"]]
    } else {
      # in some models (such as ordinal models) the tidy samples from the dpars can have fewer rows than
      # the estimate if the estimate is on the response scale and the dpars are not. Stop in this case.
      stop(
        'Different number of rows in fitted samples for dpar "', dpars[[i]], '" and the estimate. This\n',
        'can happen in ordinal and categorical models when scale = "response". Try scale = "linear" instead.'
      )
    }
  }

  samples
}
