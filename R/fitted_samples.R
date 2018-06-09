# [add_]fitted_samples
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".iteration"))


#' @rdname add_predicted_samples
#' @export
add_fitted_samples = function(newdata, model, var = "estimate", ..., n = NULL, re_formula = NULL,
  category = "category", auxpars = TRUE, scale = c("response", "linear")
) {
  fitted_samples(model, newdata, var, ..., n = n, re_formula = re_formula,
    category = category, auxpars = auxpars, scale = scale)
}

#' @rdname add_predicted_samples
#' @export
fitted_samples = function(model, newdata, var = "estimate", ..., n = NULL, re_formula = NULL,
  category = "category", auxpars = TRUE, scale = c("response", "linear")
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
  category = "category", auxpars = TRUE, scale = c("response", "linear")
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
    samples %>%
      filter(.iteration %in% !!iterations)
  } else {
    samples
  }
}

#' @rdname add_predicted_samples
#' @importFrom rlang is_true is_false is_empty
#' @importFrom purrr map
#' @export
fitted_samples.brmsfit = function(model, newdata, var = "estimate", ..., n = NULL, re_formula = NULL,
  category = "category", auxpars = TRUE, scale = c("response", "linear")
) {
  scale = match.arg(scale)

  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `fitted_samples` to support `brmsfit` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(list(...)), "[add_]fitted_samples", n = "nsamples", auxpars = "dpars"
  )

  # get the names of distributional regression parameters to include
  dpars = if (is_true(auxpars)) {
    names(brms::parse_bf(model$formula)$dpar) %>%
      .[. != "mu"]      #mu is the primary parameter, filter it out
                        #TODO: find a non-hacky solution to this
  } else if (is_false(auxpars)) {
    NULL
  } else {
    auxpars
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
    dpar = dpars[[i]]
    samples[[varname]] = fitted_predicted_samples_brmsfit_(
      fitted, model, newdata, var = ".estimate", ...,
      category = category, nsamples = n, re_formula = re_formula, dpar = dpar, scale = scale
    )[[".estimate"]]
  }

  samples
}
