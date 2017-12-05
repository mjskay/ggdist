# add_predicted_samples
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".iteration", ".pred"))


#' Add samples from the posterior fit or posterior prediction of a model to a data frame
#'
#' Given a data frame and a model, adds samples from the posterior fit (aka the linear/link-level predictor)
#' or the posterior predictions of the model to the data frame in a long format.
#'
#' \code{add_fitted_samples} adds samples from the posterior linear predictor (or the "link") to
#' the data. It corresponds to \code{\link[rstanarm]{posterior_linpred}} in \code{rstanarm} or
#' \code{\link[brms]{fitted.brmsfit}} in \code{brms}.
#'
#' \code{add_predicted_samples} adds samples from the posterior prediction to
#' the data. It corresponds to \code{\link[rstanarm]{posterior_predict}} in \code{rstanarm} or
#' \code{\link[brms]{predict.brmsfit}} in \code{brms}.
#'
#' \code{add_fitted_samples} and \code{fitted_samples} are alternate spellings of the
#' same function with opposite order of the first two arguments to facilitate use in data
#' processing pipelines that start either with a data frame or a model. Similarly,
#' \code{add_predicted_samples} and \code{predicted_samples} are alternate spellings.
#'
#' Given equal choice between the two, \code{add_fitted_samples} and \code{add_predicted_samples}
#' are the preferred spellings.
#'
#' @param newdata Data frame to generate predictions from. If omitted, most model types will
#' generate predictions from the data used to fit the model.
#' @param model A supported Bayesian model fit / MCMC object that can provide fits and predictions. Currently
#' supported models include \code{\link[rstanarm]{stanreg-objects}} (e.g. \code{rstanarm} models) and
#' \code{\link[brms]{brm}} models. While other functions in this package (like \code{\link{spread_samples}})
#' support a wider range of models, to work with \code{add_fitted_samples} and \code{add_predicted_samples}
#' a model must provide an interface for generating predictions, thus more generic Bayesian modeling interfaces
#' like \code{runjags} and \code{rstan} are not directly supported for these functions (only wrappers around
#' those languages that provide predictions, like \code{rstanarm} and \code{brm}, are supported here).
#' @param var The name of the output column for the predictions (default \code{"pred"}) or fits
#' (default \code{"estimate"}, for compatibility with \code{\link[broom]{tidy}}).
#' @param ... Additional arguments passed to the underlying prediction method for the type of
#' model given.
#' @param n The number of samples per prediction / fit to return.
#' @param re_formula formula containing group-level effects to be considered in the prediction.
#' If \code{NULL} (default), include all group-level effects; if \code{NA}, include no group-level effects.
#' Some model types (such as \code{\link[brms]{brm}} and \code{\link[rstanarm]{stanreg-objects}}) allow
#' marginalizing over grouping factors by specifying new levels of a factor in \code{newdata}. In the case of
#' \code{\link[brms]{brm}}, you must also pass \code{allow_new_levels = TRUE} here to include new levels (see
#' \code{\link[brms]{predict.brmsfit}}).
#' @param category For ordinal and multinomial models, the name of the column to put the category names
#' into in the resulting data frame. For these models, multiple sets of rows will be returned per prediction,
#' one for each category.
#' @param auxpars For \code{fitted_samples} and \code{add_fitted_samples}: Should auxiliary
#' parameters be included in the output? Valid only for models that support auxiliary parameters,
#' (such as submodels for variance parameters as in \code{brm}). If \code{TRUE}, auxiliary
#' parameters are included in the output as additional columns named after each parameter
#' (alternative names can be provided using a list or named vector, e.g. \code{c(sigma.hat = "sigma")}
#' would output the \code{"sigma"} parameter from a model as a column named \code{"sigma.hat"}).
#' If \code{FALSE}, auxiliary parameters are not included.
#' @param scale Either \code{"response"} or \code{"linear"}. If \code{"response"}, results are returned
#' on the scale of the response variable. If \code{"linear"}, fitted values are returned on the scale of
#' the linear predictor.
#' @return A data frame (actually, a \code{\link[tibble]{tibble}}) with a \code{.row} column (a
#' factor grouping rows from the input \code{newdata}), \code{.chain} column (the chain
#' each sample came from, or \code{NA} if the model does not provide chain information),
#' \code{.iteration} column (the iteration the sample came from), and \code{.pred} column (a
#' sample from the posterior predictive distribution). For convenience, the resulting data
#' frame comes grouped by the original input rows.
#' @author Matthew Kay
#' @seealso \code{\link{spread_samples}}
#' @keywords manip
#' @examples
#'
#' ##TODO
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom dplyr mutate sample_n ungroup group_by
#' @importFrom stats fitted predict
#' @export
add_predicted_samples = function(newdata, model, var = "pred", ..., n = NULL, re_formula = NULL) {
  predicted_samples(model, newdata, var, ..., n = n, re_formula = re_formula)
}

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
predicted_samples = function(model, newdata, var = "pred", ..., n = NULL, re_formula = NULL) {
  UseMethod("predicted_samples")
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
predicted_samples.default = function(model, newdata, ...) {
  stop(paste0("Models of type ", deparse0(class(model)), " are not currently supported by `predicted_samples`"))
}

#' @rdname add_predicted_samples
#' @export
fitted_samples.default = function(model, newdata, ...) {
  stop(paste0("Models of type ", deparse0(class(model)), " are not currently supported by `fitted_samples`"))
}

#' @rdname add_predicted_samples
#' @export
predicted_samples.stanreg = function(model, newdata, var = "pred", ..., n = NULL, re_formula = NULL) {
  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("The `rstantools` package is needed for `predicted_samples` to support `stanreg` objects.", call. = FALSE)
  }

  fitted_predicted_samples_brmsfit_(rstantools::posterior_predict, model, newdata, var, ...,
    draws = n, re.form = re_formula, is_brms = FALSE
  )
}

#' @rdname add_predicted_samples
#' @export
fitted_samples.stanreg = function(model, newdata, var = "estimate", ..., n = NULL, re_formula = NULL,
  category = "category", auxpars = TRUE, scale = c("response", "linear")
) {
  transform = match.arg(scale) == "response"

  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is needed for `fitted_samples` to support `stanreg` objects.", call. = FALSE)
  }

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
#' @export
predicted_samples.brmsfit = function(model, newdata, var = "pred", ..., n = NULL, re_formula = NULL) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `predicted_samples` to support `brmsfit` objects.", call. = FALSE)
  }
  fun_args = as.list(environment())
  std_args = list(pass = list(f_fitted_predicted = predict, nsamples = n),
                  names = c("n"))

  if(any(names(std_args$pass) %in% names(list(...)))) {
    warning("Use tidybayes add_predicted_samples arguments for",
            " prediction methods. See function documentation for details.")
  }
  std_args$pass[names(list(...))] = list(...)
  fun_args = fun_args[!(names(fun_args) %in% std_args$names)]

  do.call(fitted_predicted_samples_brmsfit_, c(fun_args, std_args$pass))
}

#' @rdname add_predicted_samples
#' @importFrom rlang is_true is_false
#' @export
fitted_samples.brmsfit = function(model, newdata, var = "estimate", ..., n = NULL, re_formula = NULL,
  category = "category", auxpars = TRUE, scale = c("response", "linear")
) {
  scale = match.arg(scale)

  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `fitted_samples` to support `brmsfit` objects.", call. = FALSE)
  }

  # get the names of distributional regression parameters to include
  dpars = if (is_true(auxpars)) {
    names(model$formula$pforms)
  } else if (is_false(auxpars)) {
    NULL
  } else {
    auxpars
  }

  # missing names default to the same name used for the parameter in the model
  missing_names = is.null(names(dpars))
  names(dpars)[missing_names] = dpars[missing_names]

  # get the samples for the primary parameter first so we can stick the other estimates onto it
  if(hasArg(nsamples)) {
    samples = fitted_predicted_samples_brmsfit_(
      fitted, model, newdata, var, ...,
      category = category, re_formula = re_formula, dpar = NULL, scale = scale
    )
  }
  else {
    samples = fitted_predicted_samples_brmsfit_(
      fitted, model, newdata, var, ...,
      category = category, nsamples = n, re_formula = re_formula, dpar = NULL, scale = scale
    )
  }

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


#' @importFrom arrayhelpers array2df ndim
fitted_predicted_samples_brmsfit_ = function(f_fitted_predicted, model, newdata, var, category, ...,
  is_brms = TRUE, summary = NULL #summary is ignored, we change it ourselves
) {
  newdata %<>% ungroup()

  column_format = list(
    .iteration = NA,        #NA here means numeric
    .row = NA
  )

  fits_preds <- if (is_brms) {
    # only brms has/needs the summary parameter
    f_fitted_predicted(model, newdata = newdata, summary = FALSE, ...)
  } else {
    f_fitted_predicted(model, newdata = newdata, ...)
  }

  groups = union(colnames(newdata), ".row")

  if (ndim(fits_preds) == 3) {
    #3 dimensions implies a categorical outcome, we must determine the category names
    #however, with summary = FALSE brms does not provide category names, so we'll grab them
    #by fitting just one row without it
    category_names = dimnames(f_fitted_predicted(model, newdata = newdata[1, ], ...))[[3]]
    column_format[[3]] = category_names
    names(column_format)[[3]] = category
    groups %<>% union(category)
  }

  fits_preds_df = array2df(fits_preds, column_format, label.x = var)

  #rstanarm does something weird that prevents array2df from properly seeing .row and .iteration as numerics,
  #so we have to convert them manually from factors. While we're at it, we should also make sure they are integers.
  if (is.factor(fits_preds_df$.row) || is.character(fits_preds_df$.row)) {
    fits_preds_df$.row = as.integer(as.character(fits_preds_df$.row))
  } else {
    fits_preds_df$.row = as.integer(fits_preds_df$.row)
  }
  if (is.factor(fits_preds_df$.iteration) || is.character(fits_preds_df$.iteration)) {
    fits_preds_df$.iteration = as.integer(as.character(fits_preds_df$.iteration))
  } else {
    fits_preds_df$.iteration = as.integer(fits_preds_df$.iteration)
  }


  newdata %>%
    mutate(
      .row = seq_len(n()),
      .chain = as.integer(NA)
    ) %>%
    inner_join(fits_preds_df, by = ".row") %>%
    select(-!!sym(var), !!sym(var)) %>%
    group_by(!!!syms(groups))
}
