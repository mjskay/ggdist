# add_predicted_samples
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".iteration", ".pred"))


#' Add samples from the posterior fit or posterior prediction of a model to a data frame
#'
#' Given a data frame, adds samples from the posterior fit (aka the linear/link-level predictor)
#' or the posterior predictions of the model to the data in a long format.
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
#' @param model A supported Bayesian model fit / MCMC object. Currently
#' supported models include \code{\link[coda]{mcmc}}, \code{\link[coda]{mcmc.list}},
#' \code{\link[runjags]{runjags}}, \code{\link[rstan]{stanfit}}, and anything with its own
#' \code{\link[coda]{as.mcmc.list}} implementation. If you install the \code{tidybayes.rethinking}
#' package (available at \url{https://github.com/mjskay/tidybayes.rethinking}), \code{map} and
#' \code{map2stan} models from the \code{rethinking} package are also supported.
#' @param var The name of the output column for the predictions (default `code{"pred"}`) or fits
#' (default \code{"estimate"}, for compatibility with \code{\link[broom]{tidy}}).
#' @param n The number of samples per prediction / fit to return.
#' @param auxpars For \code{fitted_samples} and \code{add_fitted_samples}: Should auxiliary
#' parameters be included in the output? Valid only for models that support auxiliary parameters,
#' (such as submodels for variance parameters as in \code{brm}). If \code{TRUE}, auxiliary
#' parameters are included in the output as additional columns named after each parameter
#' (alternative names can be provided using a list or named vector, e.g. \code{c(sigma.hat = "sigma")}
#' would output the \code{"sigma"} parameter from a model as a column named \code{"sigma.hat"}).
#' If \code{FALSE}, auxiliary parameters are not included.
#' @param ... Additional arguments passed to the underlying prediction method for the type of
#' model given.
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
#' @importFrom dplyr mutate sample_n
#' @importFrom stats fitted predict
#' @export
add_predicted_samples = function(newdata, model, var = "pred", ..., n = NULL) {
  predicted_samples(model, newdata, var, ..., n = n)
}

#' @rdname add_predicted_samples
#' @export
add_fitted_samples = function(newdata, model, var = "estimate", auxpars = TRUE, ..., n = NULL) {
  fitted_samples(model, newdata, var, auxpars = auxpars, ..., n = n)
}

#' @rdname add_predicted_samples
#' @export
predicted_samples = function(model, newdata, var = "pred", ..., n = NULL) UseMethod("predicted_samples")

#' @rdname add_predicted_samples
#' @export
fitted_samples = function(model, newdata, var = "estimate", auxpars = TRUE, ..., n = NULL) UseMethod("fitted_samples")

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

# template for predicted_samples.stanreg and fitted_samples.stanreg
fitted_predicted_samples_stanreg_ = function(f_fitted_predicted, model, newdata, var, ...) {
  newdata %>%
    data.frame(
      #for some reason calculating row here instead of in a subsequent mutate()
      #is about 3 times faster
      .row = factor(1:nrow(.)),
      .chain = as.numeric(NA),
      t(f_fitted_predicted(model, newdata = ., ...)),
      check.names = FALSE
    ) %>%
    gather_(".iteration", var, names(.)[(ncol(newdata) + 3):ncol(.)]) %>%
    mutate(
      .iteration = as.numeric(.iteration)
    ) %>%
    group_by_(".row", .dots = colnames(newdata))
}

#' @rdname add_predicted_samples
#' @export
predicted_samples.stanreg = function(model, newdata, var = "pred", ..., n = NULL) {
  if (!requireNamespace("rstantools", quietly = TRUE)) {
    stop("The `rstantools` package is needed for `predicted_samples` to support `stanreg` objects.", call. = FALSE)
  }

  fitted_predicted_samples_stanreg_(rstantools::posterior_predict, model, newdata, var, draws = n, ...)
}

#' @rdname add_predicted_samples
#' @export
fitted_samples.stanreg = function(model, newdata, var = "estimate", auxpars = TRUE, ..., n = NULL) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is needed for `fitted_samples` to support `stanreg` objects.", call. = FALSE)
  }

  samples = fitted_predicted_samples_stanreg_(rstanarm::posterior_linpred, model, newdata, var, ...)
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
predicted_samples.brmsfit = function(model, newdata, var = "pred", ..., n = NULL) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `predicted_samples` to support `brmsfit` objects.", call. = FALSE)
  }

  fitted_predicted_samples_stanreg_(predict, model, newdata, var, summary = FALSE, nsamples = n, ...)
}

#' @rdname add_predicted_samples
#' @importFrom rlang is_true is_false
#' @export
fitted_samples.brmsfit = function(model, newdata, var = "estimate", auxpars = TRUE, ..., n = NULL) {
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
  samples = fitted_predicted_samples_stanreg_(
    fitted, model, newdata, var, summary = FALSE, nsamples = n, dpar = NULL, ...
  )

  for (i in seq_along(dpars)) {
    varname = names(dpars)[[i]]
    dpar = dpars[[i]]
    samples[[varname]] = fitted_predicted_samples_stanreg_(
      fitted, model, newdata, var = ".estimate", summary = FALSE, nsamples = n, dpar = dpar, ...
    )[[".estimate"]]
  }

  samples
}
