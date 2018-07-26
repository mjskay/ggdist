# [add_]predicted_draws
#
# Author: mjskay
###############################################################################



# deprecated names for [add_]predicted_draws ----------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
predicted_samples = function(model, newdata, var = "pred", ..., n = NULL) {
  .Deprecated("predicted_draws", package = "tidybayes") # nocov
  predicted_samples_(model, newdata, prediction = var, ..., n = n) # nocov
}
predicted_samples_ = function(model, newdata, var = "pred", ..., n = NULL) {
  combine_chains_for_deprecated_(predicted_draws( # nocov
    model, newdata, prediction = var, ..., n = n  # nocov
  ))                                              # nocov
}


#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
add_predicted_samples = function(newdata, model, ..., n = NULL) {
  .Deprecated("add_predicted_draws", package = "tidybayes") # nocov
  predicted_samples_(model, newdata, ..., n = n)         # nocov
}



# [add_]predicted_draws ---------------------------------------------------

#' Add draws from (possibly transformed) posterior linear predictors (the "fit") or from posterior predictions
#' of a model to a data frame
#'
#' Given a data frame and a model, adds draws from the posterior "fit" (aka the linear/link-level predictor)
#' or from the posterior predictions of the model to the data frame in a long format.
#'
#' \code{add_fitted_draws} adds draws from (possibly transformed) posterior linear predictors (or "link-level" predictors) to
#' the data. It corresponds to \code{\link[rstanarm]{posterior_linpred}} in \code{rstanarm} or
#' \code{\link[brms]{fitted.brmsfit}} in \code{brms}.
#'
#' \code{add_predicted_draws} adds draws from posterior predictions to
#' the data. It corresponds to \code{\link[rstanarm]{posterior_predict}} in \code{rstanarm} or
#' \code{\link[brms]{predict.brmsfit}} in \code{brms}.
#'
#' \code{add_fitted_draws} and \code{fitted_draws} are alternate spellings of the
#' same function with opposite order of the first two arguments to facilitate use in data
#' processing pipelines that start either with a data frame or a model. Similarly,
#' \code{add_predicted_draws} and \code{predicted_draws} are alternate spellings.
#'
#' Given equal choice between the two, \code{add_fitted_draws} and \code{add_predicted_draws}
#' are the preferred spellings.
#'
#' \code{add_linpred_draws} and \code{linpred_draws} are alternative spellings of \code{fitted_draws}
#' and \code{add_fitted_draws} for consistency with \code{rstanarm} terminology (specifically
#' \code{\link[rstanarm]{posterior_linpred}}).
#'
#' @param newdata Data frame to generate predictions from. If omitted, most model types will
#' generate predictions from the data used to fit the model.
#' @param model A supported Bayesian model fit that can provide fits and predictions. Supported models
#' are listed in the second section of \link{tidybayes-models}: \emph{Models Supporting Prediction}. While other
#' functions in this package (like \code{\link{spread_draws}}) support a wider range of models, to work with
#' \code{add_fitted_draws} and \code{add_predicted_draws} a model must provide an interface for generating
#' predictions, thus more generic Bayesian modeling interfaces like \code{runjags} and \code{rstan} are not directly
#' supported for these functions (only wrappers around those languages that provide predictions, like \code{rstanarm}
#' and \code{brm}, are supported here).
#' @param value The name of the output column for \code{fitted_draws}; default \code{".value"}.
#' @param prediction The name of the output column for \code{predicted_draws}; default \code{".prediction"}.
#' @param ... Additional arguments passed to the underlying prediction method for the type of
#' model given.
#' @param n The number of draws per prediction / fit to return, or \code{NULL} to return all draws.
#' @param seed A seed to use when subsampling draws (i.e. when \code{n} is not \code{NULL}).
#' @param re_formula formula containing group-level effects to be considered in the prediction.
#' If \code{NULL} (default), include all group-level effects; if \code{NA}, include no group-level effects.
#' Some model types (such as \code{\link[brms]{brm}} and \code{\link[rstanarm]{stanreg-objects}}) allow
#' marginalizing over grouping factors by specifying new levels of a factor in \code{newdata}. In the case of
#' \code{\link[brms]{brm}}, you must also pass \code{allow_new_levels = TRUE} here to include new levels (see
#' \code{\link[brms]{predict.brmsfit}}).
#' @param category For \emph{some} ordinal and multinomial models (notably, \code{\link[brms]{brm}} models but
#' \emph{not} \code{\link[rstanarm]{stan_polr}} models), multiple sets of rows will be returned per input row for
#' \code{fitted_draws}, one for each category. The \code{category} argument specifies the name of the column
#' to put the category names into in the resulting data frame (default: \code{".category"}). The fact that multiple
#' rows per response are returned only for some model types reflects the fact that tidybayes takes the approach of
#' tidying whatever output is given to us, and the output from different modeling functions differ on this point.
#' See \code{vignette("tidy-brms")} and \code{vignette("tidy-rstanarm")} for examples of dealing with output
#' from ordinal models using both approaches.
#' @param dpar For \code{fitted_draws} and \code{add_fitted_draws}: Should distributional regression
#' parameters be included in the output? Valid only for models that support distributional regression parameters,
#' such as submodels for variance parameters (as in \code{brm}). If \code{TRUE}, distributional regression
#' parameters are included in the output as additional columns named after each parameter
#' (alternative names can be provided using a list or named vector, e.g. \code{c(sigma.hat = "sigma")}
#' would output the \code{"sigma"} parameter from a model as a column named \code{"sigma.hat"}).
#' If \code{FALSE} (the default), distributional regression parameters are not included.
#' @param scale Either \code{"response"} or \code{"linear"}. If \code{"response"}, results are returned
#' on the scale of the response variable. If \code{"linear"}, fitted values are returned on the scale of
#' the linear predictor.
#' @return A data frame (actually, a \code{\link[tibble]{tibble}}) with a \code{.row} column (a
#' factor grouping rows from the input \code{newdata}), \code{.chain} column (the chain
#' each draw came from, or \code{NA} if the model does not provide chain information),
#' \code{.iteration} column (the iteration the draw came from, or \code{NA} if the model does
#' not provide iteration information), and a \code{.draw} column (a unique index corresponding to each draw
#' from the distribution). In addition, \code{fitted_draws} includes a column with its name specified by
#' the \code{value} argument (default is \code{.value}) containing draws from the (transformed) linear predictor,
#' and \code{predicted_draws} contains a \code{.prediction} column containing draws from the posterior predictive
#' distribution. For convenience, the resulting data frame comes grouped by the original input rows.
#' @author Matthew Kay
#' @seealso \code{\link{spread_draws}}
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(ggplot2)
#' library(dplyr)
#' library(rstanarm)
#' library(modelr)
#'
#' theme_set(theme_light())
#'
#' m_mpg = stan_glm(mpg ~ hp * cyl, data = mtcars,
#'   # 1 chain / few iterations just so example runs quickly
#'   # do not use in practice
#'   chains = 1, iter = 500)
#'
#' # draw 100 fit lines from the posterior and overplot them
#' mtcars %>%
#'   group_by(cyl) %>%
#'   data_grid(hp = seq_range(hp, n = 101)) %>%
#'   add_fitted_draws(m_mpg, n = 100) %>%
#'   ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
#'   geom_line(aes(y = .value, group = paste(cyl, .draw)), alpha = 0.25) +
#'   geom_point(data = mtcars)
#'
#' # plot posterior predictive intervals
#' mtcars %>%
#'   group_by(cyl) %>%
#'   data_grid(hp = seq_range(hp, n = 101)) %>%
#'   add_predicted_draws(m_mpg) %>%
#'   ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
#'   stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), alpha = 0.25) +
#'   geom_point(data = mtcars) +
#'   scale_fill_brewer(palette = "Greys")
#'
#' }
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom dplyr mutate sample_n ungroup group_by
#' @importFrom stats fitted predict
#' @export
add_predicted_draws = function(newdata, model, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL) {
  predicted_draws(model, newdata, prediction, ..., n = n, seed = seed, re_formula = re_formula)
}

#' @rdname add_predicted_draws
#' @export
predicted_draws = function(model, newdata, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL) {
  UseMethod("predicted_draws")
}

#' @rdname add_predicted_draws
#' @export
predicted_draws.default = function(model, newdata, ...) {
  stop(paste0("Models of type ", deparse0(class(model)), " are not currently supported by `predicted_draws`"))
}

#' @rdname add_predicted_draws
#' @export
predicted_draws.stanreg = function(model, newdata, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is needed for `predicted_draws` to support `stanreg` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]predicted_draws", n = "draws", re_formula = "re.form"
  )

  fitted_predicted_draws_brmsfit_(rstanarm::posterior_predict, model, newdata, output_name = prediction, ...,
    draws = n, seed = seed, re.form = re_formula, is_brms = FALSE
  )
}

#' @rdname add_predicted_draws
#' @export
predicted_draws.brmsfit = function(model, newdata, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `predicted_draws` to support `brmsfit` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]predicted_draws", n = "nsamples"
  )

  fitted_predicted_draws_brmsfit_(predict, model, newdata, output_name = prediction, ...,
    nsamples = n, seed = seed, re_formula = re_formula
  )
}


#' @importFrom arrayhelpers array2df ndim
fitted_predicted_draws_brmsfit_ = function(f_fitted_predicted, model, newdata, output_name, category, ...,
  seed = NULL, is_brms = TRUE, summary = NULL #summary is ignored, we change it ourselves
) {
  newdata %<>% ungroup()

  column_format = list(
    .draw = NA,        #NA here means numeric
    .row = NA
  )

  if (!is.null(seed)) set.seed(seed)
  fits_preds <- if (is_brms) {
    # only brms has/needs the summary argument
    f_fitted_predicted(model, newdata = newdata, summary = FALSE, ...)
  } else {
    f_fitted_predicted(model, newdata = newdata, ...)
  }

  groups = union(colnames(newdata), ".row")

  if (ndim(fits_preds) == 3) {
    #3 dimensions implies a categorical outcome, add a column for it
    # N.B.: at some point getting category names to work would be nice, but may be kind of brittle
    column_format[[3]] = NA
    names(column_format)[[3]] = category
    groups %<>% union(category)
  }

  fits_preds_df = array2df(fits_preds, column_format, label.x = output_name)

  #rstanarm does something weird that prevents array2df from properly seeing .row and .draw as numerics,
  #so we have to convert them manually from factors. While we're at it, we should also make sure they are integers.
  if (is.factor(fits_preds_df$.row)) {
    fits_preds_df$.row = as.character(fits_preds_df$.row)
  }
  fits_preds_df$.row = as.integer(fits_preds_df$.row)

  if (is.factor(fits_preds_df$.draw)) {
    fits_preds_df$.draw = as.character(fits_preds_df$.draw)
  }
  fits_preds_df$.draw = as.integer(fits_preds_df$.draw)

  if (ndim(fits_preds) == 3) {
    #3 dimensions implies a categorical outcome -> make category column be factor
    fits_preds_df[[category]] = factor(fits_preds_df[[category]])
  }

  newdata %>%
    mutate(
      .row = seq_len(n()),
      .chain = as.integer(NA),
      .iteration = as.integer(NA)
    ) %>%
    inner_join(fits_preds_df, by = ".row") %>%
    select(-!!sym(output_name), !!sym(output_name)) %>%
    group_by(!!!syms(groups))
}
