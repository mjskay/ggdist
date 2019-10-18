# [add_]predicted_draws
#
# Author: mjskay
###############################################################################



# deprecated names for [add_]predicted_draws ----------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
predicted_samples = function(model, newdata, ..., n = NULL) {
  .Deprecated("predicted_draws", package = "tidybayes") # nocov
  predicted_samples_(model, newdata, ..., n = n) # nocov
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

#' Add draws from the posterior fit, predictions, or residuals of a model to a data frame
#'
#' Given a data frame and a model, adds draws from the (possibly transformed) posterior "fit" (aka the
#' linear/link-level predictor), the posterior predictions of the model, or the residuals of a model to
#' the data frame in a long format.
#'
#' `add_fitted_draws` adds draws from (possibly transformed) posterior linear predictors (or "link-level" predictors) to
#' the data. It corresponds to [rstanarm::posterior_linpred()] in `rstanarm` or
#' [brms::fitted.brmsfit()] in `brms`.
#'
#' `add_predicted_draws` adds draws from posterior predictions to
#' the data. It corresponds to [rstanarm::posterior_predict()] in `rstanarm` or
#' [brms::predict.brmsfit()] in `brms`.
#'
#' `add_fitted_draws` and `fitted_draws` are alternate spellings of the
#' same function with opposite order of the first two arguments to facilitate use in data
#' processing pipelines that start either with a data frame or a model. Similarly,
#' `add_predicted_draws` and `predicted_draws` are alternate spellings.
#'
#' Given equal choice between the two, `add_fitted_draws` and `add_predicted_draws`
#' are the preferred spellings.
#'
#' `add_linpred_draws` and `linpred_draws` are alternative spellings of `fitted_draws`
#' and `add_fitted_draws` for consistency with `rstanarm` terminology (specifically
#' [rstanarm::posterior_linpred()]).
#'
#' @param newdata Data frame to generate predictions from. If omitted, most model types will
#' generate predictions from the data used to fit the model.
#' @param model A supported Bayesian model fit that can provide fits and predictions. Supported models
#' are listed in the second section of [tidybayes-models]: *Models Supporting Prediction*. While other
#' functions in this package (like [spread_draws()]) support a wider range of models, to work with
#' `add_fitted_draws` and `add_predicted_draws` a model must provide an interface for generating
#' predictions, thus more generic Bayesian modeling interfaces like `runjags` and `rstan` are not directly
#' supported for these functions (only wrappers around those languages that provide predictions, like `rstanarm`
#' and `brm`, are supported here).
#' @param value The name of the output column for `fitted_draws`; default `".value"`.
#' @param prediction The name of the output column for `predicted_draws`; default `".prediction"`.
#' @param residual The name of the output column for `residual_draws`; default `".residual"`.
#' @param ... Additional arguments passed to the underlying prediction method for the type of
#' model given.
#' @param n The number of draws per prediction / fit to return, or `NULL` to return all draws.
#' @param seed A seed to use when subsampling draws (i.e. when `n` is not `NULL`).
#' @param re_formula formula containing group-level effects to be considered in the prediction.
#' If `NULL` (default), include all group-level effects; if `NA`, include no group-level effects.
#' Some model types (such as [brms::brmsfit] and [rstanarm::stanreg-objects]) allow
#' marginalizing over grouping factors by specifying new levels of a factor in `newdata`. In the case of
#' [brms::brm()], you must also pass `allow_new_levels = TRUE` here to include new levels (see
#' [brms::predict.brmsfit()]).
#' @param category For *some* ordinal, multinomial, and multivariate models (notably, [brms::brm()] models but
#' *not* [rstanarm::stan_polr()] models), multiple sets of rows will be returned per input row for
#' `fitted_draws` or `predicted_draws`, depending on the model type. For ordinal/multinomial models,
#' these rows correspond to different categories of the response variable. For multivariate models, these correspond to
#' different response variables. The `category` argument specifies the name of the column
#' to put the category names (or variable names) into in the resulting data frame. The default name of this column
#' (`".category"`) reflects the fact that this functionality was originally used only for ordinal models and
#' has been re-used for multivariate models. The fact that multiple rows per response are returned only for some
#' model types reflects the fact that tidybayes takes the approach of tidying whatever output is given to us, and
#' the output from different modeling functions differs on this point.
#' See `vignette("tidy-brms")` and `vignette("tidy-rstanarm")` for examples of dealing with output
#' from ordinal models using both approaches.
#' @param dpar For `fitted_draws` and `add_fitted_draws`: Should distributional regression
#' parameters be included in the output? Valid only for models that support distributional regression parameters,
#' such as submodels for variance parameters (as in `brm`). If `TRUE`, distributional regression
#' parameters are included in the output as additional columns named after each parameter
#' (alternative names can be provided using a list or named vector, e.g. `c(sigma.hat = "sigma")`
#' would output the `"sigma"` parameter from a model as a column named `"sigma.hat"`).
#' If `FALSE` (the default), distributional regression parameters are not included.
#' @param scale Either `"response"` or `"linear"`. If `"response"`, results are returned
#' on the scale of the response variable. If `"linear"`, fitted values are returned on the scale of
#' the linear predictor.
#' @return A data frame (actually, a [tibble][tibble::tibble]) with a `.row` column (a
#' factor grouping rows from the input `newdata`), `.chain` column (the chain
#' each draw came from, or `NA` if the model does not provide chain information),
#' `.iteration` column (the iteration the draw came from, or `NA` if the model does
#' not provide iteration information), and a `.draw` column (a unique index corresponding to each draw
#' from the distribution). In addition, `fitted_draws` includes a column with its name specified by
#' the `value` argument (default is `.value`) containing draws from the (transformed) linear predictor,
#' and `predicted_draws` contains a `.prediction` column containing draws from the posterior predictive
#' distribution. For convenience, the resulting data frame comes grouped by the original input rows.
#' @author Matthew Kay
#' @seealso [add_draws()] for the variant of these functions for use with packages that do not have
#' explicit support for these functions yet. See [spread_draws()] for manipulating posteriors directly.
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' if (
#'   require("rstanarm", quietly = TRUE) &&
#'   require("modelr", quietly = TRUE)
#' ) {
#'
#'   theme_set(theme_light())
#'
#'   m_mpg = stan_glm(mpg ~ hp * cyl, data = mtcars,
#'     # 1 chain / few iterations just so example runs quickly
#'     # do not use in practice
#'     chains = 1, iter = 500)
#'
#'   # draw 100 fit lines from the posterior and overplot them
#'   mtcars %>%
#'     group_by(cyl) %>%
#'     data_grid(hp = seq_range(hp, n = 101)) %>%
#'     add_fitted_draws(m_mpg, n = 100) %>%
#'     ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
#'     geom_line(aes(y = .value, group = paste(cyl, .draw)), alpha = 0.25) +
#'     geom_point(data = mtcars)
#'
#'   # plot posterior predictive intervals
#'   mtcars %>%
#'     group_by(cyl) %>%
#'     data_grid(hp = seq_range(hp, n = 101)) %>%
#'     add_predicted_draws(m_mpg) %>%
#'     ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
#'     stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), alpha = 0.25) +
#'     geom_point(data = mtcars) +
#'     scale_fill_brewer(palette = "Greys")
#' }
#' }
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom dplyr mutate sample_n ungroup group_by
#' @importFrom stats fitted predict
#' @importFrom rlang is_integerish
#' @export
add_predicted_draws = function(newdata, model, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL, category = ".category") {
  predicted_draws(model, newdata, prediction, ..., n = n, seed = seed, re_formula = re_formula, category = category)
}

#' @rdname add_predicted_draws
#' @export
predicted_draws = function(model, newdata, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL, category = ".category") {
  UseMethod("predicted_draws")
}

#' @rdname add_predicted_draws
#' @export
predicted_draws.default = function(model, newdata, ...) {
  stop(
    "Models of type ", deparse0(class(model)), " are not currently supported by `predicted_draws`.\n",
    "You might try using `add_draws()` for models that do not have explicit fit/prediction\n",
    "support; see help(\"add_draws\") for an example. See also help(\"tidybayes-models\") for\n",
    "more information on what functions are supported by what model types."
  )
}

#' @rdname add_predicted_draws
#' @export
predicted_draws.stanreg = function(model, newdata, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL, category = ".category") {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is needed for `predicted_draws` to support `stanreg` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]predicted_draws", n = "draws", re_formula = "re.form"
  )

  fitted_predicted_draws_brmsfit_(rstanarm::posterior_predict, model, newdata, output_name = prediction, ...,
    draws = n, seed = seed, re.form = re_formula, category = category, is_brms = FALSE
  )
}

#' @rdname add_predicted_draws
#' @export
predicted_draws.brmsfit = function(model, newdata, prediction = ".prediction", ..., n = NULL, seed = NULL, re_formula = NULL, category = ".category") {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `predicted_draws` to support `brmsfit` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]predicted_draws", n = "nsamples"
  )

  fitted_predicted_draws_brmsfit_(predict, model, newdata, output_name = prediction, ...,
    nsamples = n, seed = seed, re_formula = re_formula, category = category
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
  fits_preds = if (is_brms) {
    # only brms has/needs the summary argument
    f_fitted_predicted(model, newdata = newdata, summary = FALSE, ...)
  } else {
    f_fitted_predicted(model, newdata = newdata, ...)
  }

  groups = union(colnames(newdata), ".row")

  if (ndim(fits_preds) == 3) {
    #3 dimensions implies a categorical outcome, add a column for it
    # N.B.: at some point getting category names to work would be nice, but may be kind of brittle
    column_format[[3]] = TRUE
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

  #for predictions from categorical models in brms, we can use the "levels" attribute
  #to recover the original factor levels. But we must be careful: dirichlet models also
  #get this attribute set, so we must also test that responses are all positive integer values.
  prediction_levels = attr(fits_preds, "levels", exact = TRUE)
  if (!is.null(prediction_levels) &
      is_integerish(fits_preds_df[[output_name]]) &
      all(fits_preds_df[[output_name]] >= 1)
    ) {
    fits_preds_df[[output_name]] = factor(
      fits_preds_df[[output_name]],
      levels = seq_along(prediction_levels),
      labels = prediction_levels
    )
  }

  newdata %>%
    mutate(
      .row = seq_len(n()),
      .chain = NA_integer_,
      .iteration = NA_integer_
    ) %>%
    inner_join(fits_preds_df, by = ".row") %>%
    select(-!!sym(output_name), !!sym(output_name)) %>%
    group_by_at(groups)
}
