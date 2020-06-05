# [add_]fitted_draws
#
# Author: mjskay
###############################################################################


# [add_]fitted_draws / linpred_draws aliases -------------------------------------------------

#' @rdname add_predicted_draws
#' @export
add_fitted_draws = function(newdata, model, value = ".value", ..., n = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = FALSE, scale = c("response", "linear")
) {
  fitted_draws(model, newdata, value, ..., n = n, seed = seed, re_formula = re_formula,
    category = category, dpar = dpar, scale = scale)
}

#' @rdname add_predicted_draws
#' @export
fitted_draws = function(model, newdata, value = ".value", ..., n = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = FALSE, scale = c("response", "linear")
) {
  UseMethod("fitted_draws")
}

#' @rdname add_predicted_draws
#' @export
add_linpred_draws = function(newdata, model, value = ".value", ..., n = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = FALSE, scale = c("response", "linear")
) {
  fitted_draws(model, newdata, value, ..., n = n, seed = seed, re_formula = re_formula,
    category = category, dpar = dpar, scale = scale)
}

#' @rdname add_predicted_draws
#' @export
linpred_draws = function(model, newdata, value = ".value", ..., n = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = FALSE, scale = c("response", "linear")
) {
  fitted_draws(model, newdata, value, ..., n = n, seed = seed, re_formula = re_formula,
    category = category, dpar = dpar, scale = scale)
}

# fitted_draws generics -------------------------------------------------

#' @rdname add_predicted_draws
#' @export
fitted_draws.default = function(model, newdata, ...) {
  model_class = class(model)

  if (model_class %in% c("ulam", "quap", "map", "map2stan")) {
    stop(
      "Models of type ", deparse0(model_class), " are not supported by base tidybayes.\n",
      "Install the `tidybayes.rethinking` package to enable support for these models:\n",
      "  devtools::install_github('mjskay/tidybayes.rethinking')"
    )
  }
  stop(
    "Models of type ", deparse0(model_class), " are not currently supported by `fitted_draws`.\n",
    "You might try using `add_draws()` for models that do not have explicit fit/prediction\n",
    "support; see help(\"add_draws\") for an example. See also help(\"tidybayes-models\") for\n",
    "more information on what functions are supported by what model types."
  )
}

#' @rdname add_predicted_draws
#' @export
fitted_draws.stanreg = function(model, newdata, value = ".value", ..., n = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = FALSE, scale = c("response", "linear")
) {
  transform = match.arg(scale) == "response"

  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is needed for `fitted_draws` to support `stanreg` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]fitted_draws", re_formula = "re.form", scale = "transform"
  )

  draws = fitted_predicted_draws_brmsfit_(rstanarm::posterior_linpred, model, newdata, output_name = value, ...,
    seed = seed, category = category, re.form = re_formula, transform = transform, is_brms = FALSE
  )
  # posterior_linpred, unlike posterior_predict, does not have a "draws" argument for some reason
  if (!is.null(n)) {
    if (!is.null(seed)) set.seed(seed)
    draw_subset = sample(unique(draws$.draw), n)
    draws[draws[[".draw"]] %in% draw_subset,]
  } else {
    draws
  }
}

#' @rdname add_predicted_draws
#' @importFrom rlang is_true is_false is_empty
#' @importFrom purrr map
#' @export
fitted_draws.brmsfit = function(model, newdata, value = ".value", ..., n = NULL, seed = NULL, re_formula = NULL,
  category = ".category", dpar = FALSE, scale = c("response", "linear")
) {
  scale = match.arg(scale)

  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `fitted_draws` to support `brmsfit` objects.", call. = FALSE) # nocov
  }

  stop_on_non_generic_arg_(
    names(enquos(...)), "[add_]fitted_draws", n = "nsamples", dpar = "dpars"
  )

  # get the names of distributional regression parameters to include
  dpars = if (is_true(dpar)) {
    names(brms::brmsterms(model$formula)$dpar)
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


  # get the draws for the primary parameter first so we can stick the other values onto it
  draws = fitted_predicted_draws_brmsfit_(
    fitted, model, newdata, output_name = value, ...,
    category = category, re_formula = re_formula, dpar = NULL, scale = scale
  )


  for (i in seq_along(dpars)) {
    varname = names(dpars)[[i]]
    dpar_fitted_draws = fitted_predicted_draws_brmsfit_(
      fitted, model, newdata, output_name = ".value", ...,
      category = category, re_formula = re_formula, dpar = dpars[[i]], scale = scale
    )

    if (nrow(dpar_fitted_draws) == nrow(draws)) {
      draws[[varname]] = dpar_fitted_draws[[".value"]]
    } else {
      # in some models (such as ordinal models) the tidy draws from the dpars can have a different number
      # of rows than the linear predictor does if the linear predictor is on the response scale and the dpars are not.
      # In this case, we have to do a join to line things up (and in particular, a left join so that
      # rows from the linear predictor data frame are not dropped).
      join_cols = names(draws) %>%
        intersect(c(".row", ".draw", category)) %>%
        intersect(names(dpar_fitted_draws))

      dpar_fitted_draws %<>%
        ungroup() %>%
        select_at(c(join_cols, ".value")) %>%
        rename(!!varname := ".value")

      draws %<>% left_join(dpar_fitted_draws, by = join_cols)

      # stop(
      #   'Different number of rows in fitted draws for dpar "', dpars[[i]], '" and the linear predictor. This\n',
      #   'can happen in ordinal and categorical models when scale = "response". Try scale = "linear" instead.'
      # )
    }
  }

  # posterior_linpred, unlike posterior_predict, does not have a "draws" argument for some reason
  if (!is.null(n)) {
    if (!is.null(seed)) set.seed(seed)
    draw_subset = sample(unique(draws$.draw), n)
    draws[draws[[".draw"]] %in% draw_subset,]
  } else {
    draws
  }
}
