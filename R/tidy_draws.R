# tidy_draws
#
# Author: mjskay
###############################################################################



# deprecated names for tidy_draws --------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
as_sample_tibble = function(...) {
  .Deprecated("tidy_draws", package = "tidybayes") # nocov
  tidy_draws(...)                                  # nocov
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
as_sample_data_frame = function(...) {
  .Deprecated("tidy_draws", package = "tidybayes") # nocov
  tidy_draws(...)                                  # nocov
}



#' Get a sample of posterior draws from a model as a tibble
#'
#' Extract draws from a Bayesian fit into a wide-format data frame with a
#' \code{.chain}, \code{.iteration}, and \code{.draw} column, as well as all variables
#' as columns. While this function can be useful for quick glances at models (especially
#' combined with \code{\link{gather_variables}} and \code{\link{median_qi}}), it is
#' generally speaking not as useful as \code{\link{spread_draws}} or
#' \code{\link{gather_draws}} for most applications, and is mainly used internally (see `Details`).
#'
#' In practice, apart from quick looks at a model you will probably not call this directly;
#' \code{\link{spread_draws}} or \code{\link{gather_draws}}, which are build on top of this
#' function, provide support for extracting variable dimensions are so are often more useful.
#'
#' To provide support for new models in tidybayes,
#' you must provide an implementation of this function \emph{or} an implementation
#' of \code{\link[coda]{as.mcmc.list}} (\code{tidy_draws} should work on any model
#' with an implementation of \code{\link[coda]{as.mcmc.list}})
#'
#' \code{tidy_draws} can be applied to a data frame that is already a tidy-format data frame
#' of draws, provided it has one row per draw. In other words, it can be applied to data frames
#' that have the same format it returns, and it will return the same data frame back, while
#' checking to ensure the \code{.chain}, \code{.iteration}, and \code{.draw} columns are all
#' integers (converting if possible) and that the \code{.draw} column is unique. This allows
#' you to pass already-tidy-format data frames into other tidybayes functions, like
#' \code{\link{spread_draws}} or \code{\link{gather_draws}}.
#'
#' @param model A supported Bayesian model fit object. See \code{\link{tidybayes-models}} for a list of supported
#' models.
#' @return A data frame (actually, a \code{\link[tibble]{tibble}}) with a \code{.chain} column,
#' \code{.iteration} column, \code{.draw} column, and one column for every variable in \code{model}.
#' @author Matthew Kay
#' @seealso \code{\link{spread_draws}} or \code{\link{gather_draws}}, which use this function
#' internally and provides a friendly interface for extracting tidy data frames from model fits.
#' @keywords manip
#' @examples
#'
#' library(magrittr)
#'
#' data(line, package = "coda")
#'
#' line %>%
#'   tidy_draws()
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble tibble
#' @importFrom coda as.mcmc.list as.mcmc
#' @export
tidy_draws = function(model) UseMethod("tidy_draws")

#' @rdname tidy_draws
#' @export
tidy_draws.default = function(model) {
  tidy_draws(as.mcmc.list(model))
}

#' @rdname tidy_draws
#' @export
tidy_draws.data.frame = function(model) {
  stop_message = paste0(
    "To use a data frame directly with `tidy_draws()`, it must already be a\n",
    "  tidy-format data frame of draws: it must have integer-like `.chain`\n",
    "  `.iteration`, and `.draw` columns with one row per draw.\n",
    "\n"
  )

  # iterate over draw index columns to check they are integers, recording if they passed the check
  # (and cleaning as necessary)
  check_cols = c(".chain", ".iteration", ".draw")
  passed = sapply(check_cols, function(col) {
    col_value = model[[col]]

    if (is.null(col_value)) return(FALSE)
    if (is.integer(col_value)) return(TRUE)
    if (is.logical(col_value) || is_integerish(col_value)) {
      model[[col]] <<- as.integer(col_value)
      return(TRUE)
    }

    # if we make it this far, the column did not pass all checks
    FALSE
  })

  failed_cols = check_cols[!passed]
  if (length(failed_cols) > 0) {
    stop(stop_message,
      "  The following columns are not integer-like (or are missing):\n",
      "    ", paste0("`", failed_cols, "`", collapse = ", ")
    )
  }

  if (length(unique(model[[".draw"]])) != nrow(model)) {
    stop(stop_message,
      "  The `.draw` column in the input data frame has more than one row per draw\n",
      "  (its values are not unique)."
    )
  }

  model
}

#' @rdname tidy_draws
#' @export
tidy_draws.mcmc.list = function(model) {
  map_dfr(seq_along(model), function(chain) {
    #putting tibble() or as_tibble() in here makes this slower, so we put it outside
    #after all the chains have been combined
    n = nrow(model[[chain]])
    iteration = seq_len(n)
    data.frame(
      .chain = chain,
      .iteration = iteration,
      .draw = draw_from_chain_and_iteration_(chain, iteration),
      # the implementation of as.data.frame for mcmc objects takes ~ twice as long as as.matrix (!!),
      # so using as.matrix here speeds things up considerably for large samples
      as.matrix(model[[chain]]),

      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }) %>%
    as_tibble()
}

#' @rdname tidy_draws
#' @export
tidy_draws.stanfit = function(model) {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("The `rstan` package is needed for `tidy_draws` to support `stanfit` objects.", call. = FALSE) # nocov
  }

  parameter_draws = tidy_draws(rstan::As.mcmc.list(model))

  diagnostics_draws = map_dfr(rstan::get_sampler_params(model, inc_warmup = FALSE), as.data.frame)

  bind_cols(parameter_draws, diagnostics_draws)
}

#' @rdname tidy_draws
#' @export
tidy_draws.stanreg = function(model) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is needed for `tidy_draws` to support `stanreg` objects.", call. = FALSE) # nocov
  }
  #stanreg objects have more info provided for variable names than the underlying stanfit,
  #so we dont' just do tidy_draws(model$stanfit)
  sample_matrix = as.array(model) #[iteration, chain, variable]
  n_chain = dim(sample_matrix)[[2]]
  mcmc_list = as.mcmc.list(lapply(seq_len(n_chain), function(chain) as.mcmc(sample_matrix[, chain, ]))) # nolint
  parameter_draws = tidy_draws(mcmc_list)

  diagnostics_draws = map_dfr(rstan::get_sampler_params(model$stanfit, inc_warmup = FALSE), as.data.frame)

  bind_cols(parameter_draws, diagnostics_draws)
}

#' @rdname tidy_draws
#' @export
tidy_draws.runjags = function(model) {
  if (!requireNamespace("runjags", quietly = TRUE)) {
    stop("The `runjags` package is needed for `tidy_draws` to support `runjags` objects.", call. = FALSE) # nocov
  }
  tidy_draws(as.mcmc.list(model)) # nolint
}

#' @rdname tidy_draws
#' @export
tidy_draws.jagsUI = function(model) {
  if (!requireNamespace("jagsUI", quietly = TRUE)) {
    stop("The `jagsUI` package is needed for `tidy_draws` to support `jagsUI` objects.", call. = FALSE) # nocov
  }
  tidy_draws(model$samples)
}

#' @rdname tidy_draws
#' @export
tidy_draws.brmsfit = function(model) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `tidy_draws` to support `brmsfit` objects.", call. = FALSE) # nocov
  }

  parameter_draws = tidy_draws(brms::as.mcmc(model))

  diagnostics_draws = map_dfr(rstan::get_sampler_params(model$fit, inc_warmup = FALSE), as.data.frame)

  bind_cols(parameter_draws, diagnostics_draws)
}

#' @rdname tidy_draws
#' @export
tidy_draws.matrix = function(model) {
  # assume matrix indexed by [iterations, variables]
  tidy_draws(as.mcmc.list(as.mcmc(model))) # nolint
}

#' @rdname tidy_draws
#' @importFrom dplyr inner_join
#' @importFrom purrr discard
#' @export
tidy_draws.MCMCglmm = function(model) {
  # draws from MME solutions, including fixed effects
  sol_draws = tidy_draws(model$Sol)

  # draws from (co)variance matrices, ordinal cutpoints, and latent variables
  other_draws =
    model[c("VCV", "CP", "Liab")] %>%
    discard(is.null) %>%
    map2(names(.), function(draws, param_type) {
      dimnames(draws)[[2]] %<>% paste0(param_type, "_", .)
      tidy_draws(draws)
    })

  sol_draws %>%
    list() %>%
    c(other_draws) %>%
    reduce(inner_join, by = c(".chain", ".iteration", ".draw"))
}
