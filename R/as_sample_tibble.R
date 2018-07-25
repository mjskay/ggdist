# as_sample_tibble
#
# Author: mjskay
###############################################################################


#' Get a sample of posterior draws from a model as a tibble
#'
#' Extract draws from a Bayesian fit into a wide-format data frame with a
#' \code{.chain}, \code{.iteration}, and \code{.draw} column, as well as all variables
#' as columns. Generally speaking not as useful as \code{\link{spread_draws}} or
#' \code{\link{gather_draws}} and is mainly used interally (see `Details`)
#'
#' You will not typically call this directly; instead use \code{\link{spread_draws}} or \code{\link{gather_draws}}.
#' However, to provide support for new models in those functions,
#' you must provide an implementation of this function, \emph{or} an implementaiton
#' of \code{\link[coda]{as.mcmc.list}} (\code{as_sample_tibble} should work on any model
#' with an implementation of \code{\link[coda]{as.mcmc.list}})
#'
#' \code{as_sample_data_frame} is an alias.
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
#'   as_sample_tibble()
#'
#' @aliases as_sample_data_frame
#' @importFrom purrr map_df
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble tibble
#' @importFrom coda as.mcmc.list as.mcmc
#' @importFrom stats coef vcov
#' @importFrom MASS mvrnorm
#' @export
as_sample_tibble = function(model) UseMethod("as_sample_tibble")

#' @rdname as_sample_tibble
#' @export
as_sample_data_frame = as_sample_tibble

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.default = function(model) {
  as_sample_tibble(as.mcmc.list(model))
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.mcmc.list = function(model) {
  map_dfr(seq_along(model), function(chain) {
    #putting tibble() or as_tibble() in here makes this slower, so we put it outside
    #after all the chains have been combined
    n = nrow(model[[chain]])
    iteration = seq_len(n)
    data.frame(
      .chain = chain,
      .iteration = iteration,
      .draw = draw_from_chain_and_iteration(chain, iteration),
      # the implementation of as.data.frame for mcmc objects takes ~ twice as long as as.matrix (!!),
      # so using as.matrix here speeds things up considerably for large samples
      as.matrix(model[[chain]]),

      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }) %>%
    as_tibble()
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.stanfit = function(model) {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("The `rstan` package is needed for `as_sample_tibble` to support `stanfit` objects.", call. = FALSE) # nocov
  }
  as_sample_tibble(rstan::As.mcmc.list(model))
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.stanreg = function(model) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("The `rstanarm` package is needed for `as_sample_tibble` to support `stanreg` objects.", call. = FALSE) # nocov
  }
  #stanreg objects have more info provided for variable names than the underlying stanfit,
  #so we dont' just do as_sample_tibble(model$stanfit)
  sample_matrix = as.array(model) #[iteration, chain, variable]
  n_chain = dim(sample_matrix)[[2]]
  mcmc_list = as.mcmc.list(lapply(seq_len(n_chain), function(chain) as.mcmc(sample_matrix[, chain, ]))) # nolint
  as_sample_tibble(mcmc_list)
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.runjags = function(model) {
  if (!requireNamespace("runjags", quietly = TRUE)) {
    stop("The `runjags` package is needed for `as_sample_tibble` to support `runjags` objects.", call. = FALSE) # nocov
  }
  as_sample_tibble(as.mcmc.list(model)) # nolint
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.jagsUI = function(model) {
  if (!requireNamespace("jagsUI", quietly = TRUE)) {
    stop("The `jagsUI` package is needed for `as_sample_tibble` to support `jagsUI` objects.", call. = FALSE) # nocov
  }
  as_sample_tibble(model$samples)
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.brmsfit = function(model) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `as_sample_tibble` to support `brmsfit` objects.", call. = FALSE) # nocov
  }
  as_sample_tibble(brms::as.mcmc(model))
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.matrix = function(model) {
  # assume matrix indexed by [iterations, variables]
  as_sample_tibble(as.mcmc.list(as.mcmc(model))) # nolint
}

#' @rdname as_sample_tibble
#' @importFrom dplyr inner_join
#' @importFrom purrr discard
#' @export
as_sample_tibble.MCMCglmm = function(model) {
  # draws from MME solutions, including fixed effects
  sol_draws = as_sample_tibble(model$Sol)

  # draws from (co)variance matrices, ordinal cutpoints, and latent variables
  other_draws =
    model[c("VCV", "CP", "Liab")] %>%
    discard(is.null) %>%
    map2(names(.), function(draws, param_type) {
      dimnames(draws)[[2]] %<>% paste0(param_type, "_", .)
      as_sample_tibble(draws)
    })

  sol_draws %>%
    list() %>%
    c(other_draws) %>%
    reduce(inner_join, by = c(".chain", ".iteration", ".draw"))
}



# utility functions for as_sample_tibble ----------------------------------

draw_from_chain_and_iteration = function(chain, iteration) {
  max_iteration = max(iteration)
  as.integer(ifelse(is.na(chain), 0, chain - 1) * max_iteration + iteration)
}
