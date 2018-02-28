# as_sample_tibble
#
# Author: mjskay
###############################################################################


#' Get samples from a model as a tibble
#'
#' Extract samples from an MCMC fit into a wide-format data frame with a
#' \code{.chain} and \code{.iteration} column, as well as all terms
#' as columns. Generally speaking not as useful as \code{\link{spread_samples}} or
#' \code{\link{gather_samples}} and is mainly used interally (see `Details`)
#'
#' You will not typically call this directly; instead use \code{\link{spread_samples}} or \code{\link{gather_samples}}.
#' However, to provide support for new models in those functions,
#' you must provide an implementation of this function, \emph{or} an implementaiton
#' of \code{\link[coda]{as.mcmc.list}} (\code{as_sample_tibble} should work on any model
#' with an implementation of \code{\link[coda]{as.mcmc.list}})
#'
#' \code{as_sample_data_frame} is an alias.
#'
#' @param model A supported Bayesian model fit / MCMC object. Currently
#' supported models include \code{\link[coda]{mcmc}}, \code{\link[coda]{mcmc.list}},
#' \code{\link[runjags]{runjags}}, \code{\link[rstan]{stanfit}}, \code{\link[rstanarm]{stanreg-objects}},
#' \code{\link[brms]{brm}}, and anything with its own \code{\link[coda]{as.mcmc.list}} implementation.
#' If you install the \code{tidybayes.rethinking} package (available at
#' \url{https://github.com/mjskay/tidybayes.rethinking}), \code{map} and
#' \code{map2stan} models from the \code{rethinking} package are also supported.
#' @return A data frame (actually, a \code{\link[tibble]{tibble}}) with a \code{.chain} column,
#' \code{.iteration} column, and one column for every parameter in \code{model}.
#' @author Matthew Kay
#' @seealso \code{\link{spread_samples}} or \code{\link{gather_samples}}, which use this function
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
  as_sample_tibble(as.mcmc.list(model))  # nolint
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.mcmc.list = function(model) {
  n = nrow(model[[1]])
  map_dfr(seq_along(model), function(chain)
    #putting tibble() or as_tibble() in here makes this slower, so we put it outside
    #after all the chains have been combined
    data.frame(
      .chain = chain,
      .iteration = seq_len(n),
      # the implementation of as.data.frame for mcmc objects takes ~ twice as long as as.matrix (!!),
      # so using as.matrix here speeds things up considerably for large samples
      as.matrix(model[[chain]]),

      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  ) %>%
    as_tibble()
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.stanfit = function(model) {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("The `rstan` package is needed for `as_sample_tibble` to support `stanfit` objects.", call. = FALSE)
  }
  as_sample_tibble(rstan::As.mcmc.list(model))
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.stanreg = function(model) {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("The `rstan` package is needed for `as_sample_tibble` to support `stanreg` objects.", call. = FALSE)
  }
  #stanreg objects have more info provided for parameter names than the underlying stanfit,
  #so we dont' just do as_sample_tibble(model$stanfit)
  sample_matrix = as.array(model) #[iteration, chain, parameter]
  n_chain = dim(sample_matrix)[[2]]
  mcmc_list = as.mcmc.list(lapply(seq_len(n_chain), function(chain) as.mcmc(sample_matrix[, chain, ]))) # nolint
  as_sample_tibble(mcmc_list)
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.runjags = function(model) {
  if (!requireNamespace("runjags", quietly = TRUE)) {
    stop("The `runjags` package is needed for `as_sample_tibble` to support `runjags` objects.", call. = FALSE)
  }
  as_sample_tibble(as.mcmc.list(model)) # nolint
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.brmsfit = function(model) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("The `brms` package is needed for `as_sample_tibble` to support `brmsfit` objects.", call. = FALSE)
  }
  as_sample_tibble(brms::as.mcmc(model))
}

#' @rdname as_sample_tibble
#' @export
as_sample_tibble.matrix = function(model) {
  if (length(dim(model)) == 2) {
    # assume matrix indexed by [interations, variables]
    as_sample_tibble(as.mcmc.list(as.mcmc(model))) # nolint
  } else {
    stop("Matrix must have only 2 dimensions (first being the sample, second the variable).")
  }
}

#' @rdname as_sample_tibble
#' @importFrom dplyr inner_join
#' @importFrom purrr discard
#' @export
as_sample_tibble.MCMCglmm = function(model) {
  # samples from MME solutions, including fixed effects
  sol_samples = as_sample_tibble(model$Sol)

  # samples from (co)variance matrices, ordinal cutpoints, and latent variables
  other_samples =
    model[c("VCV", "CP", "Liab")] %>%
    discard(is.null) %>%
    map2(names(.), function(samples, param_type) {
      dimnames(samples)[[2]] %<>% paste0(param_type, "_", .)
      as_sample_tibble(samples)
    })

  sol_samples %>%
    list() %>%
    c(other_samples) %>%
    reduce(inner_join, by = c(".chain", ".iteration"))
}
