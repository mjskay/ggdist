# as_sample_tibble
# 
# Author: mjskay
###############################################################################


#' @rdname as_sample_tibble
#' @export
as_sample_data_frame = as_tibble

#' Get samples from a model as a tibble
#' 
#' Extract samples from an MCMC fit into a wide-format data frame with a 
#' \code{.chain} and \code{.iteration} column, as well as all parameters
#' as columns. Generally speaking not as useful as \code{\link{gather_samples}}
#' and is mainly used interally (see `Details`)
#' 
#' You will not typically call this directly; instead use \code{\link{gather_samples}}.
#' However, to provide support for new models in functions like \code{\link{gather_samples}},
#' you must provide an implementation of this function, \emph{or} an implementaiton
#' of \code{\link[coda]{as.mcmc.list}} (\code{as_sample_tibble} should work on any model
#' with an implementation of \code{\link[coda]{as.mcmc.list}})
#' 
#' \code{as_sample_data_frame} is an alias.
#' 
#' @param model A supported Bayesian model fit / MCMC object. Currently
#' supported models include \code{\link[coda]{mcmc}}, \code{\link[coda]{mcmc.list}},
#' \code{\link[runjags]{runjags}}, \code{\link[rstan]{stanfit}}, \code{\link[rethinking]{map}},
#' \code{\link[rethinking]{map2stan}}, and anything with its own \code{\link[coda]{as.mcmc.list}}
#' implementation.
#' @return A data frame (actually, a \code{\link[tibble]{tibble}}) with a \code{.chain} column,
#' \code{.iteration} column, and one column for every parameter in \code{model}.
#' @author Matthew Kay
#' @seealso \code{\link{gather_samples}}, which uses this function internally and provides a
#' friendly interface for extracting tidy data frames from model fits.
#' @keywords manip
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
as_sample_tibble.default = function(model) {
    as_sample_tibble(as.mcmc.list(model))
}
#' @rdname as_sample_tibble
#' @export
as_sample_tibble.mcmc.list = function(model) {
    n = nrow(model[[1]])
    map_df(seq_along(model), function(chain)
        bind_cols(
            tibble(
                .chain = chain,
                .iteration = 1:n
            ),
            as_tibble(model[[chain]])
        )
    )
}
#' @rdname as_sample_tibble
#' @export
as_sample_tibble.stanfit = function(model) {
    if (!requireNamespace("rstan", quietly = TRUE)) {
        stop('The `rstan` package is needed for `as_sample_tibble` to support `stanfit` objects.'
            , call. = FALSE)
    }
    as_sample_tibble(rstan::As.mcmc.list(model))
}
#' @rdname as_sample_tibble
#' @export
as_sample_tibble.runjags = function(model) {
    if (!requireNamespace("runjags", quietly = TRUE)) {
        stop('The `runjags` package is needed for `as_sample_tibble` to support `runjags` objects.'
        , call. = FALSE)
    }
    as_sample_tibble(as.mcmc.list(model))
}
#' @rdname as_sample_tibble
#' @export
as_sample_tibble.brmsfit = function(model) {
    if (!requireNamespace("brms", quietly = TRUE)) {
        stop('The `brms` package is needed for `as_sample_tibble` to support `brmsfit` objects.'
            , call. = FALSE)
    }
    as_sample_tibble(brms::as.mcmc(model))
}
#' @rdname as_sample_tibble
#' @export
as_sample_tibble.map = function(model) {
    mu = coef(model)
    samples = as_tibble(mvrnorm(n = 10000, mu = mu, Sigma = vcov(model)))
    #map models have no chains
    bind_cols(data_frame(.chain=1, .iteration = 1:nrow(samples)), samples)
}
#' @rdname as_sample_tibble
#' @export
as_sample_tibble.map2stan = function(model) {
    as_sample_tibble(model@stanfit)
}
#' @rdname as_sample_tibble
#' @export
as_sample_tibble.matrix = function(model) {
    if (length(dim(model)) == 2) {
        # assume matrix indexed by [interations, variables]
        as_sample_tibble(as.mcmc.list(as.mcmc(model)))
    } else {
        stop("Matrix must have only 2 dimensions (first being the sample, second the variable).")
    }
}
