# parameters
#
# Author: mjskay
###############################################################################


#' Get the names of the parameters in a fitted Bayesian model
#'
#' Get a character vector of the names of the parameters in a variety of fitted
#' Bayesian models. All models supported by \code{\link{as_sample_tibble}} are
#' supported.
#'
#' This function is often useful for inspecting a model interactively in order
#' to construct calls to \code{\link{spread_samples}} or \code{\link{gather_samples}}
#' in order to extract samples in a tidy format.
#'
#' @param model A supported Bayesian model fit / MCMC object. Currently
#' supported models include \code{\link[coda]{mcmc}}, \code{\link[coda]{mcmc.list}},
#' \code{\link[runjags]{runjags}}, \code{\link[rstan]{stanfit}}, \code{\link[rstanarm]{stanreg-objects}},
#' \code{\link[brms]{brm}}, and anything with its own \code{\link[coda]{as.mcmc.list}} implementation.
#' If you install the \code{tidybayes.rethinking} package (available at
#' \url{https://github.com/mjskay/tidybayes.rethinking}), \code{map} and
#' \code{map2stan} models from the \code{rethinking} package are also supported.
#' @return A character vector of parameter names in the fitted model.
#' @author Matthew Kay
#' @seealso \code{\link{spread_samples}}, \code{\link{gather_samples}}.
#' @keywords manip
#' @examples
#'
#' data(line, package = "coda")
#' parameters(line)
#'
#' data(RankCorr, package = "tidybayes")
#' parameters(RankCorr)
#'
#' @importFrom magrittr %>%
#' @export
parameters = function(model) UseMethod("parameters")

#' @rdname parameters
#' @export
parameters.default = function(model) {
  #default method just uses as_sample_tibble; could add faster model-type-specific methods
  model %>%
    as_sample_tibble() %>%
    names() %>%
    setdiff(c(".chain", ".iteration"))
}
