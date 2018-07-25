# Documentation of supported models
#
# Author: Matthew Kay
###############################################################################


#' Models supported by tidybayes
#'
#' @name tidybayes-models
#'
#' @description
#'
#' Tidybayes supports two classes of models and sample formats: Models/formats that provide prediction functions, and those that
#' do not.
#'
#' @section All Supported Models/Sample Formats:
#'
#' \strong{All supported models/formats} support the base tidybayes sample extraction functions, such as
#' \code{\link{tidy_draws}}, \code{\link{spread_draws}}, and \code{\link{gather_draws}}. These models/formats include:
#'
#' \itemize{
#'   \item \code{\link[rstan:stan]{rstan}} models
#'   \item \code{\link[brms]{brm}} models
#'   \item \code{\link[rstanarm:rstanarm-package]{rstanarm}} models
#'   \item \code{\link[runjags]{runjags}} models
#'   \item \code{\link[rjags]{jags.model}} models, if sampled using \code{\link[rjags]{coda.samples}}
#'   \item \code{\link[jagsUI]{jags}} models
#'   \item \code{\link[MCMCglmm]{MCMCglmm}} models
#'   \item \code{\link[coda]{mcmc}} and \code{\link[coda]{mcmc.list}} objects, which are output by several model
#'     types.
#'   \item Any object with an implementation of \code{\link[coda]{as.mcmc.list}}. For a list of those available in your
#'     environment, run \code{methods(as.mcmc.list)}
#' }
#'
#' If you install the \code{tidybayes.rethinking} package (available at
#' \url{https://github.com/mjskay/tidybayes.rethinking}), \code{map} and
#' \code{map2stan} models from the \code{rethinking} package are also supported.
#'
#'
#' @section Models Supporting Prediction:
#'
#' In addition, the \strong{following models support fit and prediction} extraction functions, such as
#' \code{\link{add_fitted_draws}} and \code{\link{add_predicted_draws}}:
#'
#' \itemize{
#'   \item \code{\link[brms]{brm}} models
#'   \item \code{\link[rstanarm:rstanarm-package]{rstanarm}} models
#' }
#'
#' If you install the \href{https://github.com/mjskay/tidybayes.rethinking}{tidybayes.rethinking} package, models from
#' the \href{https://github.com/rmcelreath/rethinking}{rethinking} package are also supported. Note that in
#' \code{tidybayes.rethinking}, \code{tidy_link} takes the place of
#' \code{\link{add_fitted_draws}} and \code{tidy_sim} takes the place of
#' \code{\link{add_predicted_draws}}.
#'
#'
#' @section Extending tidybayes:
#'
#' To include basic support for new models, one need only implement the \code{\link{tidy_draws}} generic function
#' for that model.
#'
#' To include support for estimation and prediction, one must implement the \code{\link{fitted_draws}} and
#' \code{\link{predicted_draws}} generic functions.
#'
NULL
