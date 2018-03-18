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
#' Tidybayes supports two classes of models and sample formats: Models that provide prediction functions, and those that
#' do not.
#'
#' @section All Supported Models:
#'
#' \strong{All supported models} support the base tidybayes sample extraction functions, such as
#' \code{\link{as_sample_tibble}}, \code{\link{spread_samples}}, and \code{\link{gather_samples}}. These models include:
#'
#' \itemize{
#'   \item \code{\link[rstan:stan]{rstan}} models
#'   \item \code{\link[runjags:run.jags]{runjags}} models
#'   \item \code{\link[brms]{brm}} models
#'   \item \code{\link[rstanarm:rstanarm-package]{rstanarm}} models
#'   \item \code{\link[MCMCglmm]{MCMCglmm}} models
#'   \item \code{\link[coda]{mcmc}} and \code{\link[coda]{mcmc.list}} objects, which are output by several of model
#'     types.
#'   \item Any object with an implementation of \code{\link[coda]{as.mcmc.list}}. For a list of those available in your
#'     environment, run \code{methods(as.mcmc.list)}
#' }
#'
#' @section Models Supporting Prediction:
#'
#' In addition, the \strong{following models support fit and prediction} extraction functions, such as
#' \code{\link{add_fitted_samples}} and \code{\link{add_predicted_samples}}:
#'
#' \itemize{
#'   \item \code{\link[brms]{brm}} models
#'   \item \code{\link[rstanarm:rstanarm-package]{rstanarm}} models
#' }
#'
#' If you install the \href{https://github.com/mjskay/tidybayes.rethinking}{tidybayes.rethinking} package, models from
#' the \href{https://github.com/rmcelreath/rethinking}{rethinking} package are also supported. Note that in
#' \code{tidybayes.rethinking}, \code{\link[tidybayes.rethinking]{tidy_link}} takes the place of
#' \code{\link{add_fitted_samples}} and \code{\link[tidybayes.rethinking]{tidy_sim}} takes the place of
#' \code{\link{add_predicted_samples}}.
#'
#'
#' @section Extending tidybayes:
#'
#' To include basic support for new models, one need only implement the \code{\link{as_sample_tibble}} generic function
#' for that model.
#'
#' To include support for estimation and prediction, one must implement the \code{\link{fitted_samples}} and
#' \code{\link{predicted_samples}} generic functions.
#'
NULL
