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
#' **All supported models/formats** support the base tidybayes sample extraction functions, such as
#' [tidy_draws()], [spread_draws()], and [gather_draws()]. These models/formats include:
#'
#' \itemize{
#'   \item [rstan][rstan::stan] models
#'   \item [brms::brm()] models
#'   \item [rstanarm][rstanarm::rstanarm-package] models
#'   \item [runjags::runjags()] models
#'   \item [rjags::jags.model()] models, if sampled using [rjags::coda.samples()]
#'   \item [jagsUI::jags()] models
#'   \item [MCMCglmm::MCMCglmm()] models
#'   \item [coda::mcmc()] and [coda::mcmc.list()] objects, which are output by several model
#'     types.
#'   \item Any object with an implementation of [coda::as.mcmc.list()]. For a list of those available in your
#'     environment, run `methods(as.mcmc.list)`
#' }
#'
#' If you install the [tidybayes.rethinking](https://mjskay.github.io/tidybayes.rethinking/) package, models from
#' the [rethinking](https://github.com/rmcelreath/rethinking) package are also supported.
#'
#'
#' @section Models Supporting Prediction:
#'
#' In addition, the **following models support fit and prediction** extraction functions, such as
#' [add_fitted_draws()] and [add_predicted_draws()]:
#'
#' \itemize{
#'   \item [brms::brm()] models
#'   \item [rstanarm][rstanarm::rstanarm-package] models
#' }
#'
#' **If your model type is not in the above list**, you may still be able to use the [add_draws()]
#' function to turn matrices of predictive draws (or fit draws) into tidy data frames.
#'
#' If you install the [tidybayes.rethinking](https://mjskay.github.io/tidybayes.rethinking/) package, models from
#' the [rethinking](https://github.com/rmcelreath/rethinking) package are also supported.
#'
#'
#' @section Extending tidybayes:
#'
#' To include basic support for new models, one need only implement the [tidy_draws()] generic function
#' for that model.
#'
#' To include support for estimation and prediction, one must implement the [fitted_draws()] and
#' [predicted_draws()] generic functions.
#'
NULL
