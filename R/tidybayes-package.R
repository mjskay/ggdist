#' Tidy Data and 'Geoms' for Bayesian Models
#'
#' @docType package
#' @name tidybayes-package
#' @aliases tidybayes
#'
#' @description
#'
#' `tidybayes` is an R package that aims to make it easy to integrate
#' popular Bayesian modeling methods into a tidy data + ggplot workflow.
#'
#' @details
#'
#' Tidy data frames (one observation per row) are particularly convenient for
#' use in a variety of R data manipulation and visualization packages (Wickham 2014).
#' However, when using Bayesian modeling functions like JAGS or Stan in R,
#' we often have to translate this data into a form the model understands,
#' and then after running the model, translate the resulting sample (or
#' predictions) into a more tidy format for use with other R functions.
#' `tidybayes` aims to simplify these two common (often tedious)
#' operations. It also provides a variety of ggplot geometries aimed
#' at making the visualization of model output easier.
#'
#' For a comprehensive overview of the package, see `vignette("tidybayes")`.
#' For overviews aimed at the `rstanarm` and `brms` packages, see
#' `vignette("tidy-rstanarm")` and `vignette("tidy-brms")`. For an overview
#' of the majority of geoms in tidybayes, see `vignette("slabinterval")`.
#'
#' For a list of supported models, see [tidybayes-models].
#'
#' @references
#'
#' Wickham, Hadley. (2014). Tidy data. _Journal of Statistical Software_,
#' 59(10), 1-23. \doi{10.18637/jss.v059.i10}.
#'
NULL
