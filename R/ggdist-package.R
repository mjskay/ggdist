#' Visualizations of Distributions and Uncertainty
#'
#' @docType package
#' @name ggdist-package
#' @aliases ggdist
#'
#' @description
#'
#' `ggdist` is an R package that aims to make it easy to integrate
#' popular Bayesian modeling methods into a tidy data + ggplot workflow.
#'
#' @details
#'
#' `ggdist` is an R package that provides a flexible set of `ggplot2` geoms and stats designed
#' especially for visualizing distributions and uncertainty. It is designed for both
#' frequentist and Bayesian uncertainty visualization, taking the view that uncertainty
#' visualization can be unified through the perspective of distribution visualization:
#' for frequentist models, one visualizes confidence distributions or bootstrap distributions
#' (see `vignette("freq-uncertainty-vis")`); for Bayesian models, one visualizes probability
#' distributions (see `vignette("tidybayes", package = "tidybayes")`).
#'
#' The [geom_slabinterval()] / [stat_slabinterval()] / [stat_dist_slabinterval()] family (see `vignette("slabinterval")`) includes
#' point summaries and intervals, eye plots, half-eye plots, CCDF bar plots, gradient plots, dotplots, and histograms.
#'
#' The [geom_lineribbon()] / [stat_lineribbon()] / [stat_dist_lineribbon()] family (see `vignette("lineribbon")`)
#' makes it easy to visualize fit lines with an arbitrary number of uncertainty bands.
#'
NULL
