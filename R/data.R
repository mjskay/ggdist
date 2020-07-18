# Documentation of datasets
#
# Author: Matthew Kay
###############################################################################


#' Thinned subset of posterior sample from a Bayesian analysis of perception of correlation.
#'
#' Data from Kay and Heer (2016), primarily used for testing and examples.
#'
#' For more details, see Kay and Heer (2016) or the Github repository describing the analysis:
#' <https://github.com/mjskay/ranking-correlation>. The original experiment (but not this analysis of it)
#' is described in Harrison *et al.* (2014).
#'
#' `data("RankCorr")` is a substantially thinned version of the original posterior sample and has omitted several
#' parameters in order for it to be a more manageable size.
#'
#' `data("RankCorr_u_tau")` is used for testing and examples and is roughly the equivalent of the following:
#'
#' ```
#' data("RankCorr")
#' RankCorr_u_tau = tidybayes::spread_draws(RankCorr, u_tau[i]))
#' ```
#'
#' @name RankCorr
#' @aliases RankCorr_u_tau
#' @docType data
#' @keywords datasets internal
#' @references
#'
#' Kay, Matthew, and Jeffrey Heer. (2016).
#' "Beyond Weber's law: A second look at ranking visualizations of correlation."
#' *IEEE transactions on visualization and computer graphics* 22(1): 469-478.
#' \doi{10.1109/TVCG.2015.2467671}
#'
#' Harrison, Lane, Fumeng Yang, Steven Franconeri, and Remco Chang. (2014).
#' "Ranking visualizations of correlation using Weber's law."
#' *IEEE transactions on visualization and computer graphics* 20(12): 1943-1952.
#' \doi{10.1109/TVCG.2014.2346979}
NULL
