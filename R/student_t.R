# Scaled and shifted Student's t distribution
#
# Author: mjskay
###############################################################################


#' Scaled and shifted Student's t distribution
#'
#' Density, distribution function, quantile function and random generation for the
#' scaled and shifted Student's t distribution, parameterized by degrees of freedom (`df`),
#' location (`mu`), and scale (`sigma`).
#'
#' @inheritParams stats::dt
#' @param mu Location parameter (median)
#' @param scale Scale parameter
#' @seealso [parse_dist()] and parsing distribution specs and the [stat_dist_slabinterval()]
#' family of stats for visualizing them.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' library(forcats)
#'
#' expand.grid(
#'   eta = 1:6,
#'   K = 2:6
#' ) %>%
#'   ggplot(aes(y = fct_rev(ordered(eta)), dist = "lkjcorr_marginal", arg1 = K, arg2 = eta)) +
#'   stat_dist_slab(p_limits = c(0,1)) +
#'   facet_grid(~ paste0(K, "x", K)) +
#'   labs(
#'     title = paste0(
#'       "Marginal correlation for LKJ(eta) prior on different matrix sizes:\n",
#'       "dlkjcorr_marginal(K, eta)"
#'     ),
#'     subtitle = "Correlation matrix size (KxK)",
#'     y = "eta",
#'     x = "Marginal correlation"
#'   ) +
#'   theme(axis.title = element_text(hjust = 0))
#'
#' @name student_t
#' @importFrom stats dt pt qt rt
#' @export
dstudent_t = function(x, df, mu = 0, sigma = 1, log = FALSE) {
  if (log) {
    dt((x - mu)/sigma, df = df, log = TRUE) - log(sigma)
  }
  else {
    dt((x - mu)/sigma, df = df) / sigma
  }
}

#' @rdname student_t
#' @export
pstudent_t = function(q, df, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  pt((q - mu)/sigma, df = df, lower.tail = lower.tail, log.p = log.p)
}

#' @rdname student_t
#' @export
qstudent_t = function(p, df, mu = 0, sigma = 1, lower.tail = TRUE, log.p = FALSE) {
  qt(p, df = df, lower.tail = lower.tail, log.p = log.p)*sigma + mu
}

#' @rdname student_t
#' @export
rstudent_t = function(n, df, mu = 0, sigma = 1) {
  rt(n, df = df)*sigma + mu
}
