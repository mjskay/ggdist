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
#' @param sigma Scale parameter
#' @return
#' - `dstudent_t` gives the density
#' - `pstudent_t` gives the cumulative distribution function (CDF)
#' - `qstudent_t` gives the quantile function (inverse CDF)
#' - `rstudent_t` generates random draws.
#'
#' The length of the result is determined by `n` for `rstudent_t`, and is the maximum of the lengths of
#' the numerical arguments for the other functions.
#'
#' The numerical arguments other than `n` are recycled to the length of the result. Only the first elements
#' of the logical arguments are used.
#'
#' @seealso [parse_dist()] and parsing distribution specs and the [stat_slabinterval()]
#' family of stats for visualizing them.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' expand.grid(
#'   df = c(3,5,10,30),
#'   scale = c(1,1.5)
#' ) %>%
#'   ggplot(aes(y = 0, dist = "student_t", arg1 = df, arg2 = 0, arg3 = scale, color = ordered(df))) +
#'   stat_slab(p_limits = c(.01, .99), fill = NA) +
#'   scale_y_continuous(breaks = NULL) +
#'   facet_grid( ~ scale) +
#'   labs(
#'     title = "dstudent_t(x, df, 0, sigma)",
#'     subtitle = "Scale (sigma)",
#'     y = NULL,
#'     x = NULL
#'   ) +
#'   theme_ggdist() +
#'   theme(axis.title = element_text(hjust = 0))
#'
#' @name student_t
#' @importFrom stats dt pt qt rt
#' @export
dstudent_t = function(x, df, mu = 0, sigma = 1, log = FALSE) {
  if (log) {
    dt((x - mu)/sigma, df = df, log = TRUE) - log(sigma)
  } else {
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
