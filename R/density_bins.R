# Density bins as data frames
#
# Author: mjskay
###############################################################################


#' Density bins and histogram bins as data frames
#'
#' Generates a data frame of bins representing the kernel density (or
#' histogram) of a vector, suitable for use in generating predictive
#' distributions for visualization. These functions were originally
#' designed for use with the now-deprecated `predict_curve()`, and
#' may be deprecated in the future.
#'
#' These functions are simple wrappers to [density()] and
#' [hist()] that compute density estimates and return their results
#' in a consistent format: a data frame of bins suitable for use with
#' the now-deprecated [predict_curve()].
#'
#' `density_bins` computes a kernel density estimate using
#' [density()].
#'
#' `histogram_bins` computes a density histogram using [hist()].
#'
#' @param x A numeric vector
#' @param n Number of bins
#' @param breaks Used to set bins for `histogram_bins`. Can be number of bins (by default it is set to the value
#' of `n`) or a method for setting bins. See the `breaks` argument of [hist()].
#' @param ...  Additional arguments passed to [density()] or
#' [hist()].
#' @return A data frame representing bins and their densities with the
#' following columns: \item{mid}{Bin midpoint} \item{lower}{Lower endpoint of
#' each bin} \item{upper}{Upper endpoint of each bin} \item{density}{Density
#' estimate of the bin}
#' @author Matthew Kay
#' @seealso See [add_predicted_draws()] and [stat_lineribbon()] for a better approach. These
#' functions may be deprecated in the future.
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(ggplot2)
#' library(dplyr)
#' library(purrr)
#' library(tidyr)
#'
#' if (
#'   require("rstanarm", quietly = TRUE) &&
#'   require("modelr", quietly = TRUE)
#' ) {
#'
#'   theme_set(theme_light())
#'
#'   m_mpg = stan_glm(mpg ~ hp * cyl, data = mtcars)
#'
#'   step = 1
#'   mtcars %>%
#'     group_by(cyl) %>%
#'     data_grid(hp = seq_range(hp, by = step)) %>%
#'     add_predicted_draws(m_mpg) %>%
#'     summarise(densities = list(density_bins(.prediction))) %>%
#'     unnest(densities) %>%
#'     ggplot() +
#'     geom_rect(aes(
#'       xmin = hp - step/2, ymin = lower, ymax = upper, xmax = hp + step/2,
#'       fill = ordered(cyl), alpha = density
#'     )) +
#'     geom_point(aes(x = hp, y = mpg, fill = ordered(cyl)), shape = 21, data = mtcars) +
#'     scale_alpha_continuous(range = c(0, 1)) +
#'     scale_fill_brewer(palette = "Set2")
#' }
#' }
#' @importFrom stats density
#' @export
density_bins = function(x, n = 101, ...) {
  d = density(x, n = n, cut = 0, ...)

  mid = d$x
  last_mid = length(mid)
  x_diffs = mid[-1] - mid[-last_mid]

  tibble(
    mid = mid,
    lower = c(mid[[1]] - x_diffs[[1]] / 2, mid[-1] - x_diffs / 2),
    upper = c(mid[-last_mid] + x_diffs / 2, mid[[last_mid]] + x_diffs[[last_mid - 1]] / 2),
    density = d$y
  )
}

#' @rdname density_bins
#' @importFrom graphics hist
#' @importFrom stats embed
#' @export
histogram_bins = function(x, n = 30, breaks = n, ...) {
  h = hist(x, breaks = breaks, ..., plot = FALSE)

  tibble(
    mid = rowMeans(embed(h$breaks, 2)),
    lower = h$breaks[-length(h$breaks)],
    upper = h$breaks[-1],
    density = h$density
  )
}
