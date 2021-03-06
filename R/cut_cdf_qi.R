# Helper function for splitting up a slab into its intervals
#
# Author: mjskay
###############################################################################


#' Categorize values from a CDF into quantile intervals
#'
#' Given a vector of probabilities from a cumulative distribution function (CDF)
#' and a list of desired quantile intervals, return a vector categorizing each
#' element of the input vector according to which quantile interval it falls into.
#' Useful for drawing slabs with intervals overlaid on the density, e.g. using
#' [stat_halfeye()] or [stat_dist_halfeye()]
#'
#' @param p A numeric vector of values from a cumulative distribution function,
#' such as values returned by `p`-prefixed distribution functions in base R (e.g. [pnorm()]),
#' the [cdf()] function, or values of the `cdf` computed aesthetic from the
#' [stat_sample_slabinterval()] or [stat_dist_slabinterval()] stats.
#' @param .width vector of probabilities to use that determine the widths of the resulting intervals.
#' @param labels One of:
#'   - `NULL` to use the default labels (`.width` converted to a character vector).
#'   - A character vector giving labels (must be same length as `.width`)
#'   - A function that takes numeric probabilities as input and returns labels as output
#'     (a good candidate might be [scales::percent_format()]).
#'
#' @return
#' An [ordered] factor of the same length as `p` giving the quantile interval to
#' which each value of `p` belongs.
#'
#' @seealso See [stat_sample_slabinterval()] or [stat_dist_slabinterval()] and
#' their shortcut stats, which generate `cdf` aesthetics that can be used with
#' `cut_cdf_qi()` to draw slabs colored by their intervals.
#' @examples
#'
#' library(ggplot2)
#' library(dplyr)
#' library(scales)
#' library(distributional)
#'
#' theme_set(theme_ggdist())
#'
#' # with a slab
#' tibble(x = dist_normal(0, 1)) %>%
#'   ggplot(aes(dist = x, y = "a")) +
#'   stat_dist_slab(aes(
#'     fill = stat(cut_cdf_qi(cdf))
#'   )) +
#'   scale_fill_brewer(direction = -1, na.value = "gray90")
#'
#' # With a halfeye (or other geom with slab and interval), NA values will
#' # show up in the fill scale from the CDF function applied to the internal
#' # interval geometry data and can be ignored, hence na.translate = FALSE
#' tibble(x = dist_normal(0, 1)) %>%
#'   ggplot(aes(dist = x, y = "a")) +
#'   stat_dist_halfeye(aes(
#'     fill = stat(cut_cdf_qi(cdf, .width = c(.5, .8, .95, 1)))
#'   )) +
#'   scale_fill_brewer(direction = -1, na.translate = FALSE)
#'
#' # we could also use the labels parameter to apply nicer formatting
#' # and provide a better name for the legend, and omit the 100% interval
#' # if desired
#' tibble(x = dist_normal(0, 1)) %>%
#'   ggplot(aes(dist = x, y = "a")) +
#'   stat_dist_halfeye(aes(
#'     fill = stat(cut_cdf_qi(cdf, .width = c(.5, .8, .95), labels = percent_format(accuracy = 1)))
#'   )) +
#'   labs(fill = "Interval") +
#'   scale_fill_brewer(direction = -1, na.translate = FALSE)
#'
#' @export
cut_cdf_qi = function(p, .width = c(.66, .95, 1), labels = NULL) {
  .width = sort(.width)

  if (is.function(labels)) {
    labels = labels(.width)
  } else if (is.null(labels)) {
    labels = .width
  }

  cut(abs(1 - p*2), labels = labels, breaks = c(0, .width), include.lowest = TRUE, ordered_result = TRUE)
}
