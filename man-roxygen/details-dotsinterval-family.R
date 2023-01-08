#' @details
#' The *dots* family of stats and geoms are similar to [geom_dotplot()] but with a number of differences:
#'
#' \itemize{
#'   \item Dots geoms act like slabs in [geom_slabinterval()] and can be given x positions (or y positions when
#'   in a horizontal orientation).
#'   \item Given the available space to lay out dots, the dots geoms will automatically determine how many bins to
#'   use to fit the available space.
#'   \item Dots geoms use a dynamic layout algorithm that lays out dots from the center out if the input data are
#'   symmetrical, guaranteeing that symmetrical data results in a symmetrical plot. The layout algorithm also prevents
#'   dots from overlapping each other.
#'   \item The shape of the dots in these geoms can be changed using the `slab_shape` aesthetic (when using the
#'   `dotsinterval` family) or the `shape` or `slab_shape` aesthetic (when using the `dots` family)
#' }
#'
#' Stat and geoms include in this family include:
#'
#'  - [geom_dots()]: dotplots on raw data
#'  - [stat_dots()]: dotplots on raw data, \pkg{distributional} objects, and [posterior::rvar()]s
#'  - [geom_dotsinterval()]: dotplot + interval plots on raw data with already-calculated
#'    intervals (rarely useful directly)
#'  - [stat_dotsinterval()]: dotplot + interval plots on raw data, \pkg{distributional} objects,
#'    and [posterior::rvar()]s (will calculate intervals for you)
#'
#' [stat_dots()] and [stat_dotsinterval()], when used with the `quantiles` argument,
#' are particularly useful for constructing quantile dotplots, which can be an effective way to communicate uncertainty
#' using a frequency framing that may be easier for laypeople to understand (Kay et al. 2016, Fernandes et al. 2018).
#'
