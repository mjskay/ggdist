#' @details
#' The *dots* family of stats and geoms are similar to [ggplot2::geom_dotplot()] but with a number of differences:
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
#' Stats and geoms in this family include:
#'
#'  - [geom_dots()]: dotplots on raw data. Ensures the dotplot fits within available space by reducing the size
#'    of the dots automatically (may result in very small dots).
#'  - [geom_swarm()] and [geom_weave()]: dotplots on raw data with defaults intended to create "beeswarm" plots.
#'    Used `side = "both"` by default, and sets the default dot size to the same size as [`geom_point()`][ggplot2::geom_point]
#'    (`binwidth = unit(1.5, "mm")`), allowing dots to overlap instead of getting very small.
#'  - [stat_dots()]: dotplots on raw data, \pkg{distributional} objects, and [posterior::rvar()]s
#'  - [geom_dotsinterval()]: dotplot + interval plots on raw data with already-calculated
#'    intervals (rarely useful directly).
#'  - [stat_dotsinterval()]: dotplot + interval plots on raw data, \pkg{distributional} objects,
#'    and [posterior::rvar()]s (will calculate intervals for you).
#'  - [geom_blur_dots()]: blurry dotplots that allow the standard deviation of a blur applied to
#'    each dot to be specified using the `sd` aesthetic.
#'  - [stat_mcse_dots()]: blurry dotplots of quantiles using the Monte Carlo Standard Error of each quantile.
#'
#' [stat_dots()] and [stat_dotsinterval()], when used with the `quantiles` argument,
#' are particularly useful for constructing quantile dotplots, which can be an effective way to communicate uncertainty
#' using a frequency framing that may be easier for laypeople to understand (Kay et al. 2016, Fernandes et al. 2018).
#'
