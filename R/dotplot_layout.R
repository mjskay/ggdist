# dotplot layouts for use with bin_dots
#
# Author: mjskay
###############################################################################
#' @include validate.R
NULL


# dotplot_layout -----------------------------------------------------------------

new_dotplot_layout_class = function(...) auto_partial(new_class(...), required = "dots")

#' Base class for dotplot and beeswarm layout algorithms.
#' @description
#' \pkg{S7} base class for dotplot and beeswarm layout algorithms.
#' @template description-dotplot-layout
#' @details
#' A `dotplot_layout` defines how dots are arranged in a dotplot created with
#' [bin_dots()]. Different layouts implement different dotplot layout algorithms.
#'
#' Layouts are always written assuming a horizontal orientation: `dots$x` contains
#' data values and `dots$y` contains the height of each dot in a bin/swarm. Thus,
#' the only valid values of `side` for this function are `"top"`, `"bottom"`, or `"both"`.
#' Transformations from this canonical orientation to a vertical orientation are
#' made (if needed) by `bin_dots()` depending on the `orientation` parameter passed
#' to that function.
#' @param dots <[data.frame]> Positions of dots. Not typically passed directly: this
#' data structure is prepared and passed to the layout by dotplot layout functions
#' like [bin_dots()] and [find_dotplot_binwidth()]. Has columns:
#'  - `x` <[numeric]> Dot positions.
#'  - `y` <[numeric]> Dot heights.
#'  - `group` <[integer]> Indices of stacked groups of dots in the layout.
#'  - `order` <[integer]> Data order from the original input data.
#' @param maxheight <scalar [numeric]> maximum height of the dotplot layout.
#' @param heightratio <scalar [numeric]> Ratio of vertical distance between dot centers to bin (dot)
#' width.
#' @param stackratio <scalar [numeric]> Ratio of vertical distance between dot centers to bin (dot)
#' height.
#' @param side <[string][character]> side where the dotplot layout should be placed. One
#' of `"top"`, `"bottom"`, or `"both"`.
#' @eval rd_param_dots_overlaps()
#' @eval rd_param_dots_span()
#' @return <[dotplot_layout]> \pkg{S7} object.
#' @import S7
dotplot_layout = new_dotplot_layout_class(
  "dotplot_layout",
  abstract = TRUE,
  properties = list(
    dots = new_property(
      class_data.frame,
      validator = function(value) {
        if (!is.numeric(value$x)) {
          "must have numeric x column"
        } else if (!is.numeric(value$y)) {
          "must have numeric y column"
        } else if (!is.numeric(value$group)) {
          "must have numeric group column"
        } else if (!is.numeric(value$order)) {
          "must have numeric order column"
        }
      },
      default = quote(data.frame(x = numeric(), y = numeric(), group = integer(), order = integer()))
    ),
    maxheight = new_property(
      class_numeric,
      validator = validate_positive_scalar,
      default = Inf
    ),
    heightratio = new_property(
      class_numeric,
      validator = validate_positive_scalar,
      default = 1
    ),
    stackratio = new_property(
      class_numeric,
      validator = validate_positive_scalar,
      default = 1
    ),
    side = new_property(
      class_character,
      validator = validate_in(c("top", "bottom", "both")),
      default = "top"
    ),
    overlaps = new_property(
      class_character,
      validator = validate_in(c("keep", "nudge")),
      default = "nudge"
    ),
    span = new_property(
      class_numeric,
      validator = validate_nonnegative_scalar,
      default = 0
    )
  )
)


# bin-based layouts ----------------------------------------------------------------

#' Binned dotplot layout
#' @description
#' Wilkinson-esque binned dotplot layout.
#' @template description-dotplot-layout
#' @inheritParams dotplot_layout
#' @prop binner <[function]> internal function that takes data and bin width as input and returns
#' a list with components `bins` and `bin_midpoints`.
#' @prop align_rows <[logical]> whether to align rows of dots when `side` is "both".
#' @return <[dotplot_layout]> object of class `layout_bin`.
layout_bin = new_dotplot_layout_class(
  "layout_bin",
  parent = dotplot_layout,
  properties = list(
    dots = new_property(
      class_data.frame,
      setter = function(self, value) {
        self@dots = value
        # examines data to determine an appropriate binning method based on its properties
        # doing this up front allows us to avoid doing it repeatedly when finding binwidth via optimization
        diff_x = diff(value$x)
        if (isTRUE(all.equal(diff_x, rev(diff_x), check.attributes = FALSE))) {
          # x is symmetric, use centered binning
          attr(self, "binner") = wilkinson_bin_from_center
        } else {
          attr(self, "binner") = wilkinson_bin
        }
        self
      },
      validator = dotplot_layout@properties$dots$validator,
      default = dotplot_layout@properties$dots$default
    ),
    binner = new_property(
      class_function,
      default = quote(function(...) cli_abort("`x` must be set to determine `binner`.")),
      getter = function(self) attr(self, "binner")
    ),
    align_rows = new_property(
      class_logical,
      getter = function(self) FALSE
    )
  )
)

#' Weave dotplot layout
#' @description
#' Weave dotplot layout with aligned rows and exact (when `overlaps = "keep"`) or near-exact (when
#' `overlaps = "nudge"`) dot placement.
#' @template description-dotplot-layout
#' @inheritParams layout_bin
#' @return <[dotplot_layout]> object of class `layout_weave`.
layout_weave = new_dotplot_layout_class(
  "layout_weave",
  parent = layout_bin,
  properties = list(
    align_rows = new_property(
      class_logical,
      getter = function(self) TRUE
    )
  )
)

#' Hexagonal dotplot layout
#' @description
#' Hexagonal dotplot layout.
#' @template description-dotplot-layout
#' @inheritParams layout_bin
#' @return <[dotplot_layout]> object of class `layout_hex`.
layout_hex = new_dotplot_layout_class(
  "layout_hex",
  parent = layout_bin,
  properties = list(
    align_rows = new_property(
      class_logical,
      getter = function(self) TRUE
    )
  )
)


# bar dotplot layout -------------------------------------------------------------

#' Bin dots into bars
#' @param x data (original positions of dots)
#' @param binwidth width of the bins in data units
#' @param span max width of the bars as a proportion of the data resolution
#' @noRd
bar_bin = function(x, binwidth, span = 0.9) {
  # determine the amount of space that each bar will take up
  # TODO: can drop as.numeric here if https://github.com/tidyverse/ggplot2/issues/5709 is fixed
  max_bar_width = resolution(as.numeric(x), zero = FALSE) * span
  n_bins = min(max(floor(max_bar_width / binwidth), 1), length(x))
  actual_bar_width = n_bins * binwidth

  # determine new x positions
  bin_positions = (ppoints(n_bins, a = 0.5) - 0.5) * actual_bar_width
  split(x, x) = lapply(split(x, x), function(x) {
    offset_to_center = max((n_bins - length(x)) / n_bins * actual_bar_width / 2, 0)
    rep_len(bin_positions, length(x)) + x[[1]] + offset_to_center
  })

  bin_midpoints = unique(x)
  list(
    bins = match(x, bin_midpoints),
    bin_midpoints = bin_midpoints
  )
}

#' Bar (waffle) dotplot layout
#' @description
#' Bar dotplot layout for discrete data.
#' @template description-dotplot-layout
#' @inheritParams dotplot_layout
#' @return <[dotplot_layout]> object of class `layout_bar`.
layout_bar = new_dotplot_layout_class(
  "layout_bar",
  parent = dotplot_layout,
  properties = list(
    binner = new_property(
      class_function,
      getter = function(self) bar_bin
    ),
    overlaps = new_property(
      class_character,
      # setting this argument is ignored since it doesn't make a difference
      # for this layout (overlaps are impossible) but internally if we fix
      # it to "keep" we can skip nudging computations
      setter = function(self, value) self,
      getter = function(self) "keep"
    ),
    align_rows = new_property(
      class_logical,
      getter = function(self) TRUE
    ),
    span = new_property(
      class_numeric,
      validator = validate_nonnegative_scalar,
      default = 0.9
    )
  )
)


# swarm layout ----------------------------------------------------------

#' Deprecated implementation of swarm dotplot layout
#' @description
#' Beeswarm dotplot layout that uses the `"compactswarm"` algorithm from the \pkg{beeswarm} package.
#' Superceded by [layout_swarm()], which is faster and supports stacking groups of dots.
#' @inheritParams dotplot_layout
#' @return <[dotplot_layout]> object of class `layout_oldswarm`.
#' @seealso [layout_swarm()]
#' @keywords internal
layout_oldswarm = new_dotplot_layout_class(
  "layout_oldswarm",
  parent = dotplot_layout
)

#' Stackable beeswarm dotplot layout
#' @description
#' Fast, stackable, compact beeswarm dotplot layout with optional stratification.
#' @template description-dotplot-layout
#' @inheritParams dotplot_layout
#' @param strata <scalar [numeric]> \eqn{\ge 1}: a postive integer giving the number of strata to use
#' per 1 dot height. Given `y_spacing` representing the vertical distance between the centers of two
#' dots stacked directly on top of each other (the dot height times the `stackratio`), strata
#' operates as follows:
#' - `strata = 1` will yield a swarm with aligned rows of dots stacked on top of each other.
#' - `strata = k` for \eqn{1 < k < \infty} will place dots at heights that are multiples of
#'   `y_spacing / strata.
#' - `strata = Inf` will place dots using a stackable variation on the compact swarm algorithm
#'   (see *Details*).
#' @param cohesion <scalar [numeric]> \eqn{\ge 0} and \eqn{\le 1}: Cohesion of stacked dot groups
#' when `strata = Inf`, given as a fraction of `y_spacing`, the vertical distance between the
#' centers of two dots stacked directly on top of each other (the dot height times the
#' `stackratio`). A `cohesion` of `1` keeps dots from the same group together, but may introduce
#' gaps in the layout where one group is stacked on top of another. Lower cohesion trades off
#' maintaining the stacking order of groups for a tighter overall layout with fewer gaps. A
#' `cohesion` of `0.5` (the default) is often a reasonable compromise. This parameter controls a
#' penalty term added to dot heights when picking the next dot to place such that the height of a
#' dot in group \eqn{i} that could be placed at height `y` if it were in group \eqn{i - 1} (the
#' group below it) is treated as if it had a height of `y + cohesion * y_spacing`.
#' @details
#' `layout_swarm` is a stackable, exact-*x*-position, compact beeswarm layout. It uses one of two
#' layout algorithms, depending on the value of `strata`. Both algorithms position dots in exactly
#' their original data position and create compact layouts that do not have a "lean" (the visual
#' artifact created by some beeswarm layouts based on the order that dots are placed in). Both
#' algorithms allow groups of dots to be stacked on top of each other in an order determined by the
#' value of `dots$group`.
#'
#' **When `strata` is finite**, a *stratified* layout algorithm is used. This algorithm places dots
#' in alternating left/right sweeps along grid lines, greedily placing the next non-overlapping dot
#' that is closest in *x* position to the most recently-placed dot in the same row. The distance
#' between grid lines is `y_spacing / strata`, where `y_spacing` is the vertical distance between
#' the centers of two dots stacked directly on top of each other (the dot height times the
#' `stackratio`).
#'
#' **When `strata` is `Inf`**, an algorithm inspired by the the "compact swarm" algorithm in
#' \pkg{beeswarm} is used, rewritten to improve performance and to allow for stacking of groups.
#' This algorithm maintains a priority queue of contiguous regions of unplaced dots. Regions are
#' prioritized by the lowest height an unplaced dot in that region can be placed at. We use golden
#' section search to find the lowest dot in a region without checking all dots in a region. Placed
#' dots are stored in a frontier sorted by x position. As dots are placed, we prune dots from
#' the frontier that are low enough that we can guarantee they will not intersect with any remaining
#' unplaced dots. Stacking of groups is achieved by queueing regions from each group separately and
#' applying a penalty term to the height of regions corresponding to higher groups (see the
#' `cohesion` parameter).
#' @return <[dotplot_layout]> object of class `layout_swarm`.
layout_swarm = new_dotplot_layout_class(
  "layout_swarm",
  parent = dotplot_layout,
  properties = list(
    dots = new_property(
      class_data.frame,
      setter = function(self, value) {
        self@dots = value
        split_swarm_xs(self)
      },
      validator = dotplot_layout@properties$dots$validator,
      default = dotplot_layout@properties$dots$default
    ),
    xs = new_property(
      class_list,
      setter = NULL,
      getter = \(self) self@xs
    ),
    strata = new_property(
      class_numeric,
      validator = validate_positive_scalar_integerish,
      default = 4L
    ),
    cohesion = new_property(
      class_numeric,
      validator = validate_unit_scalar,
      default = 0.5
    )
  )
)

#' Split x into groups so that `layout_swarm` can plot groups in order
#' @description
#' Split `x` by `group` so that we don't have to recompute these splits when
#' doing `find_dotplot_binwidth()`. The stratified swarm layout differs from
#' others in that it needs these splits in order to plot groups in the right
#' order within each stratum.
#' @noRd
split_swarm_xs = function(self) {
  if (!is.null(self@dots$x) && !is.null(self@dots$group)) {
    x_splits = vec_split(self@dots$x, self@dots$group)
    attr(self, "xs") = x_splits$val[order(x_splits$key)]
  }
  self
}
