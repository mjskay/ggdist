# dotplot layouts for use with bin_dots
#
# Author: mjskay
###############################################################################
#' @include validate.R
NULL


# dotplot_layout -----------------------------------------------------------------

new_dotplot_layout_class = function(...) auto_partial(new_class(...), required = "x")

#' Base class for dotplot layouts created with `bin_dots()`
#' @description
#' Base class for dotplot layout algorithms used by `bin_dots()`.
#' @details
#' A `dotplot_layout` defines how dots are arranged in a dotplot created with
#' `bin_dots()`. Different layouts implement different dotplot layout algorithms.
#' @param x <[numeric]> Positions of dots.
#' @param maxheight <scalar [numeric]> maximum height of the dotplot layout.
#' @param heightratio <scalar [numeric]> height ratio of the dotplot layout.
#' @param stackratio <scalar [numeric]> stack ratio of the dotplot layout.
#' @param side <[string][character]> side where the dotplot layout should be placed.
#' @param orientation <[string][character]> orientation of the dotplot layout.
#' @param overlaps <[string][character]> how to handle overlaps in the dotplot layout.
#' @param span <scalar [numeric]> smoothing/spacing parameter used in some layouts.
#' @return <[dotplot_layout]> object.
#' @import S7
dotplot_layout = new_dotplot_layout_class(
  "dotplot_layout",
  abstract = TRUE,
  properties = list(
    x = new_property(
      class_numeric,
      default = double()
    ),
    group = new_property(
      class_any,
      validator = validate_not_na,
      default = integer()
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
      validator = validate_in(
        c("topright", "top", "right", "bottomleft", "bottom", "left", "topleft", "bottomright", "both")
      ),
      default = "topright"
    ),
    orientation = new_property(
      class_character,
      validator = validate_in(c("horizontal", "vertical", "y", "x")),
      default = "horizontal"
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

#' Create a new dotplot layout
#' @param layout <[string][character]> name of the layout as passed to `bin_dots()`.
#' @param ... Additional arguments passed to the dotplot layout constructor.
#' @return An object of the specified `dotplot_layout` class.
#' @noRd
new_dotplot_layout = function(layout, ...) {
  match_function(layout, "layout_")(...)
}


# bin-based layouts ----------------------------------------------------------------

#' Binned dotplot layout
#' @description
#' Wilkinson-esque dotplot layout for use with `bin_dots()`, `geom_dots()`, etc.
#' @inheritParams dotplot_layout
#' @param bin_method <function> function that takes data and bin width as input and returns
#' a list with components `bins` and `bin_midpoints`.
#' @param align_rows <logical> whether to align rows of dots when `side` is "both".
#' @return <[dotplot_layout]> object of class `layout_bin`.
layout_bin = new_dotplot_layout_class(
  "layout_bin",
  parent = dotplot_layout,
  properties = list(
    x = new_property(
      class_numeric,
      setter = function(self, value) {
        self@x = value
        # examines data to determine an appropriate binning method based on its properties
        # doing this up front allows us to avoid doing it repeatedly when finding binwidth via optimization
        diff_x = diff(value)
        if (isTRUE(all.equal(diff_x, rev(diff_x), check.attributes = FALSE))) {
          # x is symmetric, use centered binning
          self@bin_method = wilkinson_bin_from_center
        } else {
          self@bin_method = wilkinson_bin
        }
        self
      },
      default = double()
    ),
    bin_method = new_property(
      class_function,
      default = function(...) cli_abort("`x` must be set to determine `bin_method`.")
    ),
    align_rows = new_property(
      class_logical,
      default = FALSE
    )
  )
)

#' Weave dotplot layout
#' @description
#' Weave dotplot layout for use with `bin_dots()`, `geom_dots()`, etc.
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
#' Hexagonal dotplot layout for use with `bin_dots()`, `geom_dots()`, etc.
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
  n_bins = max(floor(max_bar_width / binwidth), 1)
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
#' Bar dotplot layout for use with `bin_dots()`, `geom_dots()`, etc.
#' @inheritParams dotplot_layout
#' @return <[dotplot_layout]> object of class `layout_bar`.
layout_bar = new_dotplot_layout_class(
  "layout_bar",
  parent = dotplot_layout,
  properties = list(
    bin_method = new_property(
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

#' Swarm dotplot layout
#' @description
#' Beeswarm dotplot layout use with `bin_dots()`, `geom_dots()`, etc.
#' @inheritParams dotplot_layout
#' @return <[dotplot_layout]> object of class `layout_swarm`.
layout_swarm = new_dotplot_layout_class(
  "layout_swarm",
  parent = dotplot_layout
)

#' Stratified swarm dotplot layout
#' @description
#' Stratified beeswarm dotplot layout for use with `bin_dots()`, `geom_dots()`, etc.
#' @inheritParams dotplot_layout
#' @return <[dotplot_layout]> object of class `layout_swarm2`.
layout_swarm2 = new_dotplot_layout_class(
  "layout_swarm2",
  parent = dotplot_layout,
  properties = list(
    x = new_property(
      class_numeric,
      setter = function(self, value) {
        self@x = value
        make_swarm2_xs(self)
      },
      default = numeric()
    ),
    group = new_property(
      class_any,
      validator = validate_not_na,
      setter = function(self, value) {
        self@group = vec_recycle(value, length(self@x))
        make_swarm2_xs(self)
      },
      default = 1L
    ),
    xs = new_property(
      class_list,
      setter = NULL,
      getter = \(self) self@xs
    ),
    grid = new_property(
      class_numeric,
      validator = validate_positive_scalar_integerish,
      default = 4L
    )
  ),
  validator = \(self) {
    if (is.null(self@xs)) "x and group must both be set."
  }
)

#' Split x into groups so that `layout_swarm2` can plot groups in order
#' @description
#' Split `x` by `group` so that we don't have to recompute these splits when
#' doing `find_dotplot_binwidth()`. The stratified swarm layout differs from
#' others in that it needs these splits in order to plot groups in the right
#' order within each stratum.
#' @noRd
make_swarm2_xs = function(self) {
  if (!is.null(self@x) && !is.null(self@group)) {
    x_splits = vec_split(self@x, self@group)
    attr(self, "xs") = x_splits$val[order(x_splits$key)]
  }
  self
}
