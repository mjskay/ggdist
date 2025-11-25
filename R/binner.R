# dot binners for use with bin_dots
#
# Author: mjskay
###############################################################################
#' @include validate.R
NULL


# binner -----------------------------------------------------------------

#' Base class for dot plot binners created with `bin_dots()`
#' @description
#' Layout/binning method for dot plots created with `bin_dots()`.
#' @details
#' A `binner` defines how dots are arranged in a dot plot created with
#' `bin_dots()`. Different types of binners implement different layouts.
#' @param maxheight <scalar [numeric]> maximum height of the dots layout.
#' @param heightratio <scalar [numeric]> height ratio of the dots layout.
#' @param stackratio <scalar [numeric]> stack ratio of the dots layout.
#' @param side <[string][character]> side where the dots layout should be placed.
#' @param orientation <[string][character]> orientation of the dots layout.
#' @param overlaps <[string][character]> how to handle overlaps in the dots layout.
#' @return An object of class `binner`.
#' @import S7
#' @noRd
binner = new_class(
  "binner",
  properties = list(
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
    )
  )
)


# binner setup -----------------------------------------------------------

#' Create a new dot binner
#' @param layout <[string][character]> name of layout as passed to `bin_dots()`.
#' @param ... Additional arguments passed to the binner constructor.
#' @return An object of the specified `binner` class.
#' @noRd
new_binner = function(layout, ...) {
  match_function(layout, "binner_")(...)
}

#' Prepare binner for data
#' @description
#' This generic function updates a dot binner based on the provided data points.
#' Used for pre-calculations that depend on the data that can be used
#' to speed up automatic binwidth selection.
#' @param binner <`binner`> dot binner to update.
#' @param x <[numeric]> numeric vector of data points.
#' @return An updated `binner`.
#' @noRd
prepare_binner = new_generic("prepare_binner", c("binner"), function(binner, x, ...) {
  S7_dispatch()
})

method(prepare_binner, binner) = function(binner, x, ...) {
  binner
}


# bin-based layouts ----------------------------------------------------------------

# TODO: remove
automatic_bin = function(x, width, binner = binner_bin()) {
  prepare_binner(binner, x)@bin_method(x, width)[c("bins", "bin_midpoints")]
}

#' Bin layout
#' @description
#' Wilkinson-esque binner for dot plots created with `bin_dots()`.
#' @inheritParams binner
#' @param bin_method <function> function that takes data and bin width as input and returns a list with components `bins` and `bin_midpoints`.
#' @param align_rows <logical> whether to align rows of dots when `side` is "both".
#' @return An object of class `binner_bin`.
#' @noRd
binner_bin = new_class(
  "binner_bin",
  parent = binner,
  properties = list(
    bin_method = new_property(
      class_function,
      default = automatic_bin
    ),
    align_rows = new_property(
      class_logical,
      getter = function(self) FALSE
    )
  )
)

method(prepare_binner, binner_bin) = function(binner, x, ...) {
  # examines a vector of data and determines an appropriate binning method based on its properties
  # doing this up front allows us to doing this repeatedly when finding binwidth via optimization
  diff_x = diff(x)
  if (isTRUE(all.equal(diff_x, rev(diff_x), check.attributes = FALSE))) {
    # x is symmetric, use centered binning
    binner@bin_method = wilkinson_bin_from_center
  } else {
    binner@bin_method = wilkinson_bin
  }
  binner
}

#' Weave binner
#' @description
#' Weave `binner` for dot plots created with `bin_dots()`.
#' @inheritParams binner
#' @return An object of class `binner_weave`.
#' @noRd
binner_weave = new_class(
  "binner_weave",
  parent = binner_bin,
  properties = list(
    align_rows = new_property(
      class_logical,
      getter = function(self) TRUE
    )
  )
)

#' Hex binner
#' @description
#' Hex `binner` for dot plots created with `bin_dots()`.
#' @inheritParams binner
#' @return An object of class `binner_hex`.
#' @noRd
binner_hex = new_class(
  "binner_hex",
  parent = binner_bin,
  properties = list(
    align_rows = new_property(
      class_logical,
      getter = function(self) TRUE
    )
  )  
)


# bar binner -------------------------------------------------------------

#' Bin dots into bars
#' @param x data (original positions of dots)
#' @param width width of the bins in data units
#' @param bar_scale width of the bars as a proportion of the data resolution
#' @noRd
bar_bin = function(x, width, bar_scale = 0.9) {
  # determine the amount of space that each bar will take up
  # TODO: can drop as.numeric here if https://github.com/tidyverse/ggplot2/issues/5709 is fixed
  max_bar_width = resolution(as.numeric(x), zero = FALSE) * bar_scale
  n_bins = max(floor(max_bar_width / width), 1)
  actual_bar_width = n_bins * width

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

#' Bar binner
#' @description
#' Bar `binner` for dot plots created with `bin_dots()`.
#' @inheritParams binner
#' @return An object of class `binner_bar`.
#' @noRd
binner_bar = new_class(
  "binner_bar",
  parent = binner_bin,
  properties = list(
    bin_method = new_property(
      class_function,
      getter = function(self) bar_bin
    ),
    overlaps = new_property(
      class_character,
      getter = function(self) "keep"
    ),
    align_rows = new_property(
      class_logical,
      getter = function(self) TRUE
    )
  )
)

method(prepare_binner, binner_bar) = function(binner, x, ...) {
  binner
}


# swarm binners ----------------------------------------------------------

#' Swarm binner
#' @description
#' Swarm `binner` for dot plots created with `bin_dots()`.
#' @inheritParams binner
#' @return An object of class `binner_swarm`.
#' @noRd
binner_swarm = new_class(
  "binner_swarm",
  parent = binner,
  properties = list(
    # TODO: remove this default method when dots_heap is refactored not to call this
    bin_method = new_property(
      class_function,
      default = automatic_bin
    )
  )
)

#' Swarm2 binner
#' @description
#' Swarm2 `binner` for dot plots created with `bin_dots()`.
#' @inheritParams binner
#' @return An object of class `binner_swarm2`.
#' @noRd
binner_swarm2 = new_class(
  "binner_swarm2",
  parent = binner_swarm
)
