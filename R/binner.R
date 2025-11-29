# dot binners for use with bin_dots
#
# Author: mjskay
###############################################################################
#' @include validate.R
NULL


# binner -----------------------------------------------------------------

new_binner_class = function(...) auto_partial(new_class(...), required = "x")

#' Base class for dot plot binners created with `bin_dots()`
#' @description
#' Layout/binning method for dot plots created with `bin_dots()`.
#' @details
#' A `binner` defines how dots are arranged in a dot plot created with
#' `bin_dots()`. Different types of binners implement different layouts.
#' @param x <[numeric]> Positions of dots.
#' @param maxheight <scalar [numeric]> maximum height of the dots layout.
#' @param heightratio <scalar [numeric]> height ratio of the dots layout.
#' @param stackratio <scalar [numeric]> stack ratio of the dots layout.
#' @param side <[string][character]> side where the dots layout should be placed.
#' @param orientation <[string][character]> orientation of the dots layout.
#' @param overlaps <[string][character]> how to handle overlaps in the dots layout.
#' @return An object of class `binner`.
#' @import S7
#' @noRd
binner = new_binner_class(
  "binner",
  abstract = TRUE,
  properties = list(
    x = new_property(
      class_numeric,
      default = double()
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
    )
  )
)

#' Create a new dot binner
#' @param layout <[string][character]> name of layout as passed to `bin_dots()`.
#' @param ... Additional arguments passed to the binner constructor.
#' @return An object of the specified `binner` class.
#' @noRd
new_binner = function(layout, ...) {
  match_function(layout, "binner_")(...)
}


# bin-based layouts ----------------------------------------------------------------

#' Bin layout
#' @description
#' Wilkinson-esque binner for dot plots created with `bin_dots()`.
#' @inheritParams binner
#' @param bin_method <function> function that takes data and bin width as input and returns a list with components `bins` and `bin_midpoints`.
#' @param align_rows <logical> whether to align rows of dots when `side` is "both".
#' @return An object of class `binner_bin`.
#' @noRd
binner_bin = new_binner_class(
  "binner_bin",
  parent = binner,
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
      getter = function(self) FALSE
    )
  ),
  constructor = binner@constructor
)

#' Weave binner
#' @description
#' Weave `binner` for dot plots created with `bin_dots()`.
#' @inheritParams binner
#' @return An object of class `binner_weave`.
#' @noRd
binner_weave = new_binner_class(
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
binner_hex = new_binner_class(
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
binner_bar = new_binner_class(
  "binner_bar",
  parent = binner,
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
    )
  )
)


# swarm binners ----------------------------------------------------------

#' Swarm binner
#' @description
#' Swarm `binner` for dot plots created with `bin_dots()`.
#' @inheritParams binner
#' @return An object of class `binner_swarm`.
#' @noRd
binner_swarm = new_binner_class(
  "binner_swarm",
  parent = binner
)

#' Swarm2 binner
#' @description
#' Swarm2 `binner` for dot plots created with `bin_dots()`.
#' @inheritParams binner
#' @return An object of class `binner_swarm2`.
#' @noRd
binner_swarm2 = new_binner_class(
  "binner_swarm2",
  parent = binner_swarm,
  properties = list(
    grid = new_property(
      class_numeric,
      validator = validate_positive_scalar_integerish,
      default = 4L
    )
  )
)
