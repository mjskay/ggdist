#' @include validate.R
NULL

# dots layouts for use with bin_dots
#
# Author: mjskay
###############################################################################



# dots_layout -----------------------------------------------------------------

#' Base class for dot plot layouts created with `bin_dots()`
#' @description
#' This class defines the layout parameters for dot plots created with `bin_dots()`.
#' @param maxheight <scalar [numeric]> maximum height of the dots layout.
#' @param heightratio <scalar [numeric]> height ratio of the dots layout.
#' @param stackratio <scalar [numeric]> stack ratio of the dots layout.
#' @param side <[string][character]> side where the dots layout should be placed.
#' @param orientation <[string][character]> orientation of the dots layout.
#' @param overlaps <[string][character]> how to handle overlaps in the dots layout.
#' @return An object of class `dots_layout`.
#' @import S7
#' @noRd
dots_layout = new_class(
  "dots_layout",
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


# layout setup -----------------------------------------------------------

#' Create a new dots layout
#' @param layout <[string][character]> type of layout to create. One of "weave", "bin", "hex", "swarm", "swarm2", or "bar".
#' @param ... Additional arguments passed to the layout constructor.
#' @return An object of the specified layout class.
#' @noRd
new_layout = function(layout, ...) {
  match_function(layout, "layout_")(...)
}

#' Prepare layout for data
#' @description
#' This generic function updates a layout object based on the provided data points.
#' Used for pre-calculations that depend on the data that can be used
#' to speed up automatic binwidth selection.
#' @param layout <`dots_layout`> layout object to update.
#' @param x <[numeric]> numeric vector of data points.
#' @return An updated layout object.
#' @noRd
prepare_layout_for_data = new_generic("prepare_layout_for_data", c("layout"), function(layout, x, ...) {
  S7_dispatch()
})

method(prepare_layout_for_data, dots_layout) = function(layout, x, ...) {
  layout
}


# bin-based layouts ----------------------------------------------------------------

# TODO: remove
automatic_bin = function(x, width, layout = layout_bin()) {
  prepare_layout_for_data(layout, x)@bin_method(x, width)[c("bins", "bin_midpoints")]
}

#' Bin layout
#' @description
#' This class defines a bin layout for dot plots created with `bin_dots()`.
#' It inherits from the `dots_layout` class.
#' @inheritParams dots_layout
#' @param bin_method <function> function that takes data and bin width as input and returns a list with components `bins` and `bin_midpoints`.
#' @param align_rows <logical> whether to align rows of dots when `side` is "both".
#' @return An object of class `layout_bin`.
#' @noRd
layout_bin = new_class(
  "layout_bin",
  parent = dots_layout,
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

method(prepare_layout_for_data, layout_bin) = function(layout, x, ...) {
  # examines a vector of data and determines an appropriate binning method based on its properties
  # doing this up front allows us to doing this repeatedly when finding binwidth via optimization
  diff_x = diff(x)
  if (isTRUE(all.equal(diff_x, rev(diff_x), check.attributes = FALSE))) {
    # x is symmetric, use centered binning
    layout@bin_method = wilkinson_bin_from_center
  } else {
    layout@bin_method = wilkinson_bin
  }
  layout
}

#' Weave layout
#' @description
#' This class defines a weave layout for dot plots created with `bin_dots()`.
#' It inherits from the `dots_layout` class.
#' @inheritParams dots_layout
#' @return An object of class `layout_weave`.
#' @noRd
layout_weave = new_class(
  "layout_weave",
  parent = layout_bin,
  properties = list(
    align_rows = new_property(
      class_logical,
      getter = function(self) TRUE
    )
  )
)

#' Hex layout
#' @description
#' This class defines a hex layout for dot plots created with `bin_dots()`.
#' It inherits from the `dots_layout` class.
#' @inheritParams dots_layout
#' @return An object of class `layout_hex`.
#' @noRd
layout_hex = new_class(
  "layout_hex",
  parent = layout_bin,
  properties = list(
    align_rows = new_property(
      class_logical,
      getter = function(self) TRUE
    )
  )  
)


# bar layout -------------------------------------------------------------

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

#' Bar layout
#' @description
#' This class defines a bar layout for dot plots created with `bin_dots()`.
#' It inherits from the `dots_layout` class.
#' @inheritParams dots_layout
#' @return An object of class `layout_bar`.
#' @noRd
layout_bar = new_class(
  "layout_bar",
  parent = layout_bin,
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

method(prepare_layout_for_data, layout_bar) = function(layout, x, ...) {
  layout
}


# swarm layouts ----------------------------------------------------------

#' Swarm layout
#' @description
#' This class defines a swarm layout for dot plots created with `bin_dots()`.
#' It inherits from the `dots_layout` class.
#' @inheritParams dots_layout
#' @return An object of class `layout_swarm`.
#' @noRd
layout_swarm = new_class(
  "layout_swarm",
  parent = dots_layout,
  properties = list(
    # TODO: remove this default method when dots_heap is refactored not to call this
    bin_method = new_property(
      class_function,
      default = automatic_bin
    )
  )
)

#' Swarm2 layout
#' @description
#' This class defines a swarm2 layout for dot plots created with `bin_dots()`.
#' It inherits from the `dots_layout` class.
#' @inheritParams dots_layout
#' @return An object of class `layout_swarm2`.
#' @noRd
layout_swarm2 = new_class(
  "layout_swarm2",
  parent = layout_swarm
)
