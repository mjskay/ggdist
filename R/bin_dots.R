# dotplot layout methods for use with dots geom
#
# Author: mjskay
###############################################################################
#' @include dotplot_layout.R
NULL


# bin_dots -----------------------------------------------------------------

#' Bin data values using a dotplot layout algorithm
#'
#' @description
#' Bins the provided data values using one of several dotplot layout algorithms.
#' @param x <[numeric]> *x* values.
#' @param y <[numeric]> *y* values (same length as `x`).
#' @param binwidth <scalar [numeric]> Bin width.
#' @param heightratio <scalar [numeric]> Ratio of bin width to dot height
#' @param stackratio <scalar [numeric]> Ratio of dot height to vertical distance
#' between dot centers
#' @eval rd_param_dots_layout()
#' @eval rd_param_dots_overlaps()
#' @eval rd_param_dots_span()
#' @eval rd_param_side("dots")
#' @param orientation <[string][character]> Whether the dots are laid out horizontally
#' or vertically. Follows the naming scheme of [geom_slabinterval()]:
#'
#'   - `"horizontal"` assumes the data values for the dotplot are in the `x`
#'   variable and that dots will be stacked up in the `y` direction.
#'   - `"vertical"` assumes the data values for the dotplot are in the `y`
#'   variable and that dots will be stacked up in the `x` direction.
#'
#'  For compatibility with the base ggplot naming scheme for `orientation`,
#' `"x"` can be used as an alias for `"vertical"` and `"y"` as an alias for
#' `"horizontal"`.
#'
#' @return
#' A `data.frame` with three columns:
#'
#' - `x`: the x position of each dot
#' - `y`: the y position of each dot
#' - `bin`: a unique number associated with each bin
#'   (supplied but not used when `layout = "swarm"`)
#'
#' @seealso [find_dotplot_binwidth()] for an algorithm that finds good bin widths
#' to use with this function; [geom_dotsinterval()] for geometries that use
#' these algorithms to create dotplots.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' x = qnorm(ppoints(20))
#' bin_df = bin_dots(x = x, y = 0, binwidth = 0.5, heightratio = 1)
#' bin_df
#'
#' # we can manually plot the dotplot, though this is only recommended
#' # if you are using find_dotplot_binwidth() and bin_dots() to build your own
#' # grob. For practical use it is much easier to use geom_dots(), which will
#' # automatically select good bin widths for you (and which uses
#' # find_dotplot_binwidth() and bin_dots() internally)
#' bin_df %>%
#'   ggplot(aes(x = x, y = y)) +
#'   geom_point(size = 4) +
#'   coord_fixed()
#'
#' @export
bin_dots = function(
  x,
  y,
  binwidth,
  group = 1L,
  heightratio = 1,
  stackratio = 1,
  layout = c("bin", "weave", "hex", "swarm", "swarm2", "bar"),
  side = c("topright", "top", "right", "bottomleft", "bottom", "left", "topleft", "bottomright", "both"),
  orientation = c("horizontal", "vertical", "y", "x"),
  overlaps = "nudge",
  span = waiver()
) {
  orientation = match.arg(orientation)
  flip = switch_orientation(orientation, horizontal = FALSE, vertical = TRUE)
  side = switch_side(match.arg(side), orientation,
    topright = "top",
    bottomleft = "bottom",
    both = "both"
  )

  # from this point until we return, `dots$x` is always data and `dots$y` is
  # always offset/height in bin
  dots = setup_dots(x, y, group, flip)

  # bin the dots
  layout = new_dotplot_layout(
    layout,
    dots = dots,
    heightratio = heightratio,
    stackratio = stackratio,
    side = side,
    overlaps = overlaps,
    span = span
  )
  dotplot = setup_dotplot(layout, binwidth = binwidth)
  dots = place_dots(layout, dotplot)

  # restore the original data order in case it was destroyed
  dots = dots[order(dots$order), ]
  dots$order = NULL

  flip_data(dots, flip)
}

#' Setup a data frame of dots for use with dotplot layout algorithms
#' @noRd
setup_dots = function(x, y, group = 1L, flip = FALSE) {
  dots = data_frame0(x = x, y = y, group = rep_len(xtfrm(group), length(x)))
  dots = flip_data(dots, flip)
  stopifnot("All y values must be equal" = dots$y == dots$y[1])

  # Sort the x values, because they must be sorted for bin methods to maintain
  # the correct connection between input values and output bins.
  # Because of this (and other later grouping operations that may re-order the
  # data as well) we need to keep the original data order around so that
  # we can restore the original order at the end.
  dots$order = seq_len(nrow(dots))
  dots[order(dots$x, dots$group), ]
}

# setup_dotplot -----------------------------------------------------------

#' Initialize a dotplot layout
#'
#' @description
#' Calculate the parameters of a dotplot layout, such as the bin width, number of bins (if
#' using a binned layout), what the y spacing between dots should be, and what the height of the
#' tallest bin is. The resulting parameters can be used to with `place_dots()` to finalize the
#' dotplot layout.
#'
#' This method is separate from `place_dots()` so that it can be used by numerical optimization methods
#' in `find_dotplot_binwidth()` without needing to fully lay out a dotplot on each iteration. Therefore,
#' it should ideally do only as much work as is needed to determine the maximum `height` of the dotplot
#' and leave the remaining work required to lay out the dots to `place_dots()`.
#' @param layout <[dotplot_layout]> the layout (encapsulating a dotplot layout algorithm and the data
#' to be laid out).
#' @param nbins,binwidth <scalar [numeric]> provide either the desired number of bins (`nbins`)
#' or the desired bin width (`binwidth`); given one the other will be calculated.
#' @return <[list]> properties of a dotplot applied to the input data with the given layout:
#' - `nbins`: <scalar [integer]> number of bins
#' - `binwidth`: <scalar [numeric]> bin width
#' - `y_spacing`: <scalar [numeric]> vertical distance between dot centers
#' - `y_start`: <scalar [numeric]> starting y offset for the first dot in each bin
#' Subclasses may also add additional elements. They *must* add at least the following elements:
#' - `height`: <scalar [numeric]> height of the tallest bin in this dotplot
#' @noRd
setup_dotplot = new_generic("setup_dotplot", c("layout"), function(layout, nbins = NULL, binwidth = NULL, ...) {
  S7_dispatch()
})

method(setup_dotplot, dotplot_layout) = function(layout, nbins = NULL, binwidth = NULL, ...) {
  # determine binwidth and number of bins
  x_spread = diff(range(layout@dots$x))
  if (x_spread == 0) x_spread = 1
  if (is.null(binwidth)) {
    nbins = floor(nbins)
    binwidth = x_spread / nbins
  } else {
    nbins = max(floor(x_spread / binwidth), 1)
  }

  # determine y positioning parameters
  y_spacing = binwidth * layout@heightratio
  y_start = switch(layout@side,
    top = y_spacing / layout@stackratio / 2,
    bottom = - y_spacing / layout@stackratio / 2,
    both = 0
  )

  list(
    nbins = nbins,
    binwidth = binwidth,
    y_spacing = y_spacing,
    y_start = y_start
  )
}

## setup_dotplot for bin, hex, weave, bar ----------------------------------

#' Initialize binned dotplot layouts
#' @param layout <`layout_bin` | `layout_bar`> the dotplot layout
#' @param nbins,binwidth <scalar [numeric]> provide either the desired number of bins (`nbins`)
#' or the desired bin width (`binwidth`); given one the other will be calculated.
#' @return <[list]> properties of this dotplot (see `setup_dotplot()`), with additional elements:
#' - `bins`: <[integer]> vector of same length as `x` giving the bin number (in {1 ... `nbins`}) for each element in `x`
#' - `bin_midpoints`: <[numeric]> vector of length `nbins` giving the midpoint of each bin
#' - `bin_counts`: <[integer]> vector of length `nbins` giving the number of elements in each bin
#' - `height`: <scalar [numeric]> height of the tallest bin in this dotplot
#' @noRd
method(setup_dotplot, layout_bin | layout_bar) = function(layout, nbins = NULL, binwidth = NULL, ...) {
  dotplot = setup_dotplot(super(layout, dotplot_layout), nbins, binwidth)
  dotplot = c(dotplot, layout@bin_method(layout@dots$x, dotplot$binwidth, span = layout@span))

  # determine height of the tallest bin
  dotplot$bin_counts = tabulate(dotplot$bins)
  # max bin count is the max "effective" number of elements in a bin, which
  # is the number of elements in the bin modified by the stackratio to account
  # for how dots align with tops and bottoms of stacks when stackratio != 1
  max_bin_count = max(dotplot$bin_counts) - 1 + 1/layout@stackratio
  dotplot$height = max_bin_count * dotplot$y_spacing

  dotplot
}

## setup_dotplot for swarm, swarm2 -----------------------------------------

#' Initialize swarm dotplot layout
#' @param layout <`layout_swarm`> the dotplot layout
#' @param nbins,binwidth <scalar [numeric]> provide either the desired number of bins (`nbins`)
#' or the desired bin width (`binwidth`); given one the other will be calculated.
#' @return <[list]> properties of this dotplot  (see `setup_dotplot()`), with additional elements:
#' - `dots`: <[data.frame]> data frame with `x` and `y` columns giving the positions of dots in the swarm
#' - `height`: <scalar [numeric]> height of the tallest bin in this dotplot
#' @noRd
method(setup_dotplot, layout_swarm) = function(layout, nbins = NULL, binwidth = NULL, ...) {
  stop_if_not_installed("beeswarm", '{.help ggdist::geom_dots}(layout = "swarm")')

  dotplot = setup_dotplot(super(layout, dotplot_layout), nbins, binwidth)
  dotplot$dots = beeswarm::swarmy(
    layout@dots$x,
    0,
    xsize = dotplot$binwidth,
    ysize = dotplot$y_spacing,
    log = "",
    cex = 1,
    side = switch(layout@side, top = 1, bottom = -1, both = 0),
    compact = TRUE
  )
  dotplot$dots = recenter_swarm_clusters(layout, dotplot, dotplot$dots)
  dotplot$height = get_swarm_height(layout, dotplot, dotplot$dots)

  dotplot
}

#' Initialize stratified swarm dotplot layout
#' @param layout <`layout_swarm2`> the dotplot layout
#' @param nbins,binwidth <scalar [numeric]> provide either the desired number of bins (`nbins`)
#' or the desired bin width (`binwidth`); given one the other will be calculated.
#' @return <[list]> properties of this dotplot  (see `setup_dotplot()`), with additional elements:
#' - `dots`: <[data.frame]> data frame with `x` and `y` columns giving the positions of dots in the swarm
#' - `height`: <scalar [numeric]> height of the tallest bin in this dotplot
#' @noRd
method(setup_dotplot, layout_swarm2) = function(layout, nbins = NULL, binwidth = NULL, ...) {
  dotplot = setup_dotplot(super(layout, dotplot_layout), nbins, binwidth)
  dotplot$dots = grid_swarm(
    layout@xs,
    0,
    xsize = dotplot$binwidth,
    ysize = dotplot$y_spacing,
    ygrid = layout@grid,
    side = switch(layout@side, top = 1, bottom = -1, both = 0)
  )
  dotplot$dots = recenter_swarm_clusters(layout, dotplot, dotplot$dots)
  dotplot$height = get_swarm_height(layout, dotplot, dotplot$dots)

  dotplot
}

#' Re-center swarm clusters for side = "both" in swarm layouts
#' @param layout <[dotplot_layout]> the dotplot layout
#' @param dotplot <[list]> dotplot properties as returned by `setup_dotplot()`
#' @param dots <[data.frame]> dot positions with `x` and `y` columns, where `x`
#' is always the data values and `y` is the vertical position assigned by the swarm algorithm.
#' @noRd
recenter_swarm_clusters = function(layout, dotplot, dots) {
  if (layout@side != "both") return(dots)

  # re-center contiguous clusters around their mean y position so that
  # small clusters are visually centered (rather than e.g. a cluster of
  # two points having one point on the origin line and one above it)
  dots$y = recenter_swarm_clusters_(dots$x, dots$y, dotplot$binwidth)
  dots
}

#' Get the height of a swarm dotplot layout
#' @param layout <[dotplot_layout]> the dotplot layout
#' @param dotplot <[list]> dotplot properties as returned by `setup_dotplot()`
#' @param dots <[data.frame]> dot positions with `x` and `y` columns, where `x`
#' is always the data values and `y` is the vertical position assigned by the swarm algorithm.
#' @returns <scalar [numeric]> height of the dotplot
#' @noRd
get_swarm_height = function(layout, dotplot, dots) {
  height_minus_1_dot = max(abs(dotplot$dots$y)) * if (layout@side == "both") 2 else 1
  dot_height = dotplot$y_spacing / layout@stackratio
  height_minus_1_dot + dot_height
}


# place_dots -------------------------------------------------------------

#' Find the x and y positions of dots in a dotplot
#' @param layout <[dotplot_layout]> the dotplot layout algorithm
#' @param dotplot <[list]> dotplot properties as returned by `setup_dotplot()`
#' @param dots <[data.frame]> dot positions with `x` and `y` columns, where `x`
#' is always the data values and `y` is the vertical position assigned by the swarm algorithm.
#' @returns <[data.frame]> modified version of `dots` with updated `x` and `y` columns
#' @noRd
place_dots = new_generic("place_dots", c("layout"), function(layout, dotplot) {
  S7_dispatch()
})

## place_dots for bin, hex, weave, bar ---------------------------------------

method(place_dots, layout_bin | layout_bar) = function(layout, dotplot) {
  dots = layout@dots
  dots$bin = dotplot$bins
  dots = place_dots_x_binned(layout, dotplot, dots)
  dots = place_dots_y_binned(layout, dotplot, dots)
  dots
}

method(place_dots, layout_hex) = function(layout, dotplot) {
  dots = place_dots(super(layout, layout_bin), dotplot)

  dots = ddply_(dots, "bin", function(bin_df) {
    n_dots = nrow(bin_df)
    row_start_offset = get_row_start_offset(layout, dotplot, n_dots)
    # depending on whether this is an even or odd column, need to start the
    # x offset to the left or to the right
    x_offset_start = if (row_start_offset %% 2 == 0) 1 else -1
    bin_df$x = bin_df$x + rep_len(c(-0.25, 0.25) * x_offset_start, n_dots) * dotplot$binwidth
    bin_df
  })

  dots
}

method(place_dots, layout_weave) = function(layout, dotplot) {
  dots = layout@dots
  dots$bin = dotplot$bins

  # keep original x positions, but re-order within bins so that overlaps
  # across bins are less likely
  dots = ddply_(dots, "bin", function(bin_df) {
    seq_fun = if (layout@side == "both") seq_interleaved_centered_grouped else seq_interleaved_grouped
    bin_df = bin_df[seq_fun(bin_df$group),]
    bin_df$row = seq_len(nrow(bin_df))
    if (layout@side == "both") bin_df$row = bin_df$row - round((nrow(bin_df) - 1) / 2)
    bin_df
  })

  if (layout@overlaps == "nudge") {
    # nudge values within each row to ensure there are no overlaps
    dots = ddply_(dots, "row", function(row_df) {
      row_df$x = nudge_bins(row_df$x, dotplot$binwidth)
      row_df
    })
  }

  dots$row = NULL

  if (layout@side == "both") {
    dots$y_orig = dots$y
    dots = place_dots_y_binned(layout, dotplot, dots)
    dots = dots[order(dots$x), ]
    dots = recenter_swarm_clusters(layout, dotplot, dots)
    dots$y = dots$y + dots$y_orig
    dots$y_orig = NULL
  } else {
    dots = place_dots_y_binned(layout, dotplot, dots)
  }

  dots
}

#' Find the x positions of dots in binned layouts
#' @param layout <[dotplot_layout]> dotplot layout
#' @param dotplot <[list]> dotplot properties as returned by `setup_dotplot()`
#' @param dots <[data.frame]> dot positions with at least `x`, `y`, `bin`, and `order` columns
#' @returns <[data.frame]> modified version of `dots` with updated `x` column
#' @noRd
place_dots_x_binned = function(layout, dotplot, dots) {
  bin_midpoints = dotplot$bin_midpoints
  if (layout@overlaps == "nudge") {
    bin_midpoints = nudge_bins(bin_midpoints, dotplot$binwidth, dotplot$bin_counts)
  }
  dots$x = bin_midpoints[dotplot$bins]
  # maintain original data order within each bin when finding y positions
  dots = dots[order(dots$bin, dots$order), ]
  dots
}

#' Find the y positions of dots in binned layouts
#' @param layout <[dotplot_layout]> dotplot layout
#' @param dotplot <[list]> dotplot properties as returned by `setup_dotplot()`
#' @param dots <[data.frame]> dot positions with at least `x`, `y`, `bin`, and `order` columns
#' @returns <[data.frame]> modified version of `dots` with updated `y` column
#' @noRd
place_dots_y_binned = function(layout, dotplot, dots) {
  dots = ddply_(dots, "bin", function(bin_df) {
    y_offset = seq(
      0,
      dotplot$y_spacing * (nrow(bin_df) - 1),
      length.out = nrow(bin_df)
    )
    row_start_offset = get_row_start_offset(layout, dotplot, nrow(bin_df))
    switch(layout@side,
      top = {},
      bottom = {
        y_offset = -y_offset
      },
      both = {
        y_offset = y_offset - dotplot$y_spacing * row_start_offset
      }
    )
    bin_df$y = bin_df$y + dotplot$y_start + y_offset

    bin_df
  })
  dots
}

#' Get the number of rows the start of a dot column will be offset by
#' @param layout <[dotplot_layout]> dotplot layout
#' @param dotplot <[list]> dotplot properties as returned by `setup_dotplot()`
#' @param n_dots <[integer]> number of dots in the column
#' @returns <[integer]> number of rows the start of the column is offset by
#' @noRd
get_row_start_offset = function(layout, dotplot, n_dots) {
  if (layout@side == "both") {
    row_start_offset = (n_dots - 1) / 2
    if (layout@align_rows) {
      # weave and hex require rows to be aligned exactly so that x offsets
      # can be applied within rows; bar because it looks weird otherwise
      row_start_offset = round(row_start_offset)
    }
    row_start_offset
  } else {
    0
  }
}

## place_dots for swarm, swarm2 -------------------------------------------------

method(place_dots, layout_swarm | layout_swarm2) = function(layout, dotplot) {
  dots = layout@dots
  dots$x = dotplot$dots$x
  dots$y = dots$y + dotplot$y_start + dotplot$dots$y
  dots
}


# modified wilkinson methods ----------------------------------------------

#' a variant of the basic wilkinson binned layout method, a single left-to-right sweep
#' @param x sorted numeric vector
#' @param binwidth bin width
#' @noRd
wilkinson_bin_to_right = function(x, binwidth) {
  if (length(x) == 0) {
    return(list(
      bins = integer(0),
      bin_midpoints = numeric(0),
      bin_left = numeric(0),
      bin_right = numeric(0)
    ))
  }

  # determine bins
  bins = wilkinson_bin_to_right_(x, binwidth)

  # determine bin positions
  bin_runs = rle_bins(bins)
  locate_bins(bin_runs, x)
}

#' do a backwards sweep after a left-to-right wilkinson binning, trying to
#' eliminate extra space at the end of the binning by eating up slack between
#' bins
#' @param x sorted numeric vector
#' @param b a binning returned by wilkinson_bin_to_right
#' @param binwidth bin width
#' @param first_slack max amount of slack on the first bin
#' @noRd
wilkinson_sweep_back = function(x, b, binwidth, first_slack = Inf) {
  n_bin = length(b$bin_left)
  if (n_bin < 2) return(b)

  # amount we want to move left is the extra space at the end of the last bin
  move_left = binwidth - b$bin_right[[n_bin]] + b$bin_left[[n_bin]] - .Machine$double.eps
  if (move_left <= 0) return(b)

  # slack is the distance between bins
  slack = b$bin_left[-1] - (b$bin_left[-n_bin] + binwidth)

  # slack on first bin is at most half the move_left amount; this makes it so that
  # in the worst case we are compromising between first and last bins being
  # optimal
  first_slack = min(first_slack, move_left/2)
  slack = c(first_slack, slack)

  # the sum of all slack is the max amount we can move bins left by
  total_slack = sum(slack)
  move_left = min(move_left, total_slack)

  # move bins left, using up slack until we have achieved our desired
  # total move_left amount
  for (j in seq(n_bin, 1)) {
    b$bin_left[[j]] = b$bin_left[[j]] - move_left
    move_left = move_left - slack[[j]]
    if (move_left < 0) break
  }
  min_changed_bin = max(j - 1, 1)
  changed_bin_is = min_changed_bin:n_bin
  changed_x_is = which(b$bin_left[[min_changed_bin]] <= x)

  # re-bin xs in the changed region into new bins
  x_changed = x[changed_x_is]
  bins_changed = findInterval(x_changed, b$bin_left[changed_bin_is]) + min_changed_bin - 1
  b$bins[changed_x_is] = bins_changed

  # re-number bins to be consecutive in case some bins got removed completely
  bin_runs = rle_bins(b$bins)
  bin_runs = renumber_bins(bin_runs)
  locate_bins(bin_runs, x)
}

#' a rightward or leftward wilkinson binning followed by a backwards sweep to
#' reduce edge effects by taking up slack in the binning (spaces between bins)
#' @param x numeric vector
#' @param binwidth bin width
#' @param span adjacent bin smoothing window as a fraction of `binwidth`
#' @param right bin left-to-right (TRUE) or right-to-left (FALSE)?
#' @param first_slack maximum slack on the first bin (passed to wilkinson_sweep_back)
#' @noRd
wilkinson_bin = function(x, binwidth, span = 0, right = TRUE, first_slack = Inf) {
  if (length(x) == 0) {
    return(list(
      bins = integer(0),
      bin_midpoints = numeric(0)
    ))
  }

  if (right) {
    b = wilkinson_bin_to_right(x, binwidth)
    b = wilkinson_sweep_back(x, b, binwidth, first_slack = first_slack)
  } else {
    rev_x = -rev(x)
    b = wilkinson_bin_to_right(rev_x, binwidth)
    b = wilkinson_sweep_back(rev_x, b, binwidth, first_slack = first_slack)
    b = list(
      # renumber bins so 1,2,3,3 => 3,2,1,1 (then reverse so it matches original vector order)
      bins = rev(max(b$bins) + 1 - b$bins),
      bin_midpoints = -rev(b$bin_midpoints)
    )
  }

  wilkinson_smooth(x, b, binwidth, span = span)
}

#' A modified wilkinson-style binned layout that expands outward from the center of
#' the data. Works best on symmetric data.
#'  x must be sorted
#' @param x numeric vector
#' @param binwidth bin width
#' @param span adjacent bin smoothing window as a fraction of `binwidth`
#' @noRd
wilkinson_bin_from_center = function(x, binwidth, span = 0) {
  if (length(x) == 0) {
    list(
      bins = integer(0),
      bin_midpoints = numeric(0)
    )
  } else if (length(x) == 1 || abs(x[[length(x)]] - x[[1]]) < binwidth) {
    # everything is in 1 bin
    list(
      bins = rep(1, length(x)),
      bin_midpoints = (x[[1]] + x[[length(x)]]) / 2
    )
  } else {
    # > 1 bin
    if (length(x) %% 2 == 0) {
      # even number of items
      if (x[[length(x)/2]] != x[[length(x)/2 + 1]]) {
        # even number of items and items in middle not equal => even number of bins and
        # we bin out from center on either side of the middle
        first_slack = (x[[length(x)/2 + 1]] - x[[length(x)/2]])/2
        left = wilkinson_bin(x[1:(length(x)/2)], binwidth, right = FALSE, first_slack = first_slack)
        right = wilkinson_bin(x[(length(x)/2 + 1):length(x)], binwidth, first_slack = first_slack)
        return(list(
          bins = c(left$bins, length(left$bin_midpoints) + right$bins),
          bin_midpoints = c(left$bin_midpoints, right$bin_midpoints)
        ))
      } else {
        # even number of items and center two items are equal, stick them into a single bin together
        # and make that the center bin
        edge_offset_from_center = 0.5
      }
    } else {
      # odd number of items => odd number of bins
      edge_offset_from_center = 0
    }

    # if we made it this far there is either an odd number of items OR an even number of items
    # where the center two items are equal to each other. In both of these cases we construct
    # a center bin first and then bin out from around it.
    center_i = length(x) / 2 + 0.5
    for (offset in (1:floor(length(x) / 2)) - edge_offset_from_center) {
      if (abs(x[[center_i + offset]] - x[[center_i - offset]]) < binwidth) {
        # can add both points
        edge_offset_from_center = offset
      } else {
        break
      }
    }
    n_center = 1 + edge_offset_from_center * 2 # number of items in center bin
    center_midpoint = (x[[center_i - edge_offset_from_center]] + x[[center_i + edge_offset_from_center]])/2

    # construct bins for regions left / right of center
    left = wilkinson_bin(
      x[1:(center_i - edge_offset_from_center - 1)], binwidth, right = FALSE,
      first_slack = x[[center_i - edge_offset_from_center]] - x[[center_i - edge_offset_from_center - 1]]
    )
    right = wilkinson_bin(
      x[(center_i + edge_offset_from_center + 1):length(x)], binwidth,
      first_slack = x[[center_i + edge_offset_from_center + 1]] - x[[center_i + edge_offset_from_center]]
    )

    center_bin_i = length(left$bin_midpoints) + 1
    b = list(
      bins = c(left$bins, rep(center_bin_i, n_center), center_bin_i + right$bins),
      bin_midpoints = c(left$bin_midpoints, center_midpoint, right$bin_midpoints)
    )

    wilkinson_smooth(x, b, binwidth, span = span)
  }
}

#' Get the run-length encoding of the bins in a Wilkinson binning
#' @param bins <[list]> sorted bin numbers starting at `1`, as in the
#' `"bins"` element of output from `wilkinson_` functions.
#' @returns <[data.frame]> with columns:
#' - `"bin"`: `unique(bins)`
#' - `"count"`: occurrences of each bin.
#' @noRd
rle_bins = function(bins) {
  out = vec_unrep(bins)
  names(out) = c("bin", "count")
  out
}

#' Re-number bins to be consecutive
#'
#' Removes empty bins and gaps in bins
#' @param bin_runs <[data.frame]> as returned by `rle_bins()`. Missing bins
#' will be removed: the `"bin"` element should be increasing but need
#' not be consecutive and the `"count"` element may have zeros.
#' @returns modified `bin_runs` with consecutive `"bin"` element starting
#' at zero and `"count"` all positive.
#' @noRd
renumber_bins = function(bin_runs) {
  bin_runs = bin_runs[bin_runs$count > 0, ]
  bin_runs$bin = seq_len(nrow(bin_runs))
  bin_runs
}

#' Convert run-length encoded bins into wilkinson binning format and find bin locations
#' @param bin_runs <[data.frame]> as returned by `rle_bins()`. The
#' `"bin"` element should be consecutive starting from `1`and the
#' `"count"` element should not have zeros.
#' @param x <[numeric]> data values to be binned
#' @param b <[list]> as returned by `wilkinson_` methods.
#' @returns <[list]> binning format returned by `wilkinson_` methods, with
#' elements `"bins"`, `"bin_left"`, `"bin_right"`, `"bin_midpoints"`.
#' @noRd
locate_bins = function(bin_runs, x) {
  bins = rep.int(bin_runs$bin, times = bin_runs$count)

  # can take advantage of the fact that bins is sorted runs of numbers to
  # get the first and last entry from each bin
  bin_left = x[bins != c(0, bins[-length(bins)])]
  bin_right = x[bins != c(bins[-1], 0)]
  bin_midpoints = (bin_left + bin_right) / 2

  list(
    bins = bins,
    bin_left = bin_left,
    bin_right = bin_right,
    bin_midpoints = bin_midpoints
  )
}

#' Adjacent-bin moving average smooth as described in Wilkinson
#' @description
#' Exchanges dots between adjacent bins in a dotplot as described in Wilkinson.
#' @param x <[numeric]> sorted data values
#' @param b <[numeric]> binning as returned by other `wilkinson_` functions: a
#' [list] with elements `bins` (consecutive integers starting at `1` with the same
#' length as `x`) and `bin_midpoints` (having length equal to `max(bins)`).
#' @param binwidth <scalar [numeric]> positive bin width
#' @param span <scalar [numeric]> multiple of bin width giving the window within
#' which to consider the next bin "adjacent". If `0`, no smoothing is done. A value
#' of `1.25` is equivalent to Wilkinson's recommended smoothing if bins are within
#' `binwidth/4` of each other.
#' @noRd
wilkinson_smooth = function(x, b, binwidth, span = 0) {
  if (span == 0) return(b)
  window = binwidth * span

  bin_runs = rle_bins(b$bins)
  for (i in seq_len(nrow(bin_runs) - 1)) {
    if (b$bin_midpoints[[i + 1]] - b$bin_midpoints[[i]] <= window) {
      dots_to_move = floor((bin_runs$count[[i + 1]] - bin_runs$count[[i]]) / 2)
      bin_runs$count[[i]] = bin_runs$count[[i]] + dots_to_move
      bin_runs$count[[i + 1]] = bin_runs$count[[i + 1]] - dots_to_move
    }
  }

  bin_runs = renumber_bins(bin_runs)
  locate_bins(bin_runs, x)
}


# stratified swarm -------------------------------------------------------------

#' Beeswarm layout using a fractional grid.
#' @description
#' Lays out dots by sweeping one row at a time on a fractional grid, alternating the direction of
#' sweeps on each row.
#' @param xs <[list] of [numeric]> groups of sorted x values
#' @param y <[numeric]> y values (should be constant)
#' @param xsize <scalar [numeric]> horizontal spacing between dots
#' @param ysize <scalar [numeric]> vertical spacing between dots
#' @param ygrid <scalar [integer]> \eqn{\ge 1} resolution of the fractional grid used to place dots.
#' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
#' @returns <[data.frame]> data frame with columns x and y giving the new positions
#' @noRd
grid_swarm = function(xs, y, xsize, ysize = xsize, ygrid = 3, side = 1) {
  if (ygrid == Inf) {
    dots = compact_swarm_prog_(xs, xsize, ysize, side)
  } else {
    dots = grid_swarm_(xs, xsize, ysize, ygrid, side)
  }
  dots = dots[order(dots$x), ]
  dots$y = dots$y + y
  dots
}


# nudging for overlaps ------------------------------------------------

#' Nudge bins/dots to avoid overlaps
#'
#' Given the midpoints of bins/dots produced by a layout, nudge
#' bin midpoints to ensure they are at least `width` apart. Nudging is done
#' by constrained optimization, minimizing the sum of squares of distances of
#' new bin midpoints to old (weighted by number of items in each bin), subject
#' to adjacent bins being at least `width` apart.
#' @param bin_midpoints vector: midpoints of each bin
#' @param binwidth scalar: width of bins
#' @param count vector of `length(bin_midpoints)``: number of items in each bin
#' @returns vector of `length(bin_midpoints)` giving new bin midpoints
#' @noRd
nudge_bins = function(bin_midpoints, binwidth, count = rep(1, length(bin_midpoints))) {
  n = length(bin_midpoints)
  if (n < 2) return(bin_midpoints)

  # make coefs minimize squared distance to bin centers, weighted
  # by the number of elements in each bin
  d = count^2 * bin_midpoints
  # equivalent to D = diag(count^2) when factorized = FALSE
  R_inv = diag(1/count)

  # constrain difference between adjacent bin centers to be greater than bin width
  # equivalent to A = matrix(rep_len(c(-1, 1, rep(0, n - 1)), n * (n - 1)), nrow = n)
  # when using solve.QP()
  Amat = matrix(rep(c(-1, 1), n - 1), nrow = 2)
  Aind = rbind(
    rep(2, n - 1),
    seq_len(n - 1),
    seq(2, n)
  )
  b = rep(binwidth, n - 1)

  quadprog::solve.QP.compact(R_inv, d, Amat, Aind, b, factorized = TRUE)$solution
}
