# binning methods for use with dots geom
#
# Author: mjskay
###############################################################################
#' @include binner.R
NULL


# bin_dots -----------------------------------------------------------------

#' Bin data values using a dotplot algorithm
#'
#' @description
#' Bins the provided data values using one of several dotplot algorithms.
#' @param x <[numeric]> *x* values.
#' @param y <[numeric]> *y* values (same length as `x`).
#' @param binwidth <scalar [numeric]> Bin width.
#' @param heightratio <scalar [numeric]> Ratio of bin width to dot height
#' @param stackratio <scalar [numeric]> Ratio of dot height to vertical distance
#' between dot centers
#' @eval rd_param_dots_layout()
#' @eval rd_param_dots_overlaps()
#' @eval rd_param_slab_side()
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
#' # we can manually plot the binning above, though this is only recommended
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
bin_dots = function(x, y, binwidth,
  heightratio = 1,
  stackratio = 1,
  layout = c("bin", "weave", "hex", "swarm", "swarm2", "bar"),
  side = c("topright", "top", "right", "bottomleft", "bottom", "left", "topleft", "bottomright", "both"),
  orientation = c("horizontal", "vertical", "y", "x"),
  overlaps = c("nudge", "keep")
) {
  layout = match.arg(layout)
  side = match.arg(side)
  orientation = match.arg(orientation)
  overlaps = match.arg(overlaps)

  d = data_frame0(x = x, y = y)

  # after this point `x` and `y` refer to column names in `d` according
  # to the orientation
  define_orientation_variables(orientation)

  # Sort the x values, because they must be sorted for bin methods to maintain
  # the correct connection between input values and output bins.
  # Because of this (and other later grouping operations that may re-order the
  # data as well) we need to keep the original data order around so that
  # we can restore the original order at the end.
  d$order = seq_len(nrow(d))
  d = d[order(d[[x]]), ]

  # bin the dots
  binner = new_binner(
    layout,
    x = d[[x]],
    heightratio = heightratio,
    stackratio = stackratio,
    side = side,
    orientation = orientation,
    overlaps = overlaps
  )
  binning = arrange_bins(binner, d[[x]], binwidth = binwidth)
  d = place_dots(binner, d, binning)

  # restore the original data order in case it was destroyed
  d = d[order(d$order), ]
  d$order = NULL

  d
}


# arrange_bins -----------------------------------------------------------

#' Arrange a binning of dots
#' 
#' Create a dot binning, which includes a set of bins of dots and other properties of the
#' binning, such as what the bins are, what the dot widths are, what the y spacing between 
#' dots should be, etc.
#' @param binner <`binner`> the binning method
#' @param x <[numeric]> vector of dot positions
#' @param nbins,binwidth <scalar [numeric]> provide either the desired number of bins (`nbins`)
#' or the desired bin width (`binwidth`); given one the other will be calculated.
#' @return <[list]> properties of this dot binning, with elements:
#' - `nbins`: <scalar [integer]> number of bins
#' - `binwidth`: <scalar [numeric]> bin width
#' - `y_spacing`: <scalar [numeric]> vertical distance between dot centers
#' - `y_start`: <scalar [numeric]> starting y offset for the first dot in each bin
#' Subclasses may also add additional elements. They *must* add at least the following elements:
#' - `height`: <scalar [numeric]> height of the tallest bin in this binning
#' @noRd
arrange_bins = new_generic("arrange_bins", c("binner"), function(binner, x, nbins = NULL, binwidth = NULL) {
  S7_dispatch()
})
  
method(arrange_bins, binner) = function(binner, x, nbins = NULL, binwidth = NULL) {
  # determine binwidth and number of bins
  x_spread = diff(range(x))
  if (x_spread == 0) x_spread = 1
  if (is.null(binwidth)) {
    nbins = floor(nbins)
    binwidth = x_spread / nbins
  } else {
    nbins = max(floor(x_spread / binwidth), 1)
  }

  # determine y positioning parameters
  y_spacing = binwidth * binner@heightratio
  y_start = switch_side(binner@side, binner@orientation,
    topright = y_spacing / binner@stackratio / 2,
    bottomleft = - y_spacing / binner@stackratio / 2,
    both = 0
  )

  list(
    nbins = nbins,
    binwidth = binwidth,
    y_spacing = y_spacing,
    y_start = y_start
  )
}

## arrange_bins for bin, hex, weave, bar ----------------------------------

#' Arrange a binning of dots for binned layouts
#' @param binner <`binner_bin` | `binner_bar`> the binning method
#' @param x <[numeric]> vector of dot positions
#' @param nbins,binwidth <scalar [numeric]> provide either the desired number of bins (`nbins`)
#' or the desired bin width (`binwidth`); given one the other will be calculated.
#' @return <[list]> properties of this dot binning, with additional elements:
#' - `bins`: <[integer]> vector of same length as `x` giving the bin number (in {1 ... `nbins`}) for each element in `x`
#' - `bin_midpoints`: <[numeric]> vector of length `nbins` giving the midpoint of each bin
#' - `bin_counts`: <[integer]> vector of length `nbins` giving the number of elements in each bin
#' - `height`: <scalar [numeric]> height of the tallest bin in this binning
#' @noRd
method(arrange_bins, binner_bin | binner_bar) = function(binner, x, nbins = NULL, binwidth = NULL) {
  binning = arrange_bins(super(binner, get("binner", mode = "function")), x, nbins, binwidth)
  binning = c(binning, binner@bin_method(x, binning$binwidth))

  # determine height of the tallest bin
  binning$bin_counts = tabulate(binning$bins)
  # max bin count is the max "effective" number of elements in a bin, which
  # is the number of elements in the bin modified by the stackratio to account
  # for how dots align with tops and bottoms of stacks when stackratio != 1
  max_bin_count = max(binning$bin_counts) - 1 + 1/binner@stackratio
  binning$height = max_bin_count * binning$y_spacing

  # height = bin_count*y_spacing + y_spacing * (1/stackratio - 1)

  binning
}

## arrange_bins for swarm, swarm2 -----------------------------------------

#' Arrange a binning of dots for swarm layouts
#' @param binner <`binner_swarm`> the binning method
#' @param x <[numeric]> vector of dot positions
#' @param nbins,binwidth <scalar [numeric]> provide either the desired number of bins (`nbins`)
#' or the desired bin width (`binwidth`); given one the other will be calculated.
#' @return <[list]> properties of this dot binning, with additional elements:
#' - `dots`: <[data.frame]> data frame with `x` and `y` columns giving the positions of dots in the swarm
#' - `height`: <scalar [numeric]> height of the tallest bin in this binning
#' @noRd
method(arrange_bins, binner_swarm) = function(binner, x, nbins = NULL, binwidth = NULL) {
  stop_if_not_installed("beeswarm", '{.help ggdist::geom_dots}(layout = "swarm")')

  binning = arrange_bins(super(binner, get("binner", mode = "function")), x, nbins, binwidth)
  binning$dots = beeswarm::swarmy(
    x, 0,
    xsize = binning$binwidth, ysize = binning$y_spacing,
    log = "", cex = 1,
    side = switch_side(binner@side, binner@orientation, topright = 1, bottomleft = -1, both = 0),
    compact = TRUE
  )
  binning$dots = recenter_swarm_clusters(binner, binning$dots, binning)
  binning$height = get_swarm_height(binner, binning$dots, binning)

  binning
}

#' Arrange a binning of dots for swarm2 layouts
#' @param binner <`binner_swarm2`> the binning method
#' @param x <[numeric]> vector of dot positions
#' @param nbins,binwidth <scalar [numeric]> provide either the desired number of bins (`nbins`)
#' or the desired bin width (`binwidth`); given one the other will be calculated.
#' @return <[list]> properties of this dot binning, with additional elements:
#' - `dots`: <[data.frame]> data frame with `x` and `y` columns giving the positions of dots in the swarm
#' - `height`: <scalar [numeric]> height of the tallest bin in this binning
#' @noRd
method(arrange_bins, binner_swarm2) = function(binner, x, nbins = NULL, binwidth = NULL) {
  binning = arrange_bins(super(binner, get("binner", mode = "function")), x, nbins, binwidth)
  binning$dots = weave_swarm(
    x, 0,
    xsize = binning$binwidth, ysize = binning$y_spacing,
    side = switch_side(binner@side, binner@orientation, topright = 1, bottomleft = -1, both = 0)
  )
  binning$dots = recenter_swarm_clusters(binner, binning$dots, binning)
  binning$height = get_swarm_height(binner, binning$dots, binning)

  binning
}

#' Re-center swarm clusters for side = "both" in swarm layouts
#' @param binner <`binner`> dot binning method
#' @param dots <[data.frame]> dot positions with `x` and `y` columns, where `x`
#' is always the data values and `y` is the vertical position assigned by the swarm algorithm.
#' @param binning <[list]> dot binning properties as returned by `arrange_bins()`
#' @noRd
recenter_swarm_clusters = function(binner, dots, binning) {
  if (binner@side != "both") return(dots)

  # re-center contiguous clusters around their mean y position so that
  # small clusters are visually centered (rather than e.g. a cluster of
  # two points having one point on the origin line and one above it)
  dots$bin = cumsum(c(1L, diff(dots$x) >= binning$binwidth))
  ddply_(dots, "bin", function(bin_df) {
    bin_df$y = bin_df$y - mean(bin_df$y)
    bin_df
  })
}

#' Get the height of a swarm binning
#' @param binner <`binner`> dot binning method
#' @param dots <[data.frame]> dot positions with `x` and `y` columns, where `x`
#' is always the data values and `y` is the vertical position assigned by the swarm algorithm.
#' @param binning <[list]> dot binning properties as returned by `arrange_bins()`
#' @returns <scalar [numeric]> height of the swarm binning
#' @noRd
get_swarm_height = function(binner, dots, binning) {
  height_minus_1_dot = max(abs(binning$dots$y)) * if (binner@side == "both") 2 else 1
  dot_height = binning$y_spacing / binner@stackratio
  height_minus_1_dot + dot_height
}

# place_dots -------------------------------------------------------------

#' Find the x and y position of dots given a binner and a binning
#' @param binner <`binner`> dot binning method
#' @param d <[data.frame]> dot positions with at least `x`, `y`, and `bin` columns
#' @param binning <[list]> dot binning properties as returned by `arrange_bins()`
#' @returns <[data.frame]> modified version of `d` with updated `x` and `y` columns
#' @noRd
place_dots = new_generic("place_dots", c("binner"), function(binner, d, binning) {
  S7_dispatch()
})

## place_dots for bin, hex, weave, bar ---------------------------------------

method(place_dots, binner_bin | binner_bar) = function(binner, d, binning) {
  d$bin = binning$bins
  d = place_dots_x_binned(binner, d, binning)
  d = place_dots_y_binned(binner, d, binning)
  d
}

method(place_dots, binner_hex) = function(binner, d, binning) {
  define_orientation_variables(binner@orientation)

  d = place_dots(super(binner, binner_bin), d, binning)
  d = ddply_(d, "bin", function(bin_df) {
    n_dots = nrow(bin_df)
    row_start_offset = get_row_start_offset(binner, binning, n_dots)
    # depending on whether this is an even or odd column, need to start the
    # x offset to the left or to the right
    x_offset_start = if (row_start_offset %% 2 == 0) 1 else -1
    bin_df[[x]] = bin_df[[x]] + rep_len(c(-0.25, 0.25) * x_offset_start, n_dots) * binning$binwidth
    bin_df
  })

  d
}

method(place_dots, binner_weave) = function(binner, d, binning) {
  define_orientation_variables(binner@orientation)

  # keep original x positions, but re-order within bins so that overlaps
  # across bins are less likely
  d$bin = binning$bins
  d = ddply_(d, "bin", function(bin_df) {
    seq_fun = if (binner@side == "both") seq_interleaved_centered else seq_interleaved
    bin_df = bin_df[seq_fun(nrow(bin_df)),]
    bin_df$row = seq_len(nrow(bin_df))
    if (binner@side == "both") bin_df$row = bin_df$row - round((nrow(bin_df) - 1) / 2)
    bin_df
  })

  if (binner@overlaps == "nudge") {
    # nudge values within each row to ensure there are no overlaps
    d = ddply_(d, "row", function(row_df) {
      row_df[[x]] = nudge_bins(row_df[[x]], binning$binwidth)
      row_df
    })
  }

  d$row = NULL

  d = place_dots_y_binned(binner, d, binning)

  d
}

#' Find the x positions of dots in binned layouts
#' @param binner <`binner`> dot binning method
#' @param d <[data.frame]> dot positions with at least `x`, `y`, and `bin` columns
#' @param binning <[list]> dot binning properties as returned by `arrange_bins()`
#' @returns <[data.frame]> modified version of `d` with updated `x` or `y` column depending on orientation
#' @noRd
place_dots_x_binned = function(binner, d, binning) {
  define_orientation_variables(binner@orientation)

  bin_midpoints = binning$bin_midpoints
  if (binner@overlaps == "nudge") {
    bin_midpoints = nudge_bins(bin_midpoints, binning$binwidth, binning$bin_counts)
  }
  d[[x]] = bin_midpoints[binning$bins]
  # maintain original data order within each bin when finding y positions
  d = d[order(d$bin, d$order), ]
  d
}

#' Find the y positions of dots in binned layouts
#' @param binner <`binner`> dot binning method
#' @param d <[data.frame]> dot positions with at least `x`, `y`, and `bin` columns
#' @param binning <[list]> dot binning properties as returned by `arrange_bins()`
#' @returns <[data.frame]> modified version of `d` with updated `x` or `y` column depending on orientation
#' @noRd
place_dots_y_binned = function(binner, d, binning) {
  define_orientation_variables(binner@orientation)

  d = ddply_(d, "bin", function(bin_df) {
    y_offset = seq(
      0,
      binning$y_spacing * (nrow(bin_df) - 1),
      length.out = nrow(bin_df)
    )
    row_start_offset = get_row_start_offset(binner, binning, nrow(bin_df))
    switch_side(binner@side, binner@orientation,
      topright = {},
      bottomleft = {
        y_offset = -y_offset
      },
      both = {
        y_offset = y_offset - binning$y_spacing * row_start_offset
      }
    )
    bin_df[[y]] = bin_df[[y]] + binning$y_start + y_offset

    bin_df
  })
  d
}

#' Get the number of rows the start of a dot column will be offset by
#' @param binner <`binner`> dot binning method
#' @param binning <[list]> dot binning properties as returned by `arrange_bins()`
#' @param n_dots <[integer]> number of dots in the column
#' @returns <[integer]> number of rows the start of the column is offset by
#' @noRd
get_row_start_offset = function(binner, binning, n_dots) {
  if (binner@side == "both") {
    row_start_offset = (n_dots - 1) / 2
    if (binner@align_rows) {
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

method(place_dots, binner_swarm) = function(binner, d, binning) {
  define_orientation_variables(binner@orientation)

  d[[x]] = binning$dots$x
  d[[y]] = d[[y]] + binning$y_start + binning$dots$y
  d
}


# modified wilkinson methods ----------------------------------------------

#' a variant of the basic wilkinson binning method, a single left-to-right sweep
#' @param x sorted numeric vector
#' @param width bin width
#' @noRd
wilkinson_bin_to_right = function(x, width) {
  if (length(x) == 0) {
    return(list(
      bins = integer(0),
      bin_midpoints = numeric(0),
      bin_left = numeric(0),
      bin_right = numeric(0)
    ))
  }

  # determine bins
  bins = wilkinson_bin_to_right_(x, width)

  # determine bin positions
  # can take advantage of the fact that bins is sorted runs of numbers to
  # get the first and last entry from each bin
  bin_left = x[!duplicated(bins)]
  bin_right = x[!duplicated(bins, fromLast = TRUE)]
  bin_midpoints = (bin_left + bin_right) / 2

  list(
    bins = bins,
    bin_midpoints = bin_midpoints,
    bin_left = bin_left,
    bin_right = bin_right
  )
}

#' do a backwards sweep after a left-to-right wilkinson binning, trying to
#' eliminate extra space at the end of the binning by eating up slack between
#' bins
#' @param x sorted numeric vector
#' @param b a binning returned by wilkinson_bin_to_right
#' @param width bin width
#' @param first_slack max amount of slack on the first bin
#' @noRd
wilkinson_sweep_back = function(x, b, width, first_slack = Inf) {
  n_bin = length(b$bin_left)
  if (n_bin < 2) return(b)

  # amount we want to move left is the extra space at the end of the last bin
  move_left = width - b$bin_right[[n_bin]] + b$bin_left[[n_bin]] - .Machine$double.eps
  if (move_left <= 0) return(b)

  # slack is the distance between bins
  slack = b$bin_left[-1] - (b$bin_left[-n_bin] + width)

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
  first_x_in_bin = !duplicated(b$bins)
  b$bins = cumsum(first_x_in_bin)

  # can take advantage of the fact that b$bins is sorted runs of numbers to
  # get the first and last entry from each bin
  b$bin_left = x[first_x_in_bin]
  b$bin_right = x[!duplicated(b$bins, fromLast = TRUE)]
  b$bin_midpoints = (b$bin_left + b$bin_right) / 2

  b
}

#' a rightward or leftward wilkinson binning followed by a backwards sweep to
#' reduce edge effects by taking up slack in the binning (spaces between bins)
#' @param x numeric vector
#' @param width bin width
#' @param right bin left-to-right (TRUE) or right-to-left (FALSE)?
#' @param first_slack maximum slack on the first bin (passed to wilkinson_sweep_back)
#' @noRd
wilkinson_bin = function(x, width, right = TRUE, first_slack = Inf) {
  if (length(x) == 0) {
    return(list(
      bins = integer(0),
      bin_midpoints = numeric(0)
    ))
  }

  if (right) {
    b = wilkinson_bin_to_right(x, width)
    wilkinson_sweep_back(x, b, width, first_slack = first_slack)
  } else {
    rev_x = -rev(x)
    b = wilkinson_bin_to_right(rev_x, width)
    b = wilkinson_sweep_back(rev_x, b, width, first_slack = first_slack)
    list(
      # renumber bins so 1,2,3,3 => 3,2,1,1 (then reverse so it matches original vector order)
      bins = rev(max(b$bins) + 1 - b$bins),
      bin_midpoints = -rev(b$bin_midpoints)
    )
  }
}

#' A modified wilkinson-style binning that expands outward from the center of
#' the data. Works best on symmetric data.
#'  x must be sorted
#' @param x numeric vector
#' @param width bin width
#' @noRd
wilkinson_bin_from_center = function(x, width) {
  if (length(x) == 0) {
    list(
      bins = integer(0),
      bin_midpoints = numeric(0)
    )
  } else if (length(x) == 1 || abs(x[[length(x)]] - x[[1]]) < width) {
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
        left = wilkinson_bin(x[1:(length(x)/2)], width, right = FALSE, first_slack = first_slack)
        right = wilkinson_bin(x[(length(x)/2 + 1):length(x)], width, first_slack = first_slack)
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
      if (abs(x[[center_i + offset]] - x[[center_i - offset]]) < width) {
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
      x[1:(center_i - edge_offset_from_center - 1)], width, right = FALSE,
      first_slack = x[[center_i - edge_offset_from_center]] - x[[center_i - edge_offset_from_center - 1]]
    )
    right = wilkinson_bin(
      x[(center_i + edge_offset_from_center + 1):length(x)], width,
      first_slack = x[[center_i + edge_offset_from_center + 1]] - x[[center_i + edge_offset_from_center]]
    )

    center_bin_i = length(left$bin_midpoints) + 1
    list(
      bins = c(left$bins, rep(center_bin_i, n_center), center_bin_i + right$bins),
      bin_midpoints = c(left$bin_midpoints, center_midpoint, right$bin_midpoints)
    )
  }
}


# weave swarm -------------------------------------------------------------

#' Weave/swarm hybrid
#'
#' @param x <[numeric]> sorted x values
#' @param x <[numeric]> y values (should be constant)
#' @param xsize <scalar [numeric]> horizontal spacing between dots
#' @param ysize <scalar [numeric]> vertical spacing between dots
#' @param side <scalar [integer]> which side to place dots on: 0 = both, 1 = above, -1 = below
#' @returns <[data.frame]> data frame with columns x and y giving the new positions
#' @noRd
weave_swarm = function(x, y, xsize, ysize = xsize, side = 1) {
  df = weave_swarm_(x, xsize, ysize, side)
  df = df[order(df$x), ]
  df$y = df$y + y
  df
}


# bin nudging for overlaps ------------------------------------------------

#' given a binning produced by one of the binning methods, nudge
#' bin midpoints to ensure they are at least `width` apart. Nudging is done
#' by constrained optimization, minimizing the sum of squares of distances of
#' new bin midpoints to old (weighted by number of items in each bin), subject
#' to adjacent bins being at least `width` apart.
#' @param bin_midpoints vector: midpoints of each bin
#' @param width scalar: width of bins
#' @param count vector of length(bin_midpoints): number of items in each bin
#' @returns vector of length(bin_midpoints) giving new bin midpoints
#' @noRd
nudge_bins = function(bin_midpoints, width, count = rep(1, length(bin_midpoints))) {
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
  b = rep(width, n - 1)

  quadprog::solve.QP.compact(R_inv, d, Amat, Aind, b, factorized = TRUE)$solution
}
