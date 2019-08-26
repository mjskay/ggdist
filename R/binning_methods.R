# binning methods for use with dots geom
#
# Author: mjskay
###############################################################################



# modified wilkinson methods ----------------------------------------------

# this implements a variant of the basic wilkinson binning method, a single
# left-to-right sweep.
# x must be sorted
wilkinson_bin_to_right = function(x, width, direction = 1) {
  if (length(x) == 0) {
    return(list(
      bins = integer(0),
      bin_midpoints = NULL
    ))
  }

  # determine bins and midpoints of bins
  bins = c(1L, rep(NA_integer_, length(x) - 1))
  bin_midpoints = c()
  current_bin = 1L
  first_x = x[[1]]
  n = 1
  for (i in seq_along(x)[-1]) {
    x_first_x_diff = abs(x[[i]] - first_x)
    # This is equivalent to x_first_x_diff >= width but it accounts for machine precision.
    # If we instead used `>=` directly some things that should be symmetric will not be
    if (x_first_x_diff > width || abs(x_first_x_diff - width) < .Machine$double.eps) {
      bin_midpoints[[current_bin]] = (x[[i - 1]] + first_x) / 2
      current_bin = current_bin + 1L
      first_x = x[[i]]
    }
    bins[[i]] = current_bin
  }
  if (length(bin_midpoints) < current_bin) {
    # calculate midpoint for last bin
    bin_midpoints[[current_bin]] = (x[[length(x)]] + first_x) / 2
  }

  list(
    bins = bins,
    bin_midpoints = bin_midpoints
  )
}

# the basic wilkinson method, but sweeping right-to-left
# x must be sorted
wilkinson_bin_to_left = function(x, width) {
  if (length(x) == 0) {
    return(list(
      bins = integer(0),
      bin_midpoints = NULL
    ))
  }

  binning = wilkinson_bin_to_right(rev(x), width, direction = -1)
  list(
    # renumber bins so 1,2,3,3 => 3,2,1,1 (then reverse so it matches original vector order)
    bins = rev(max(binning$bins) + 1 - binning$bins),
    bin_midpoints = rev(binning$bin_midpoints)
  )
}

# a modified wilkinson-style binning that expands outward from the center of the data
# works best on symmetric data.
# x must be sorted
wilkinson_bin_from_center = function(x, width) {
  if (length(x) == 0) {
    list(
      bins = integer(0),
      bin_midpoints = NULL
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
        left = wilkinson_bin_to_left(x[1:(length(x)/2)], width)
        right = wilkinson_bin_to_right(x[(length(x)/2 + 1):length(x)], width)
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
    left = wilkinson_bin_to_left(x[1:(center_i - edge_offset_from_center - 1)], width)
    right = wilkinson_bin_to_right(x[(center_i + edge_offset_from_center + 1):length(x)], width)

    center_bin_i = length(left$bin_midpoints) + 1
    list(
      bins = c(left$bins, rep(center_bin_i, n_center), center_bin_i + right$bins),
      bin_midpoints = c(left$bin_midpoints, center_midpoint, right$bin_midpoints)
    )
  }
}



# histogram binning -------------------------------------------------------

hist_bin = function(x, width) {
  xrange = range(x)
  breaks = seq(xrange[[1]], xrange[[2]], by = width)
  bins = as.numeric(cut(x, breaks, include.lowest = TRUE))

  binned_xs = split(x, bins)
  bin_midpoints = sapply(binned_xs, function(x) {
    (x[[1]] + x[[length(x)]])/2
  })

  list(
    bins = bins,
    bin_midpoints = bin_midpoints
  )
}



# dynamic binning method selection ----------------------------------------

# examines a vector x and determines an appropriate binning method based on its properties
automatic_bin = function(x, width) {
  diff_x = diff(x)
  if (isTRUE(all.equal(diff_x, rev(diff_x), check.attributes = FALSE))) {
    # x is symmetric, used centered binning
    wilkinson_bin_from_center(x, width)
  } else {
    wilkinson_bin_to_right(x, width)
  }
}



# bin nudging for overlaps ------------------------------------------------

# given a binning produced by one of the binning methods, nudge
# bin midpoints to ensure they are at least `width` apart
nudge_bins = function(binning, width) {
  bin_midpoints = binning$bin_midpoints

  if (length(bin_midpoints) >= 2) {
    if (length(bin_midpoints) %% 2 == 0) {
      # even number of bins => ensure the two center bins are proper width apart
      left_center_bin = length(bin_midpoints) / 2
      right_center_bin = left_center_bin + 1

      # ensure the two center bins are proper width apart
      center_bin_nudge = max((width - abs(bin_midpoints[[left_center_bin]] - bin_midpoints[[right_center_bin]]))/2, 0)
      bin_midpoints[[left_center_bin]] = bin_midpoints[[left_center_bin]] - center_bin_nudge
      bin_midpoints[[right_center_bin]] = bin_midpoints[[right_center_bin]] + center_bin_nudge
    } else {
      # odd number of bins => don't need to adjust the center
      left_center_bin = floor(length(bin_midpoints) / 2)
      right_center_bin = left_center_bin + 2
    }

    # nudge the left bins (those below the center) apart as necessary
    # can't use lag here since we're changing the values as we go
    for (i in left_center_bin:1) {
      bin_midpoints[[i]] = bin_midpoints[[i]] -
        max(width - abs(bin_midpoints[[i]] - bin_midpoints[[i + 1]]), 0)
    }

    # nudge the right bins (those above the center) apart as necessary
    for (i in right_center_bin:length(bin_midpoints)) {
      bin_midpoints[[i]] = bin_midpoints[[i]] +
        max(width - abs(bin_midpoints[[i]] - bin_midpoints[[i - 1]]), 0)
    }

    binning$bin_midpoints = bin_midpoints
  }

  binning
}
