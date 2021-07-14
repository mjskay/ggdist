# binning methods for use with dots geom
#
# Author: mjskay
###############################################################################



# binning -----------------------------------------------------------------

#' Bin data values using a dotplot algorithm
#'
#' Bins the provided data values using one of several dotplot algorithms.
#'
#' @param x numeric vector of x values
#' @param y numeric vector of y values
#' @param binwidth bin width
#' @param heightratio ratio of bin width to dot height
#' @template param-dots-layout
#' @template param-slab-side
#' @param orientation Whether the dots are laid out horizontally or vertically.
#' Follows the naming scheme of `geom_slabinterval()`:
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
  layout = c("bin", "weave", "swarm"),
  side = c("topright", "top", "right", "bottomleft", "bottom", "left", "topleft", "bottomright", "both"),
  orientation = c("horizontal", "vertical", "y", "x")
) {
  layout = match.arg(layout)
  side = match.arg(side)
  orientation = match.arg(orientation)

  d = data.frame(x = x, y = y)

  # after this point `x` and `y` refer to column names in `d` according
  # to the orientation
  define_orientation_variables(orientation)

  # binning from the center (automatic_bin) is only useful in the "bin" layout
  # and can give weird results with "weave" (and is pointless on "swarm")
  bin_method = if (layout == "bin") automatic_bin else wilkinson_bin_to_right

  # bin the dots
  h = dot_heap(d[[x]], binwidth = binwidth, heightratio = heightratio, bin_method = bin_method)
  d$bin = h$binning$bins

  # determine x positions (for bin/weave) or x and y positions (for swarm)
  y_start = switch_side(side, orientation,
    topright = h$y_spacing / 2,
    bottomleft = - h$y_spacing / 2,
    both = 0
  )
  switch(layout,
    bin = {
      bin_midpoints = nudge_bins(h$binning$bin_midpoints, binwidth)
      d[[x]] = bin_midpoints[h$binning$bins]
    },
    weave = {
      # keep original x positions, but re-order within bins so that overlaps
      # across bins are less likely
      d = ddply_(d, "bin", function(bin_df) {
        seq_fun = if (side == "both") seq_interleaved_centered else seq_interleaved
        bin_df = bin_df[seq_fun(nrow(bin_df)),]
        bin_df$row = seq_len(nrow(bin_df))
        bin_df
      })

      # nudge values within each row to ensure there are no overlaps
      # (rows are not well-defined in side = "both" for this to work,
      # so we skip this step in that case)
      if (side != "both") {
        d = ddply_(d, "row", function(row_df) {
          row_df[[x]] = nudge_bins(row_df[[x]], binwidth)
          row_df
        })
      }

      d$row = NULL
    },
    swarm = {
      if (!requireNamespace("beeswarm", quietly = TRUE)) {
        stop('Using layout = "swarm" with the dots geom requires the `beeswarm` package to be installed.') #nocov
      }

      swarm_xy = beeswarm::swarmy(d[[x]], d[[y]],
        xsize = h$binwidth, ysize = h$y_spacing,
        log = "", cex = 1,
        side = switch_side(side, orientation, topright = 1, bottomleft = -1, both = 0),
        compact = TRUE
      )

      d[[x]] = swarm_xy[["x"]]
      d[[y]] = swarm_xy[["y"]] + y_start
    }
  )

  # determine y positions (for bin/weave)
  if (layout %in% c("bin", "weave")) {
    d = ddply_(d, "bin", function(bin_df) {
      y_offset = seq(0, h$y_spacing * (nrow(bin_df) - 1), length.out = nrow(bin_df))
      switch_side(side, orientation,
        topright = {},
        bottomleft = {
          y_offset = - y_offset
        },
        both = {
          y_offset = y_offset - h$y_spacing * (nrow(bin_df) - 1) / 2
        }
      )
      bin_df[[y]] = bin_df[[y]] + y_start + y_offset
      bin_df
    })
  }

  d
}

# dynamic binwidth selection ----------------------------------------------

#' Dynamically select a good bin width for a dotplot
#'
#' Searches for a nice-looking bin width to use to draw a dotplot such that
#' the height of the dotplot fits within a given space (`maxheight`).
#'
#' @param x numeric vector of values
#' @param maxheight maximum height of the dotplot
#' @param heightratio ratio of bin width to dot height
#'
#' @details
#' This dynamic bin selection algorithm uses a binary search over the number of
#' bins to find a bin width such that if the input data (`x`) is binned
#' using a Wilkinson-style dotplot algorithm the height of the tallest bin
#' will be less than `maxheight`.
#'
#' This algorithm is used by `geom_dotsinterval()` (and its variants) to automatically
#' select bin widths. Unless you are manually implementing you own dotplot [`grob`]
#' or `geom`, you probably do not need to use this function directly
#'
#' @return A suitable bin width such that a dotplot created with this bin width
#' and `heightratio` should have its tallest bin be less than or equal to `maxheight`.
#'
#' @seealso [bin_dots()] for an algorithm can bin dots using bin widths selected
#' by this function; [geom_dotsinterval()] for geometries that use
#' these algorithms to create dotplots.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' x = qnorm(ppoints(20))
#' binwidth = find_dotplot_binwidth(x, maxheight = 4, heightratio = 1)
#' binwidth
#'
#' bin_df = bin_dots(x = x, y = 0, binwidth = binwidth, heightratio = 1)
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
#' @importFrom grDevices nclass.Sturges nclass.FD nclass.scott
#' @export
find_dotplot_binwidth = function(x, maxheight, heightratio = 1) {
  # figure out a reasonable minimum number of bins based on histogram binning
  min_nbins = if (length(x) <= 1) {
    1
  } else{
    min(nclass.scott(x), nclass.FD(x), nclass.Sturges(x))
  }
  min_h = dot_heap(x, nbins = min_nbins, maxheight = maxheight, heightratio = heightratio)

  if (!min_h$is_valid) {
    # figure out a maximum number of bins based on data resolution
    max_h = dot_heap(x, binwidth = resolution(x), maxheight = maxheight, heightratio = heightratio)

    if (max_h$nbins <= min_h$nbins) {
      # even at data resolution there aren't enough bins, not much we can do...
      h = min_h
    } else if (max_h$nbins == min_h$nbins + 1) {
      # nowhere to search, use maximum number of bins
      h = max_h
    } else {
      # use binary search to find a reasonable number of bins
      repeat {
        h = dot_heap(x, (min_h$nbins + max_h$nbins) / 2, maxheight = maxheight, heightratio = heightratio)
        if (h$is_valid) {
          # heap spec is valid, search downwards
          if (h$nbins - 1 <= min_h$nbins) {
            # found it, we're done
            break
          }
          max_h = h
        } else {
          # heap spec is not valid, search upwards
          if (h$nbins + 1 >= max_h$nbins) {
            # found it, we're done
            h = max_h
            break
          }
          min_h = h
        }
      }
    }
  } else {
    h = min_h
  }

  # check if the selected heap spec is valid....
  if (!h$is_valid) {
    # ... if it isn't, this means we've ended up with some bin that's too
    # tall, probably because we have discrete data --- we'll just
    # conservatively shrink things down so they fit by backing out a bin
    # width that works with the tallest bin
    y_spacing = maxheight / h$max_bin_count
    y_spacing / heightratio
  } else {
    h$max_binwidth
  }
}


# dot "heaps": collections of bins of dots -----------------------------------

#' create a dot "heap", which includes a binning of dots and properties of that
#' binning, such as what the bins are, what the dot widths are, what the
#' y spacing between dots should be, etc.
#' @param x a vector values
#' @param nbins,binwidth must provide either the desired number of bins (`nbins`)
#' or the desired bin width (`binwidth`); given one the other will be calculated.
#' @param maxheight maximum height of a single bin
#' @param heightratio ratio between the bin width and the y spacing
#' @return  a list of properties of this dot "heap"
#' @noRd
dot_heap = function(x, nbins = NULL, binwidth = NULL, maxheight = Inf, heightratio = 1, bin_method = automatic_bin) {
  xrange = range(x)
  xspread = xrange[[2]] - xrange[[1]]
  if (xspread == 0) xspread = 1
  if (is.null(binwidth)) {
    nbins = floor(nbins)
    binwidth = xspread / nbins
  } else {
    nbins = max(floor(xspread / binwidth), 1)
  }
  binning = bin_method(x, binwidth)
  max_bin_count = max(tabulate(binning$bins))

  y_spacing = binwidth * heightratio

  if (nbins == 1) {
    # if there's only 1 bin, we can scale it to be as large as we want as long as it fits, so
    # let's back out a max bin size based on that...
    max_y_spacing = maxheight / max_bin_count
    max_binwidth = max_y_spacing / heightratio
  } else {
    # if there's more than 1 bin, the provided nbins or bin width determines the max bin width
    max_y_spacing = y_spacing
    max_binwidth = binwidth
  }

  # is this a "valid" heap of dots; i.e. is its tallest bin less than max height?
  is_valid = isTRUE(max_bin_count * max_y_spacing <= maxheight)

  as.list(environment())
}

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
  bin_midpoints = numeric()
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
nudge_bins = function(bin_midpoints, width) {
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
  }

  bin_midpoints
}
