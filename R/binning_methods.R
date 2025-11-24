# binning methods for use with dots geom
#
# Author: mjskay
###############################################################################
#' @include dots_layout.R
NULL


# binning -----------------------------------------------------------------

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
  layout = new_layout(
    match.arg(layout),
    heightratio = heightratio,
    stackratio = stackratio,
    side = match.arg(side),
    orientation = match.arg(orientation),
    overlaps = match.arg(overlaps)
  )
  d = data_frame0(x = x, y = y)

  # after this point `x` and `y` refer to column names in `d` according
  # to the orientation
  define_orientation_variables(layout@orientation)

  # Sort the x values, because they must be sorted for bin methods to maintain
  # the correct connection between input values and output bins.
  # Because of this (and other later grouping operations that may re-order the
  # data as well) we need to keep the original data order around so that
  # we can restore the original order at the end.
  d$order = seq_len(nrow(d))
  d = d[order(d[[x]]), ]

  # bin the dots
  layout = prepare_layout_for_data(layout, d[[x]])
  h = dot_heap(layout, d[[x]], binwidth = binwidth)
  d$bin = h$bins
  d = find_dots_xy(layout, d, h)

  # restore the original data order in case it was destroyed
  d = d[order(d$order), ]
  d$order = NULL

  d
}

#' Find the x and y position of dots given a layout and dot heap
#' @param layout dots layout
#' @param d data frame of dots with at least `x`, `y`, and `bin` columns
#' @param h dot heap
#' @noRd
find_dots_xy = new_generic("find_dots_xy", c("layout"), function(layout, d, h) {
  S7_dispatch()
})


# find_dots_xy for bin, hex, weave, bar ---------------------------------------

method(find_dots_xy, layout_bin) = function(layout, d, h) {
  d = find_dots_x_binned(layout, d, h)
  d = find_dots_y_binned(layout, d, h)
  d
}

#' Find the x positions of dots in binned layouts
#' @param layout dots layout
#' @param d data frame of dots with at least `x`, `y`, and `bin` columns
#' @param h dot heap
#' @noRd
find_dots_x_binned = function(layout, d, h) {
  define_orientation_variables(layout@orientation)

  bin_midpoints = h$bin_midpoints
  if (layout@overlaps == "nudge") {
    bin_midpoints = nudge_bins(bin_midpoints, h$binwidth, h$bin_counts)
  }
  d[[x]] = bin_midpoints[h$bins]
  # maintain original data order within each bin when finding y positions
  d = d[order(d$bin, d$order), ]
  d
}

#' Find the y positions of dots in binned layouts
#' @param layout dots layout
#' @param d data frame of dots with at least `x`, `y`, and `bin` columns
#' @param h dot heap
#' @noRd
find_dots_y_binned = function(layout, d, h) {
  define_orientation_variables(layout@orientation)

  d = ddply_(d, "bin", function(bin_df) {
    y_offset = seq(
      0,
      h$y_spacing * (nrow(bin_df) - 1),
      length.out = nrow(bin_df)
    )
    row_start_offset = get_row_start_offset(layout, h, nrow(bin_df))
    switch_side(layout@side, layout@orientation,
      topright = {},
      bottomleft = {
        y_offset = -y_offset
      },
      both = {
        y_offset = y_offset - h$y_spacing * row_start_offset
      }
    )
    bin_df[[y]] = bin_df[[y]] + h$y_start + y_offset

    bin_df
  })
  d
}

#' Get the number of rows the start of a dot column will be offset by
#' @param layout dots layout
#' @param h dot heap
#' @param n_dots number of dots in the column
#' @noRd
get_row_start_offset = function(layout, h, n_dots) {
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

method(find_dots_xy, layout_hex) = function(layout, d, h) {
  define_orientation_variables(layout@orientation)

  d = find_dots_xy(super(layout, layout_bin), d, h)
  d = ddply_(d, "bin", function(bin_df) {
    n_dots = nrow(bin_df)
    row_start_offset = get_row_start_offset(layout, h, n_dots)
    # depending on whether this is an even or odd column, need to start the
    # x offset to the left or to the right
    x_offset_start = if (row_start_offset %% 2 == 0) 1 else -1
    bin_df[[x]] = bin_df[[x]] + rep_len(c(-0.25, 0.25) * x_offset_start, n_dots) * h$binwidth
    bin_df
  })
  d
}

method(find_dots_xy, layout_weave) = function(layout, d, h) {
  define_orientation_variables(layout@orientation)

  # keep original x positions, but re-order within bins so that overlaps
  # across bins are less likely
  d = ddply_(d, "bin", function(bin_df) {
    seq_fun = if (layout@side == "both") seq_interleaved_centered else seq_interleaved
    bin_df = bin_df[seq_fun(nrow(bin_df)),]
    bin_df$row = seq_len(nrow(bin_df))
    if (layout@side == "both") bin_df$row = bin_df$row - round((nrow(bin_df) - 1) / 2)
    bin_df
  })

  if (layout@overlaps == "nudge") {
    # nudge values within each row to ensure there are no overlaps
    d = ddply_(d, "row", function(row_df) {
      row_df[[x]] = nudge_bins(row_df[[x]], h$binwidth)
      row_df
    })
  }

  d$row = NULL  
  d = find_dots_y_binned(layout, d, h)
  d
}


# find_dots_xy for swarm -------------------------------------------------

method(find_dots_xy, layout_swarm) = function(layout, d, h) {
  stop_if_not_installed("beeswarm", '{.help ggdist::geom_dots}(layout = "swarm")')
  define_orientation_variables(layout@orientation)

  swarm_xy = beeswarm::swarmy(
    d[[x]], d[[y]],
    xsize = h$binwidth, ysize = h$y_spacing,
    log = "", cex = 1,
    side = switch_side(layout@side, layout@orientation, topright = 1, bottomleft = -1, both = 0),
    compact = TRUE
  )

  d$y_origin = d[[y]]
  d[[x]] = swarm_xy[["x"]]
  d[[y]] = swarm_xy[["y"]] + h$y_start
  d = recenter_swarm_clusters(layout, d, h)
  d$y_origin = NULL
  d
}

method(find_dots_xy, layout_swarm2) = function(layout, d, h) {
  define_orientation_variables(layout@orientation)

  swarm_xy = weave_swarm(d[[x]], d[[y]],
    xsize = h$binwidth, ysize = h$y_spacing,
    side = switch_side(layout@side, layout@orientation, topright = 1, bottomleft = -1, both = 0)
  )

  d$y_origin = d[[y]]
  d[[x]] = swarm_xy[["x"]]
  d[[y]] = swarm_xy[["y"]] + h$y_start
  d = recenter_swarm_clusters(layout, d, h)
  d$y_origin = NULL
  d
}

#' Re-center swarm clusters for side = "both" in swarm layouts
#' @param layout dots layout
#' @param d data frame of dots with at least `x`, `y`, `bin`, and `y_origin` columns
#' @param h dot heap
#' @noRd
recenter_swarm_clusters = function(layout, d, h) {
  if (layout@side != "both") return(d)
  define_orientation_variables(layout@orientation)

  # re-center contiguous clusters around their mean y position so that
  # small clusters are visually centered (rather than e.g. a cluster of
  # two points having one point on the origin line and one above it)
  d$bin = cumsum(c(1L, diff(d[[x]]) >= h$binwidth))
  d = ddply_(d, "bin", function(bin_df) {
    bin_df[[y]] = bin_df[[y]] - mean(bin_df[[y]]) + bin_df$y_origin
    bin_df
  })
  d
}


# dynamic binwidth selection ----------------------------------------------

#' Dynamically select a good bin width for a dotplot
#'
#' Searches for a nice-looking bin width to use to draw a dotplot such that
#' the height of the dotplot fits within a given space (`maxheight`).
#'
#' @param x <[numeric]> Data values.
#' @param maxheight <scalar [numeric]> Maximum height of the dotplot.
#' @param heightratio <scalar [numeric]> Ratio of bin width to dot height.
#' @param stackratio <scalar [numeric]> Ratio of dot height to vertical distance
#' between dot centers
#' @eval rd_param_dots_layout()
#'
#' @details
#' This dynamic bin selection algorithm uses a binary search over the number of
#' bins to find a bin width such that if the input data (`x`) is binned
#' using a Wilkinson-style dotplot algorithm the height of the tallest bin
#' will be less than `maxheight`.
#'
#' This algorithm is used by [geom_dotsinterval()] (and its variants) to automatically
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
#' @importFrom stats optimize
#' @export
find_dotplot_binwidth = function(
  x,
  maxheight,
  heightratio = 1,
  stackratio = 1,
  layout = c("bin", "weave", "hex", "swarm", "swarm2", "bar")
) {
  x = sort(as.numeric(x), na.last = TRUE)

  # figure out a reasonable minimum number of bins based on histogram binning
  min_nbins = if (length(x) <= 1) {
    1
  } else {
    min(nclass.scott(x), nclass.FD(x), nclass.Sturges(x))
  }
  layout = new_layout(match.arg(layout),
    maxheight = maxheight,
    heightratio = heightratio,
    stackratio = stackratio
  )
  layout = prepare_layout_for_data(layout, x)
  dot_heap_ = function(...) dot_heap(layout, x, ...)
  min_h = dot_heap_(nbins = min_nbins)

  if (min_h$is_valid) {
    # if the minimum heap (i.e. the dot heap constructed from the smallest
    # number of bins --- thus, at the upper limit of the height we will allow)
    # is valid, then we don't need to search and can just use it.
    h = min_h
  } else {
    # figure out a maximum number of bins based on data resolution (except
    # for bars, which handle duplicate values differently, so must go by
    # number of data points instead of unique data points)
    # TODO: don't special case bar here
    max_h = if (S7_inherits(layout, layout_bar)) {
      dot_heap_(nbins = length(x))
    } else {
      dot_heap_(binwidth = resolution(x))
    }

    if (max_h$nbins <= min_h$nbins + 1) {
      # nowhere to search, use maximum number of bins
      h = max_h
    } else {
      # use binary search to find a reasonable number of bins
      repeat {
        h = dot_heap_(nbins = (min_h$nbins + max_h$nbins) / 2)
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

    # attempt to refine binwidth using optimization.
    # after finding a reasonable candidate based on number of bins, we refine
    # the binwidth around that number of bins using optimization. We do this
    # only as a second step because just using optimization on binwidth as a
    # first step tends to end up in a local minimum, sometimes very far from
    # maxheight.
    candidate_binwidths = c(min_h$binwidth, max_h$binwidth, h$binwidth)
    if (length(unique(candidate_binwidths)) != 1) {
      binwidth = optimize(
        function(binwidth) {
          h = dot_heap_(binwidth = binwidth)
          (h$max_bin_count * h$max_y_spacing - maxheight)^2
        },
        candidate_binwidths,
        tol = sqrt(.Machine$double.eps)
      )$minimum
      new_h = dot_heap_(binwidth = binwidth)

      # approximate version of new_h$is_valid used here to tolerate approximation with optimize()
      if (new_h$is_valid_approx) {
        h = new_h
      }
    }
  }

  # check if the selected heap spec is valid....
  if (h$is_valid_approx) {
    h$max_binwidth
  } else {
    # ... if it isn't, this means we've ended up with some bin that's too
    # tall, probably because we have discrete data --- we'll just
    # conservatively shrink things down so they fit by backing out a bin
    # width that works with the tallest bin
    y_spacing = maxheight / h$max_bin_count
    y_spacing / heightratio
  }
}


# dot "heaps": collections of bins of dots -----------------------------------

#' create a dot "heap", which includes a binning of dots and properties of that
#' binning, such as what the bins are, what the dot widths are, what the
#' y spacing between dots should be, etc.
#' @param x a vector values
#' @param nbins,binwidth must provide either the desired number of bins (`nbins`)
#' or the desired bin width (`binwidth`); given one the other will be calculated.
#' @param heightratio ratio between the bin width and the y spacing
#' @return  a list of properties of this dot "heap"
#' @noRd
dot_heap = function(
  layout,
  x,
  nbins = NULL,
  binwidth = NULL
) {
  xspread = diff(range(x))
  if (xspread == 0) xspread = 1
  if (is.null(binwidth)) {
    nbins = floor(nbins)
    binwidth = xspread / nbins
  } else {
    nbins = max(floor(xspread / binwidth), 1)
  }
  binning = layout@bin_method(x, binwidth)
  bin_counts = tabulate(binning$bins)
  # max bin count is the max "effective" number of elements in a bin, which
  # is the number of elements in the bin modified by the stackratio to account
  # for how dots align with tops and bottoms of stacks when stackratio != 1
  max_bin_count = max(bin_counts) - 1 + 1/layout@stackratio

  y_spacing = binwidth * layout@heightratio
  y_start = switch_side(layout@side, layout@orientation,
    topright = y_spacing / layout@stackratio / 2,
    bottomleft = - y_spacing / layout@stackratio / 2,
    both = 0
  )

  if (length(bin_counts) == 1) {
    # if there's only 1 bin, we can scale it to be as large as we want as long as it fits, so
    # let's back out a max bin size based on that...
    max_y_spacing = layout@maxheight / max_bin_count
    max_binwidth = max_y_spacing / layout@heightratio
  } else {
    # if there's more than 1 bin, the provided nbins or bin width determines the max bin width
    max_y_spacing = y_spacing
    max_binwidth = binwidth
  }

  # is this a "valid" heap of dots; i.e. is its tallest bin less than max height?
  is_valid = isTRUE(max_bin_count * max_y_spacing <= layout@maxheight)
  is_valid_approx = isTRUE(max_bin_count * max_y_spacing <= layout@maxheight + .Machine$double.eps^0.25)

  list(
    nbins = nbins,
    binwidth = binwidth,
    bins = binning$bins,
    bin_midpoints = binning$bin_midpoints,
    bin_counts = bin_counts,
    max_bin_count = max_bin_count,
    y_spacing = y_spacing,
    y_start = y_start,
    max_y_spacing = max_y_spacing,
    max_binwidth = max_binwidth,
    is_valid = is_valid,
    is_valid_approx = is_valid_approx
  )
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
#' @param x sorted x values
#' @param y y values (must be constant)
#' @noRd
weave_swarm = function(x, y, xsize, ysize = xsize, side = 1) {
  y_grid = 5

  can_place_candidate = function(candidate, last_placed, last_rows) {
    candidate >= last_placed + xsize &&
      all(map_lgl_(seq_len(y_grid - 1), function(i) {
        y_offset = i / y_grid
        candidate >= (tail(last_rows[[i]][last_rows[[i]] <= candidate], 1) + sqrt(1 - y_offset^2) * xsize) &&
          candidate <= (head(last_rows[[i]][candidate < last_rows[[i]]], 1) - sqrt(1 - y_offset^2) * xsize)
      }))
  }

  place_row = function(reverse = FALSE, both = side == 0) {
    if (length(remaining) == 0) return()

    kth_last_row = function(k, rows) c(-Inf, rows[max(length(rows) + 1 - k, 0)][1][[1]] %||% numeric(), Inf)
    last_rows = lapply(seq_len(y_grid), kth_last_row, rows)
    if (both) last_rows_bottom = lapply(seq_len(y_grid), kth_last_row, rows_bottom)
    candidates = remaining
    if (reverse) {
      last_rows = lapply(last_rows, function(r) rev(-r))
      if (both) last_rows_bottom = lapply(last_rows_bottom, function(r) rev(-r))
      candidates = rev(-candidates)
    }

    row = numeric()
    if (both) row_bottom = numeric()
    next_remaining = numeric()
    last_placed = -Inf
    if (both) last_placed_bottom = -Inf

    for (candidate in candidates) {
      if (can_place_candidate(candidate, last_placed, last_rows)) {
        row = c(row, candidate)
        last_placed = candidate
      } else if (both && can_place_candidate(candidate, last_placed_bottom, last_rows_bottom)) {
        row_bottom = c(row_bottom, candidate)
        last_placed_bottom = candidate
      } else {
        next_remaining = c(next_remaining, candidate)
      }
    }

    if (reverse) {
      row = rev(-row)
      if (both) row_bottom = rev(-row_bottom)
      next_remaining = rev(-next_remaining)
    }
    rows <<- c(rows, list(row))
    if (both) rows_bottom <<- c(rows_bottom, list(row_bottom))
    remaining <<- next_remaining
  }

  remaining = x
  rows = list()
  both = side == 0

  place_row(both = FALSE)
  rows_bottom = rows

  while (length(remaining) > 0) {
    # place_row()
    for (i in seq_len(y_grid)) place_row()
    for (i in seq_len(y_grid)) place_row(reverse = TRUE)
    # place_row()
  }

  row_y = function(rows, side) (seq_along(rows) - 1) / y_grid * ysize * side
  df = data.frame(
    x = unlist(rows),
    y = rep(row_y(rows, side = if (both) 1 else side), lengths(rows))
  )
  if (both) {
    df = rbind(
      df,
      data.frame(
        x = unlist(rows_bottom[-1]),
        y = rep(row_y(rows_bottom, side = -1)[-1], lengths(rows_bottom[-1]))
      )
    )
  }
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
