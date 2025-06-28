# binning methods for use with dots geom
#
# Author: mjskay
###############################################################################



# binning -----------------------------------------------------------------

#' Bin data values using a dotplot algorithm
#'
#' Bins the provided data values using one of several dotplot algorithms.
#'
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
  layout = c("bin", "weave", "hex", "swarm", "bar"),
  side = c("topright", "top", "right", "bottomleft", "bottom", "left", "topleft", "bottomright", "both"),
  orientation = c("horizontal", "vertical", "y", "x"),
  overlaps = "nudge"
) {
  layout = match.arg(layout)
  side = match.arg(side)
  orientation = match.arg(orientation)

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
  bin_method = select_bin_method(d[[x]], layout)
  h = dot_heap(d[[x]], binwidth = binwidth, heightratio = heightratio, stackratio = stackratio, bin_method = bin_method)
  d$bin = h$binning$bins

  # determine x positions (for bin/weave) or x and y positions (for swarm)
  y_start = switch_side(side, orientation,
    topright = h$y_spacing / stackratio / 2,
    bottomleft = - h$y_spacing / stackratio / 2,
    both = 0
  )
  switch(layout,
    bin = , hex = , bar = {
      bin_midpoints = h$binning$bin_midpoints
      if (overlaps == "nudge" && layout != "bar") {
        bin_midpoints = nudge_bins(bin_midpoints, binwidth, h$bin_counts)
      }
      d[[x]] = bin_midpoints[h$binning$bins]
      # maintain original data order within each bin when finding y positions
      d = d[order(d$bin, d$order), ]
    },
    weave = {
      # keep original x positions, but re-order within bins so that overlaps
      # across bins are less likely
      d = ddply_(d, "bin", function(bin_df) {
        seq_fun = if (side == "both") seq_interleaved_centered else seq_interleaved
        bin_df = bin_df[seq_fun(nrow(bin_df)),]
        bin_df$row = seq_len(nrow(bin_df))
        if (side == "both") bin_df$row = bin_df$row - round((nrow(bin_df) - 1) / 2)
        bin_df
      })

      if (overlaps == "nudge") {
        # nudge values within each row to ensure there are no overlaps
        d = ddply_(d, "row", function(row_df) {
          row_df[[x]] = nudge_bins(row_df[[x]], binwidth)
          row_df
        })
      }

      d$row = NULL
    },
    swarm = {
      stop_if_not_installed("beeswarm", '{.help ggdist::geom_dots}(layout = "swarm")')

      swarm_xy = beeswarm::swarmy(d[[x]], d[[y]],
        xsize = h$binwidth, ysize = h$y_spacing,
        log = "", cex = 1,
        side = switch_side(side, orientation, topright = 1, bottomleft = -1, both = 0),
        compact = TRUE
      )

      y_origin = d[[y]]
      d[[x]] = swarm_xy[["x"]]
      d[[y]] = swarm_xy[["y"]] + y_start

      if (side == "both") {
        # re-center contiguous clusters around their mean y position so that
        # small clusters are visually centered (rather than e.g. a cluster of
        # two points having one point on the origin line and one above it)
        d$y_origin = y_origin
        d$bin = cumsum(c(1L, diff(d[[x]]) >= h$binwidth))
        d = ddply_(d, "bin", function(bin_df) {
          bin_df[[y]] = bin_df[[y]] - mean(bin_df[[y]]) + bin_df$y_origin
          bin_df
        })
        d$y_origin = NULL
      }
    }
  )

  # determine y positions (for bin/weave/bar) and also x offsets (for hex)
  if (layout %in% c("bin", "weave", "hex", "bar")) {
    d = ddply_(d, "bin", function(bin_df) {
      y_offset = seq(0, h$y_spacing * (nrow(bin_df) - 1), length.out = nrow(bin_df))
      row_offset = 0
      switch_side(side, orientation,
        topright = {},
        bottomleft = {
          y_offset = - y_offset
        },
        both = {
          row_offset = (nrow(bin_df) - 1) / 2
          if (layout %in% c("weave", "hex", "bar")) {
            # weave and hex require rows to be aligned exactly so that x offsets
            # can be applied within rows; bar because it looks weird otherwise
            row_offset = round(row_offset)
          }
          y_offset = y_offset - h$y_spacing * row_offset
        }
      )
      bin_df[[y]] = bin_df[[y]] + y_start + y_offset

      if (layout == "hex") {
        # depending on whether this is an even or odd row, need to start the
        # x offset to the left or to the right
        x_offset_start = if (row_offset %% 2 == 0) 1 else -1
        bin_df[[x]] = bin_df[[x]] + rep_len(c(-0.25, 0.25) * x_offset_start, nrow(bin_df)) * binwidth
      }

      bin_df
    })
  }

  # restore the original data order in case it was destroyed
  d = d[order(d$order), ]
  d$order = NULL

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
  layout = c("bin", "weave", "hex", "swarm", "bar")
) {
  layout = match.arg(layout)
  x = sort(as.numeric(x), na.last = TRUE)

  # figure out a reasonable minimum number of bins based on histogram binning
  min_nbins = if (length(x) <= 1) {
    1
  } else {
    min(nclass.scott(x), nclass.FD(x), nclass.Sturges(x))
  }
  bin_method = select_bin_method(x, layout)
  dot_heap_ = function(...) {
    dot_heap(
      x,
      ...,
      maxheight = maxheight,
      heightratio = heightratio,
      stackratio = stackratio,
      bin_method = bin_method
    )
  }
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
    max_h = if (layout == "bar") {
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
          (h$max_bin_count * h$max_y_spacing - h$maxheight)^2
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
#' @param maxheight maximum height of a single bin
#' @param heightratio ratio between the bin width and the y spacing
#' @return  a list of properties of this dot "heap"
#' @noRd
dot_heap = function(
  x,
  nbins = NULL,
  binwidth = NULL,
  maxheight = Inf,
  heightratio = 1,
  stackratio = 1,
  bin_method = automatic_bin
) {
  xspread = diff(range(x))
  if (xspread == 0) xspread = 1
  if (is.null(binwidth)) {
    nbins = floor(nbins)
    binwidth = xspread / nbins
  } else {
    nbins = max(floor(xspread / binwidth), 1)
  }
  binning = bin_method(x, binwidth)
  bin_counts = tabulate(binning$bins)
  # max bin count is the max "effective" number of elements in a bin, which
  # is the number of elements in the bin modified by the stackratio to account
  # for how dots align with tops and bottoms of stacks when stackratio != 1
  max_bin_count = max(bin_counts) - 1 + 1/stackratio

  y_spacing = binwidth * heightratio

  if (length(bin_counts) == 1) {
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
  is_valid_approx = isTRUE(max_bin_count * max_y_spacing <= maxheight + .Machine$double.eps^0.25)

  as.list(environment())
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


# dynamic binning method selection ----------------------------------------

automatic_bin = function(x, width, layout = "bin") {
  select_bin_method(x, layout)(x, width)[c("bins", "bin_midpoints")]
}

# examines a vector x and determines an appropriate binning method based on its properties
select_bin_method = function(x, layout = "bin") {
  if (layout == "bar") return(bar_bin)

  diff_x = diff(x)
  if (isTRUE(all.equal(diff_x, rev(diff_x), check.attributes = FALSE))) {
    # x is symmetric, used centered binning
    wilkinson_bin_from_center
  } else {
    wilkinson_bin
  }
}


# bar layout --------------------------------------------------------------

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
