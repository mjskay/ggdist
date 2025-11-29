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
#' @eval rd_param_slab_side()
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
  layout = c("bin", "weave", "hex", "swarm", "swarm2", "bar"),
  side = c("topright", "top", "right", "bottomleft", "bottom", "left", "topleft", "bottomright", "both")
) {
  layout = match.arg(layout)
  side = match.arg(side)

  x = sort(as.numeric(x), na.last = TRUE)

  # figure out a reasonable minimum number of bins based on histogram binning
  min_nbins = if (length(x) <= 1) {
    1
  } else {
    min(nclass.scott(x), nclass.FD(x), nclass.Sturges(x))
  }
  binner = new_binner(
    layout,
    x,
    maxheight = maxheight,
    heightratio = heightratio,
    stackratio = stackratio,
    side = side
  )
  min_binning = arrange_bins(binner, x, nbins = min_nbins)

  if (isTRUE(min_binning$height <= maxheight)) {
    # if the minimum binning (i.e. the binning constructed from the smallest
    # number of bins --- thus, at the upper limit of the height we will allow)
    # is valid, then we don't need to search and can just use it.
    binning = min_binning
  } else {
    # figure out a maximum number of bins based on data resolution (except
    # for bars, which handle duplicate values differently, so must go by
    # number of data points instead of unique data points)
    # TODO: don't special case binner_bar here --- instead, have binners
    # implement a method to get max_binning
    max_binning = if (S7_inherits(binner, binner_bar)) {
      arrange_bins(binner, x, nbins = length(x))
    } else {
      arrange_bins(binner, x, binwidth = resolution(x))
    }

    if (max_binning$nbins <= min_binning$nbins + 1) {
      # nowhere to search, use maximum number of bins
      binning = max_binning
    } else {
      # use binary search to find a reasonable number of bins
      repeat {
        binning = arrange_bins(binner, x, nbins = (min_binning$nbins + max_binning$nbins) / 2)
        if (isTRUE(binning$height <= maxheight)) {
          # binning is valid, search downwards
          if (binning$nbins - 1 <= min_binning$nbins) {
            # found it, we're done
            break
          }
          max_binning = binning
        } else {
          # binning is not valid, search upwards
          if (binning$nbins + 1 >= max_binning$nbins) {
            # found it, we're done
            binning = max_binning
            break
          }
          min_binning = binning
        }
      }
    }

    # attempt to refine binwidth using optimization.
    # after finding a reasonable candidate based on number of bins, we refine
    # the binwidth around that number of bins using optimization. We do this
    # only as a second step because just using optimization on binwidth as a
    # first step tends to end up in a local minimum, sometimes very far from
    # maxheight.
    candidate_binwidths = c(min_binning$binwidth, max_binning$binwidth, binning$binwidth)
    if (length(unique(candidate_binwidths)) != 1) {
      binwidth = optimize(
        function(binwidth) {
          binning = arrange_bins(binner, x, binwidth = binwidth)
          (binning$height - maxheight)^2
        },
        candidate_binwidths,
        tol = sqrt(.Machine$double.eps)
      )$minimum
      new_binning = arrange_bins(binner, x, binwidth = binwidth)

      # approximate test that binning is valid, used here to tolerate approximation with optimize()
      if (isTRUE(new_binning$height <= maxheight + .Machine$double.eps^0.25)) {
        binning = new_binning
      }
    }
  }

  # check if the selected binning is valid....
  if (isTRUE(binning$height <= maxheight + .Machine$double.eps^0.25)) {
    binning$binwidth
  } else {
    # ... if it isn't, this means we've ended up with some bin that's too
    # tall, probably because we have discrete data --- we'll just
    # conservatively shrink things down so they fit by backing out a bin
    # width that works with the tallest bin
    binning$binwidth * maxheight / binning$height
  }
}
