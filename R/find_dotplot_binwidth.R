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
#' @eval rd_param_side("dots")
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
  group = 1L,
  heightratio = 1,
  stackratio = 1,
  layout = c("bin", "weave", "hex", "swarm", "swarm2", "bar"),
  side = c("topright", "top", "right", "bottomleft", "bottom", "left", "topleft", "bottomright", "both"),
  span = waiver()
) {
  side = match.arg(side)

  d = data_frame0(x = as.numeric(x), group = group)
  d = d[order(d$x, d$group), ]
  x = d$x
  group = d$group

  # figure out a reasonable minimum number of bins based on histogram binning
  # TODO: remove
  min_nbins = if (length(x) <= 1) {
    1
  } else {
    min(nclass.scott(x), nclass.FD(x), nclass.Sturges(x))
  }
  min_nbins = 1
  binner = new_binner(
    layout,
    x,
    group = group,
    maxheight = maxheight,
    heightratio = heightratio,
    stackratio = stackratio,
    side = side,
    span = span
  )

  arrange_bins_ = method(arrange_bins, object = binner)
  arrange_bins_binner = function(...) arrange_bins_(binner, ...)
  max_binning = arrange_bins_binner(nbins = min_nbins)
  max_binwidth = max_binning$binwidth
  min_binwidth = 0

  eps = .Machine$double.eps^0.25
  height_eps = maxheight * eps
  binwidth_eps = height_eps / heightratio / sqrt(length(x))
  if (isTRUE(max_binning$height <= maxheight + height_eps)) {
    # if the max binning (i.e. the binning constructed from the smallest
    # number of bins --- thus, at the upper limit of the height we will allow)
    # is valid, then we don't need to search and can just use it.
    binwidth = max_binning$binwidth
    height = max_binning$height

    widths = binwidth
    heights = height
    methods = "start"
  } else {
    # make a first guess using a density estimator
    max_density = max(density(x)$y)

    # max_bin_count = density * n * binwidth - 1 + 1/stackratio
    # maxheight = (density * n * binwidth - 1 + 1/stackratio) * binwidth * heightratio
    # binwidth_1 = (sqrt(4 * max_density * maxheight * length(x) * stackratio^2 + heightratio * (stackratio - 1)^2) + sqrt(heightratio) * (stackratio - 1))/(2 * max_density * sqrt(heightratio) * length(x) * stackratio)

    # 0 = density * length(x) * binwidth^2 - binwidth * (1 + 1 / stackratio) - maxheight / heightratio
    # a = max_density * length(x)
    # b = (1 + 1 / stackratio)
    # c = maxheight / heightratio
    # cat(a, b, c)
    # binwidth_1 = (-b + sqrt(b^2 - 4*a*c)) / (2*a)
    # print(binwidth_1, maxheight / (length(x) * max_density * heightratio))
    # binwidth_1 = maxheight / (length(x) * max_density * heightratio * (1 + 1 / stackratio))
    binwidth_2 = (sqrt(4 * max_density * maxheight * length(x) * stackratio^2 + heightratio * (stackratio - 1)^2) + sqrt(heightratio) * (stackratio - 1))/(2 * max_density * sqrt(heightratio) * length(x) * stackratio)
    binning_2 = arrange_bins_binner(binwidth = binwidth_2)

    binwidth_1 = binwidth_2 / 2
    binning_1 = arrange_bins_binner(binwidth = binwidth_1)

    # search for a reasonable binwidth
    # print(binwidth_eps)
    zero = zero_or_less(
      function(x) arrange_bins_binner(binwidth = x)$height - maxheight,
      xs = c(0,
        # binning_1$binwidth,
        # binning_2$binwidth,
        max_binning$binwidth),
      ys = c(0,
        # binning_1$height,
        # binning_2$height,
        max_binning$height) - maxheight,
      binwidth_eps,
      height_eps
    )
    widths = zero$xs
    heights = zero$ys + maxheight
    methods = zero$methods
    binwidth = zero$x_best
    # cat("Search iterations:", length(widths), "\n")

    # attempt to refine binwidth using optimization.
    # after finding a reasonable candidate based on number of bins, we refine
    # the binwidth around that number of bins using optimization. We do this
    # only as a second step because just using optimization on binwidth as a
    # first step tends to end up in a local minimum, sometimes very far from
    # maxheight.
    # if (abs(zero$y_best) > height_eps) {
    #   candidate_binwidths = c(zero$x_1, zero$x_2, zero$x_best) #c(min_binning$binwidth, max_binning$binwidth, binning$binwidth)
    #   if (length(unique(candidate_binwidths)) != 1) {
    #     opt = optimize(
    #       function(binwidth) {
    #         binning = arrange_bins_binner(binwidth = binwidth)
    #         abs(binning$height - maxheight)
    #       },
    #       candidate_binwidths,
    #       tol = binwidth_eps
    #     )
    #     new_binning = arrange_bins_binner(binwidth = opt$minimum)

    #     # approximate test that binning is valid, used here to tolerate approximation with optimize()
    #     new_err = new_binning$height - maxheight
    #     new_abs_err = abs(new_err)
    #     if (isTRUE(new_err <= height_eps && new_abs_err < abs(zero$y_best))) {
    #       binwidth = opt$minimum
    #     }
    #   }
    # }
    valid = heights <= maxheight + height_eps
    i = which.min(abs(heights[valid] - maxheight))
    binwidth = widths[valid][i]
    height = heights[valid][i]
  }

  # check if the selected binning is valid....
  # cat("Total iterations:", length(widths), "\n")
  # print(binning$binwidth)
  # print(binning$height - maxheight)
  # if (isTRUE(binning$height <= maxheight + height_eps)) {
  #   binning$binwidth
  # } else {
  #   # ... if it isn't, this means we've ended up with some bin that's too
  #   # tall, probably because we have discrete data --- we'll just
  #   # conservatively shrink things down so they fit by backing out a bin
  #   # width that works with the tallest bin
  #   binning$binwidth * maxheight / binning$height
  # }
  structure(
    binwidth,
    iterations = data.frame(i = seq_along(widths), widths, heights, methods, chosen = widths == binwidth),
    binwidth_eps = binwidth_eps,
    height_err = abs(height - maxheight),
    height_eps = height_eps
  )
}



zero_or_less = function(f, xs, ys, eps_x, eps_y, tol = sqrt(.Machine$double.eps)) {
  x_1 = xs[[1]]
  y_1 = ys[[1]]
  x_2 = xs[[length(xs)]]
  y_2 = ys[[length(ys)]]
  x_best = x_1
  y_best = y_1
  err_best = abs(y_best)

  methods = rep("start", length(xs))
  for (i in 1:30) {
    f_approx = splinefun(xs, ys, ties = min, method = "monoH.FC")
    x_new = uniroot(f_approx, lower = x_1, upper = x_2, tol = tol)$root
    method_new = "spline"
    if (x_new <= x_1 || x_new >= x_2) {
      # when the root found by spline approximation lies outside the
      # search interval, use bisection
      x_new = (x_1 + x_2) / 2
      method_new = "bisection"
    }
    y_new = f(x_new)
    err_new = abs(y_new)

    xs = c(xs, x_new)
    ys = c(ys, y_new)
    methods = c(methods, method_new)

    if (y_new <= eps_y && err_new < err_best) {
      # store the best <= eps_y so far
      x_best = x_new
      y_best = y_new
      err_best = err_new
    }

    if (err_new <= eps_y || (x_2 - x_1) < 2 * eps_x) {
      # found it, we're done
        break
    }

    if (y_new > 0) {
      # search downwards
      x_2 = x_new
      y_2 = y_new
    } else {
      # search upwards
      x_1 = x_new
      y_1 = y_new
    }
    stopifnot(y_1 < 0, 0 < y_2)
  }

  list(
    x_1 = x_1,
    x_best = x_best,
    x_2 = x_2,
    y_1 = y_1,
    y_best = y_best,
    y_2 = y_2,
    xs = xs,
    ys = ys,
    methods = methods
  )
}
