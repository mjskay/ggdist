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
  out = .find_dotplot_binwidth(
    x = x,
    maxheight = maxheight,
    group = group,
    heightratio = heightratio,
    stackratio = stackratio,
    layout = layout,
    side = side,
    span = span
  )
  attributes(out) = NULL
  out
}

#' `find_dotplot_binwidth()` with additional debug output
#'
#' Results can be plotted with `plot_fdb()` to see the shape of the binwidth-height function.
#' @noRd
.find_dotplot_binwidth = function(
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
  # min_nbins = if (length(x) <= 1) {
  #   1
  # } else {
  #   min(nclass.scott(x), nclass.FD(x), nclass.Sturges(x))
  # }
  # min_nbins = 1
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

  max_binwidth = max(diff(range(x)), maxheight / stackratio / heightratio)
  max_binning = arrange_bins_binner(binwidth = max_binwidth)
  min_binwidth = 0

  eps = .Machine$double.eps^0.2
  height_eps = maxheight * eps
  binwidth_eps = height_eps / heightratio / sqrt(length(x))
  if (isTRUE(max_binning$height <= maxheight + height_eps)) {
    # if the max binning (i.e. the binning at the upper limit of the height we will allow)
    # is valid, then we don't need to search and can just use it.
    binwidth = max_binning$binwidth
    height = max_binning$height

    iter = data.frame(
      x = binwidth,
      y = height,
      method = "max"
    )
  } else {
    # set up initial guesses for the search
    iter = data.frame(
      x = c(0, max_binwidth),
      y = c(0, max_binning$height),
      method = c("min", "max")
    )
    add_guess = function(binwidth, method) {
      binning = arrange_bins_binner(binwidth = binwidth)
      iter <<- rbind(
        iter,
        data.frame(
          x = binning$binwidth,
          y = binning$height,
          method = method
        )
      )
      binning$height
    }

    # make a guess using a density estimator
    # the rough idea here is to estimate the maximum density of the data
    # using a kernel density estimator, then back out a binwidth that
    # would produce the desired maxheight assuming that density
    if (length(x) >= 2) {
      max_density = max(density(x)$y)
      binwidth_dens = (sqrt(4 * max_density * maxheight * length(x) * stackratio^2 + heightratio * (stackratio - 1)^2) + sqrt(heightratio) * (stackratio - 1))/(2 * max_density * sqrt(heightratio) * length(x) * stackratio)
      if (binwidth_dens < max_binwidth) add_guess(binwidth_dens, "density")
    }

    # make a guess using data resolution
    # adding this guess helps a lot with discrete data or when
    # the binwidth is less than the data resolution (since often
    # the relationship between binwidth and height between 0 and
    # this value will be linear, so having a guess here helps the
    # search converge faster)
    binwidth_res = resolution(x, FALSE, FALSE)
    if (binwidth_res < max_binwidth) add_guess(binwidth_res, "resolution")

    # make a guess assuming all data is in one bin
    grid = if (prop_exists(binner, "grid")) binner@grid else 1
    n_to_binwidth = \(n_in_bin, .grid = grid) {
      (maxheight / heightratio) / (n_in_bin / .grid - 1 + 1/stackratio)
    }
    binwidth_to_n = \(binwidth, .grid = grid) {
      (maxheight / heightratio / binwidth + 1 - 1/stackratio) * .grid
    }
    binwidth_max_n = n_to_binwidth(length(x), 1)
    if (binwidth_max_n < max_binwidth) add_guess(binwidth_max_n, "max_n")

    # binary search over number of elements in tallest bin
    min_gte = \(x, target) suppressWarnings(min(x[x >= target]))
    is_min_gte = \(x, target) x == min_gte(x, target)
    max_lte = \(x, target) suppressWarnings(max(x[x <= target]))
    is_max_lte = \(x, target) x == max_lte(x, target)
    n_1 = max(floor(binwidth_to_n(iter$x[is_min_gte(iter$y, maxheight)])), 1)
    n_2 = min(ceiling(binwidth_to_n(iter$x[is_max_lte(iter$y, maxheight)])), length(x))
    while (n_2 - n_1 > 1) {
      # traditional binary search would be:
      # > n_mid = floor((n_1 + n_2) / 2)
      # but we use harmonic mean as this is equivalent to bisection on the scale
      # of binwidth (reciprocal of n) and tends to converge faster
      n_mid = ceiling(2 * n_1 * n_2 / (n_1 + n_2))
      if (n_mid == n_2) break
      binwidth_mid = n_to_binwidth(n_mid)
      height_mid = add_guess(binwidth_mid, "binsearch")
      err_mid = height_mid - maxheight
      if (abs(err_mid) <= height_eps) break
      if (err_mid <= 0) {
        n_2 = n_mid
      } else {
        n_1 = n_mid
      }
    }


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
    # binwidth_2 = (sqrt(4 * max_density * maxheight * length(x) * stackratio^2 + heightratio * (stackratio - 1)^2) + sqrt(heightratio) * (stackratio - 1))/(2 * max_density * sqrt(heightratio) * length(x) * stackratio)
    # binning_2 = arrange_bins_binner(binwidth = binwidth_2)

    # binwidth_1 = binwidth_2 / 2
    # binwidth_1 = resolution(x, FALSE, FALSE)
    # binning_1 = arrange_bins_binner(binwidth = binwidth_1)

    # search for a reasonable binwidth
    # print(binwidth_eps)
    iter = max_f_lte_y(
      function(x) arrange_bins_binner(binwidth = x)$height,
      max_y = maxheight,
      iter = iter,
      eps_x = binwidth_eps,
      eps_y = height_eps
    )$iter
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
    valid = iter$y <= maxheight + height_eps
    i = which.min(abs(iter$y[valid] - maxheight))
    binwidth = iter$x[valid][i]
    height = iter$y[valid][i]
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
    binner = binner,
    iterations = data.frame(i = seq_len(nrow(iter)), width = iter$x, height = iter$y, method = iter$method, chosen = iter$x == binwidth),
    binwidth_eps = binwidth_eps,
    height_err = abs(height - maxheight),
    height_eps = height_eps
  )
}



zero_or_less_old = function(f, xs, ys, eps_x, eps_y, tol = sqrt(.Machine$double.eps), methods = rep("init", length(xs))) {
  x_1 = xs[[1]]
  y_1 = ys[[1]]

  max_i = which.max(xs)
  x_2 = xs[[max_i]]
  y_2 = ys[[max_i]]

  y_best = max(ys[ys <= eps_y])
  best_i = which.max(ys == y_best)
  x_best = xs[[best_i]]
  err_best = abs(y_best)

  bisect = FALSE
  for (i in 1:30) {
    # bisect = (i %% 2 == 0)
    # bisect = FALSE
    if (!bisect) {
      f_approx = splinefun(xs, ys, ties = min, method = "monoH.FC")
      x_new = uniroot(f_approx, lower = min(xs), upper = max(xs), tol = tol)$root
      method_new = "spline"
    } else {
    # if (bisect || x_new <= x_1 || x_new >= x_2) {
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

    if (err_best <= eps_y || (x_2 - x_1) <= eps_x) {
      # found it, we're done
      break
    }

    old_width = x_2 - x_1
    if (y_new > 0) {
      x_2 = x_new
      y_2 = y_new
      i_1 = max(which(xs < x_new & ys < 0))
      x_1 = xs[[i_1]]
      y_1 = ys[[i_1]]
    } else {
      x_1 = x_new
      y_1 = y_new
      i_2 = min(which(xs > x_new & ys > 0))
      x_2 = xs[[i_2]]
      y_2 = ys[[i_2]]
    }

    new_width = x_2 - x_1
    bisect = new_width > 0.8 * old_width
    stopifnot(y_1 < 0, 0 < y_2)
  }

  list(
    x_best = x_best,
    y_best = y_best,
    xs = xs,
    ys = ys,
    methods = methods
  )
}



zero_or_less_mh = function(f, xs, ys, eps_x, eps_y, tol = sqrt(.Machine$double.eps), methods = rep("init", length(xs))) {
  iter = data.frame(x = xs, y = ys, method = methods)

  y_best = max(iter$y[iter$y <= eps_y])
  best_i = which.max(iter$y == y_best)
  x_best = iter$x[[best_i]]
  err_best = abs(y_best)

  err_bests = c(err_best, err_best)

  for (i in 1:40) {
    df = iter[order(iter$x), ]
    df = df[!duplicated(df$x), ]
    df$x_2 = c(df$x[-1], NA)
    df$y_2 = c(df$y[-1], NA)

    # find regions to search further
    df$x_diff = df$x_2 - df$x
    df$y_diff = df$y_2 - df$y
    df$slope = df$y_diff / df$x_diff
    df$sign_change = sign(df$y) != sign(df$y_2)
    df$sign_change_after = c(df$sign_change[-1], FALSE)
    df$sign_change_before = c(FALSE, df$sign_change[-nrow(df)])
    df$slope_sign = sign(df$slope)
    df$slope_change = c(df$slope_sign[-1] != df$slope_sign[-nrow(df)], FALSE)
    df$slope_change_before = c(FALSE, df$slope_change[-nrow(df)]) & FALSE
    df$candidate = (df$sign_change | df$sign_change_before | df$x == x_best | df$x_2 == x_best) %in% TRUE
      # (df$sign_change | df$sign_change_after | df$sign_change_before | (df$slope_change & FALSE) | df$slope_change_before) %in% TRUE &
      # df$x
    df$method =
      ifelse(df$sign_change %in% TRUE, "sign_change",
      # ifelse(df$sign_change_after %in% TRUE, "sign_change_after",
      ifelse(df$sign_change_before %in% TRUE, "sign_change_before",
      ifelse(df$x == x_best, "best_x",
      ifelse(df$x_2 == x_best, "best_x_before",
      ifelse(df$slope_change %in% TRUE, "slope_change",
        "slope_change_before"
      )))))
    # df$x_new = (df$x + df$x_2) / 2
    # df$x_new = ifelse(
    #   df$sign_change %in% TRUE, df$x + df$x_diff / df$y_diff * (-df$y),
    #   (df$x + df$x_2) / 2
    # )

    f_approx = splinefun(iter$x, iter$y, ties = min, method = "monoH.FC")
    # x_new = uniroot(f_approx, lower = min(iter$x), upper = max(iter$x), tol = tol)$root
    # x_new = splinefun(iter$y, iter$x, ties = min, method = "natural")(0)

    # df = df[df$candidate, ]
    # df =
      # rbind(
      # df[df$candidate, c("x_new", "method")]
      # data.frame(x_new = x_new, method = "spline")
    # )

    df_bisect = df[df$candidate, ]
    df_bisect$x_new = (df_bisect$x + df_bisect$x_2) / 2
    df_rf = df[df$candidate, ]
    # df_rf$x_new = df_rf$x + df_rf$x_diff / df_rf$y_diff * (-df_rf$y)
    w1 = if (i %% 3 == 2) 1 else 1 - (i %% 3) / 2
    w2 = if (i %% 3 == 2) 1 else 0.5 + (i %% 2) / 2
    df_rf$x_new = (w1 * df_rf$x * df_rf$y_2 - w2 * df_rf$x_2 * df_rf$y) / (w1 * df_rf$y_2 - w2 * df_rf$y)
    df_rf$method = paste0(df_rf$method, "_rf")

    df = rbind(
      df_bisect[, c("x_new", "method")],
      df_rf[, c("x_new", "method")]
      # data.frame(x_new = (df$x + df$x_2) / 2, method = "lower third"),
      # data.frame(x_new = (df$x + df$x_2) * 2 / 3, method = "upper third"),
          # data.frame(x_new = x_new, method = "spline")
    )

        # df =
      # rbind(
      # df[df$candidate, c("x_new", "method")]
      # data.frame(x_new = x_new, method = "spline")
    # )

    # print(iter)
    # print(i)

    df = df[sapply(df$x_new, \(x_new) all(abs(x_new - iter$x) > eps_x)), ]
    if (nrow(df) == 0) break

    df = df[order(df$x_new), ]
    df = df[c(TRUE, diff(df$x_new) > eps_x), ]
    # if (nrow(df) <= 4) break

    df = df[head(order(
      # f_approx(df$x_new) > 2 * eps_y,
      abs(f_approx(df$x_new)
    )), n = 1), , drop = FALSE]
    # df = df[union(1, nrow(df)), , drop = FALSE]



    for (j in seq_len(nrow(df))) {
      # x_1 = df$x[[j]]
      # x_2 = df$x_2[[j]]
      # print(x_1)
      # print(x_2)
    # bisect = (i %% 2 == 0)
    # bisect = FALSE
    # if (!bisect) {
    #   f_approx = splinefun(xs, ys, ties = min, method = "fmm")-
    #   x_new = uniroot(f_approx, lower = min(xs), upper = max(xs), tol = tol)$root
    #   method_new = "spline"
    # } else {
    # if (bisect || x_new <= x_1 || x_new >= x_2) {
      x_new = df$x_new[[j]]
      method_new = df$method[[j]]
      y_new = f(x_new)
      err_new = abs(y_new)

      iter = rbind(iter, data.frame(x = x_new, y = y_new, method = method_new))

      if (y_new <= eps_y && err_new < err_best) {
        # store the best <= eps_y so far
        x_best = x_new
        y_best = y_new
        err_best = err_new
      }

      if (err_best <= eps_y) break
    }
    if (err_best <= eps_y) break
    # err_bests = c(err_bests, err_best)

    # old_width = x_2 - x_1
    # if (y_new > 0) {
    #   x_2 = x_new
    #   y_2 = y_new
    #   i_1 = max(which(xs < x_new & ys < 0))
    #   x_1 = xs[[i_1]]
    #   y_1 = ys[[i_1]]
    # } else {
    #   x_1 = x_new
    #   y_1 = y_new
    #   i_2 = min(which(xs > x_new & ys > 0))
    #   x_2 = xs[[i_2]]
    #   y_2 = ys[[i_2]]
    # }

    # new_width = x_2 - x_1
    # bisect = new_width > 0.8 * old_width
    # stopifnot(y_1 < 0, 0 < y_2)
  }

  list(
    x_best = x_best,
    y_best = y_best,
    xs = iter$x,
    ys = iter$y,
    methods = iter$method
  )
}

zero_or_less_loess = function(f, xs, ys, eps_x, eps_y, tol = sqrt(.Machine$double.eps), methods = rep("init", length(xs))) {
  iter = data.frame(x = xs, y = ys, method = methods)

  y_best = max(iter$y[iter$y <= eps_y])
  best_i = which.max(iter$y == y_best)
  x_best = iter$x[[best_i]]
  err_best = abs(y_best)

  for (i in 1:20) {
    do_loess = i %% 2 != 0
    if (do_loess) {
      if (nrow(iter) >= 8) {
        m = suppressWarnings(loess(x ~ y, data = iter, span = 1/3))
        pred = \(y) suppressWarnings(predict(m, newdata = data.frame(y = y)))
        df = data.frame(
          x_new = pred(c(-2 * eps_y, 0)),
          method = "loess"
        )
      } else {
        do_loess = FALSE
      }
    }
    # bisect = FALSE #i %% 5 == 0
    # if (bisect) {
    #   df = iter[order(iter$x), ]
    #   df = df[!duplicated(df$x), ]
    #   df$x_2 = c(df$x[-1], NA)
    #   df$y_2 = c(df$y[-1], NA)
    #   df = df[(sign(df$y) != sign(df$y_2)) %in% TRUE, ]
    #   df$x_new = (df$x + df$x_2) / 2
    #   if (nrow(df) > 0) {
    #     df$method = "bisection"
    #   } else {
    #     bisect = FALSE
    #   }
    # }
    if (!do_loess) {
      df = bind_rows(lapply(split_monotonic(iter), \(df) {
        df_lt0 = df[df$y < 0, ]
        df_gt0 = df[df$y > 0, ]
        f_inv_approx = approxfun(df$y, df$x) #, ties = min, method = "monoH.FC")
        bi = max(which(df$y < 0))
        rbind(
          # if (i %% 3 == 0) data.frame(x_new = f_inv_approx(0), method = "linear"),
          # if (i %% 4 == 1) data.frame(x_new = splinefun(df$y, df$x, method = "monoH.FC")(0), method = "spline"),
          # if (i %% 4 == 0)
            data.frame(x_new = (df$x[[bi]] + df$x[[bi + 1]]) / 2, method = "bisection"),
          if (i %% 4 == 0 && nrow(df_gt0) >= 2) data.frame(x_new = splinefun(df_gt0$y, df_gt0$x, method = "monoH.FC")(0), method = "spline_gt0"),
          if (i %% 4 == 0 && nrow(df_lt0) >= 2) data.frame(x_new = splinefun(df_lt0$y, df_lt0$x, method = "monoH.FC")(0), method = "spline_lt0")
        )
      }))
    }

    df = df[!is.na(df$x_new) & sapply(df$x_new, \(x_new) all(abs(x_new - iter$x) > eps_x)), ]
    if (nrow(df) == 0) {
      # next
      if (i %% 4 != 1) next
      break
    }

    df = df[order(df$x_new), ]
    # df = df[!duplicated(df$x_new), ]
    df = df[c(TRUE, diff(df$x_new) > eps_x), ]

    for (j in seq_len(nrow(df))) {
      x_new = df$x_new[[j]]
      method_new = df$method[[j]]
      y_new = f(x_new)
      err_new = abs(y_new)

      iter = rbind(iter, data.frame(x = x_new, y = y_new, method = method_new))

      if (y_new <= eps_y && err_new < err_best) {
        # store the best <= eps_y so far
        x_best = x_new
        y_best = y_new
        err_best = err_new
      }

      if (err_best <= eps_y) break
    }
    if (err_best <= eps_y) break
    # err_bests = c(err_bests, err_best)

    # old_width = x_2 - x_1
    # if (y_new > 0) {
    #   x_2 = x_new
    #   y_2 = y_new
    #   i_1 = max(which(xs < x_new & ys < 0))
    #   x_1 = xs[[i_1]]
    #   y_1 = ys[[i_1]]
    # } else {
    #   x_1 = x_new
    #   y_1 = y_new
    #   i_2 = min(which(xs > x_new & ys > 0))
    #   x_2 = xs[[i_2]]
    #   y_2 = ys[[i_2]]
    # }

    # new_width = x_2 - x_1
    # bisect = new_width > 0.8 * old_width
    # stopifnot(y_1 < 0, 0 < y_2)
  }

  list(
    x_best = x_best,
    y_best = y_best,
    xs = iter$x,
    ys = iter$y,
    methods = iter$method
  )
}


max_f_lte_y = function(
  f, max_y, eps_x, eps_y,
  tol = sqrt(.Machine$double.eps),
  iter = data.frame(x = numeric(), y = numeric(), method = character())
) {
  iter$y = iter$y - max_y

  y_best = max(iter$y[iter$y <= eps_y])
  best_i = which.max(iter$y == y_best)
  x_best = iter$x[[best_i]]
  err_best = abs(y_best)

  for (i in 1:20) {
    if (err_best <= eps_y) break

    stepped_linear_approx_at_0 = function(iter) {
      df = stepped_linear_approx(iter$x, iter$y + max_y, eps_x)
      df$y = df$y - max_y
      df$x_2 = c(df$x[-1], NA)
      df$y_2 = c(df$y[-1], NA)
      crosses_zero = (sign(df$y) != sign(df$y_2)) %in% TRUE
      df = df[crosses_zero, ]
      df$x_new = (df$x * df$y_2 - df$x_2 * df$y) / (df$y_2 - df$y)
      df$method = "stepped"
      df
    }
    df = stepped_linear_approx_at_0(iter)

    # if (FALSE) {
      df = dplyr::bind_rows(df, lapply(split_monotonic(iter), \(df) {
        df_lt0 = df[df$y < 0, ]
        df_gt0 = df[df$y > 0, ]
        f_inv_approx = approxfun(df$y, df$x) #, ties = min, method = "monoH.FC")
        bi = max(which(df$y < 0))
        dplyr::bind_rows(
          # if (i %% 3 == 0) data.frame(x_new = f_inv_approx(0), method = "linear"),
          # if (i %% 4 == 1) data.frame(x_new = splinefun(df$y, df$x, method = "monoH.FC")(0), method = "spline"),
          # if (i %% 4 == 1)
            transform(stepped_linear_approx_at_0(df), method = "stepped_mono")
          # data.frame(x_new = (df$x[[bi]] + df$x[[bi + 1]]) / 2, method = "bisection"),
          # if (nrow(df_gt0) >= 2) data.frame(x_new = splinefun(df_gt0$y, df_gt0$x, method = "monoH.FC")(0), method = "spline_gt0"),
          # if (nrow(df_lt0) >= 2) data.frame(x_new = splinefun(df_lt0$y, df_lt0$x, method = "monoH.FC")(0), method = "spline_lt0")
        )
      }))
    # }

    df = df[!is.na(df$x_new) & sapply(df$x_new, \(x_new) all(abs(x_new - iter$x) > eps_x/2)), ]
    if (nrow(df) == 0) {
      # next
      # if (i %% 4 != 1) next
      break
    }

    df = df[order(df$x_new), ]
    # df = df[!duplicated(df$x_new), ]
    df = df[c(TRUE, diff(df$x_new) > eps_x), ]

    for (j in seq_len(nrow(df))) {
      x_new = df$x_new[[j]]
      method_new = df$method[[j]]
      y_new = f(x_new) - max_y
      err_new = abs(y_new)

      iter = rbind(iter, data.frame(x = x_new, y = y_new, method = method_new))

      if (y_new <= eps_y && err_new < err_best) {
        # store the best <= eps_y so far
        x_best = x_new
        y_best = y_new
        err_best = err_new
      }

      if (err_best <= eps_y) break
    }
    # err_bests = c(err_bests, err_best)

    # old_width = x_2 - x_1
    # if (y_new > 0) {
    #   x_2 = x_new
    #   y_2 = y_new
    #   i_1 = max(which(xs < x_new & ys < 0))
    #   x_1 = xs[[i_1]]
    #   y_1 = ys[[i_1]]
    # } else {
    #   x_1 = x_new
    #   y_1 = y_new
    #   i_2 = min(which(xs > x_new & ys > 0))
    #   x_2 = xs[[i_2]]
    #   y_2 = ys[[i_2]]
    # }

    # new_width = x_2 - x_1
    # bisect = new_width > 0.8 * old_width
    # stopifnot(y_1 < 0, 0 < y_2)
  }

  iter$y = iter$y + max_y
  list(
    x_best = x_best,
    y_best = y_best,
    iter = iter
  )
}

zero_or_less_ur = function(f, xs, ys, eps_x, eps_y, tol = sqrt(.Machine$double.eps), methods = rep("init", length(xs))) {
  iter = data.frame(x = xs, y = ys, method = methods)

  y_best = max(iter$y[iter$y <= eps_y])
  best_i = which.max(iter$y == y_best)
  x_best = iter$x[[best_i]]

  res = uniroot(
    # x_best,
    \(x) {
      i = match(x, iter$x)
      if (!is.na(i)) {
        y = iter$y[i]
      } else {
        y = f(x)
      }
      iter <<- rbind(iter, data.frame(x = x, y = y, method = "optimize"))
      y
    },
    lower = min(iter$x),
    upper = max(iter$x)
    # method = "Nelder-Mead"
  )

  list(
    x_best = res$root,
    y_best = res$f.root,
    xs = iter$x,
    ys = iter$y,
    methods = iter$method
  )
}

n_to_binwidth = \(n_in_bin, binner, grid = if (prop_exists(binner, "grid")) binner@grid else 1) {
  (binner@maxheight / binner@heightratio) / (n_in_bin / grid - 1 + 1/binner@stackratio)
}
binwidth_to_n = \(binwidth, binner, grid = if (prop_exists(binner, "grid")) binner@grid else 1) {
  (binner@maxheight / binner@heightratio / binwidth + 1 - 1/binner@stackratio) * grid
}
pseudo_n_to_binwidth = \(pseudo_n, binner, eps) {
  n_in_bin = floor(pseudo_n + 0.5)
  rel_pos = 1 - 2 * ((pseudo_n + 0.5) %% 1)
  n_to_binwidth(n_in_bin, binner) * (1 + rel_pos * eps)
}
binwidth_to_pseudo_n = \(binwidth, binner, eps) {
  n_in_bin = round(binwidth_to_n(binwidth, binner))
  ref_binwidth = n_to_binwidth(n_in_bin, binner)
  rel_pos = pmin(1, pmax(-1, (binwidth - ref_binwidth) / (ref_binwidth * eps)))
  n_in_bin - rel_pos / 2
}
pseudo_binwidth_to_binwidth = \(pseudo_binwidth, binner, eps) {
  pseudo_n_to_binwidth(binwidth_to_n(pseudo_binwidth, binner), binner, eps)
}
binwidth_to_pseudo_binwidth = \(binwidth, binner, eps) {
  n_to_binwidth(binwidth_to_pseudo_n(binwidth, binner, eps), binner)
}

plot_fdb = function(fdb, zoom = .85, ...) {
  iters = attr(fdb, "iterations")
  binner = attr(fdb, "binner")
  height_eps = attr(fdb, "height_eps")
  p_range_around = \(x, i, p) {
    center = x[i][[1]]
    range = quantile(x[x >= center], p) - quantile(x[x <= center], 1 - p)
    c(max(0, center - range/2), center + range/2)
  }
  xlim = p_range_around(iters$width, iters$chosen, zoom)
  ylim = p_range_around(iters$height, iters$chosen, zoom)

  high_res_curve = tibble(
    width = seq(xlim[1], xlim[2], length.out = 200),
    height = sapply(width, \(x) arrange_bins(binner, binwidth = x)$height)
  )

  grid = if (prop_exists(binner, "grid")) binner@grid else 1
  transform_n = scales::new_transform("binwidth", \(bw) binwidth_to_n(bw, binner), \(n) n_to_binwidth(n, binner))
  transform_pseudo_n = scales::new_transform("binwidth", \(bw) binwidth_to_pseudo_n(bw, binner, height_eps), \(n) pseudo_n_to_binwidth(n, binner, height_eps))
  transform_pseudo_binwidth = scales::new_transform("binwidth", \(bw) binwidth_to_pseudo_binwidth(bw, binner, height_eps), \(n) pseudo_binwidth_to_binwidth(n, binner, height_eps))

  exact_refs = data.frame(
    height = binner@maxheight,
    width = n_to_binwidth(seq(round(binwidth_to_n(max(setdiff(iters$height, Inf), na.rm = TRUE), binner)), round(binwidth_to_n(min(setdiff(iters$height, 0), na.rm = TRUE), binner))), binner)
  )
  exact_refs = exact_refs[xlim[1] <= exact_refs$width & exact_refs$width <= xlim[2], ]

  iters |>
    dplyr::filter(...) |>
    ggplot(aes(width, height)) +
    annotate("ribbon", x = c(0.11, 0.12), ymin = binner@maxheight - attr(fdb, "height_eps"), ymax = binner@maxheight + attr(fdb, "height_eps"), alpha = 0.1) +
    geom_hline(yintercept = c(binner@maxheight - height_eps, binner@maxheight + height_eps), alpha = 0.5) +
    # geom_abline(slope = seq(1, max(iters$height/binner@heightratio/iters$width, na.rm = TRUE), by = 1/grid) + 1/binner@stackratio, color = "gray85") +
    geom_line(
      color = "blue",
      alpha = 0.8,
      data = high_res_curve
    ) +
    geom_point(
      size = 0.5,
      color = "blue",
      alpha = 0.5,
      data = high_res_curve
    ) +
    geom_line(
      aes(group = split),
      data = split_monotonic(iters, "width", "height") |> dplyr::bind_rows(.id = "split"),
      color = "gray65"
    ) +
    geom_point(aes(color = method)) +
    geom_point(data = iters[iters$chosen, ], shape = 12, size = 3) +
    geom_hline(yintercept = binner@maxheight, linetype = "dashed") +
    geom_abline(intercept = binner@maxheight, slope = c(binner@heightratio, -binner@heightratio), linetype = "dotted") +
    geom_point(shape = 1, size = 2, data = exact_refs) +
    coord_cartesian(xlim = xlim, ylim = ylim)
    # coord_transform(xlim = xlim, ylim = ylim, x = scales::transform_reciprocal())
    # coord_transform(xlim = xlim, ylim = ylim, x = transform_n)

}


split_monotonic = function(iter, x = "x", y = "y") {
  iter = iter[order(iter[[x]]), ]
  iter = iter[!duplicated(iter[[x]]), ]
  rev_cummin = \(x) rev(cummin(rev(x)))

  splits = list()
  while (TRUE) {
    # construct a monotonic split from the end
    max_y = rev_cummin(iter[[y]])
    iter$in_split = iter[[y]] == max_y
    split = iter[iter$in_split, ]
    splits = c(splits, list(split))
    if (all(iter$in_split)) break

    # remove any points after the last point in the split
    # that are less than the last point in the split
    last_in_split = tail(iter[!iter$in_split, ], n = 1)
    iter = iter[iter[[x]] <= last_in_split[[x]] | iter[[y]] >= last_in_split[[y]], ]
  }
  splits
}


stepped_linear_approx = function(x, y, eps_x = .Machine$double.eps^0.25) {
  ord = order(x)
  x = x[ord]
  y = y[ord]

  eps_x = min(eps_x, diff(unique(x)) / 2)

  n = length(x)
  x_1 = x[-n]
  y_1 = y[-n]
  x_4 = x[-1]
  y_4 = y[-1]

  x_2 = (x_1 + x_4 - eps_x) / 2
  y_2 = y_1 / x_1 * x_2
  x_3 = (x_1 + x_4 + eps_x) / 2
  y_3 = y_4 / x_4 * x_3

  data.frame(
    x = vec_interleave(x_1, x_2, x_3, x_4),
    y = vec_interleave(y_1, y_2, y_3, y_4)
  )
}
