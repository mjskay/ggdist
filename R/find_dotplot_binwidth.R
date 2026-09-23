# dynamic binwidth selection ----------------------------------------------

#' Dynamically select a good bin width for a dotplot
#'
#' Searches for a nice-looking bin width to use to draw a dotplot such that
#' the height of the dotplot fits within a given space (`maxheight`).
#'
#' @param x <[numeric]> Data values.
#' @param maxheight <scalar [numeric]> Maximum height of the dotplot.
#' @inheritParams bin_dots
#' @inheritDotParams dotplot_layout -dots
#'
#' @details
#' The dynamic bin selection algorithm uses a variety of heuristics and search algorithms to attempt
#' to quickly find a `binwidth` for the provided `layout` such that the height of the tallest bin
#' will be less than or equal to `maxheight` + \eqn{\epsilon} for an internally-determined relative
#' \eqn{\epsilon} (which depends on `maxheight`).
#'
#' `find_dotplot_binwidth` internally builds up an approximation of the height function (the
#' function from `binwidth` onto the height of the dotplot with that bindwidth) and uses it to
#' select the next candidate `binwidth` to evaluate. Because dotplot height functions tend to be
#' discontinuous, using an off-the-self numerical optimization tends to require many iterations,
#' which can be expensive for large dotplots particularly on some layout algorithms. We employ a
#' number of heuristics to narrow the search space and to select `binwidth`s to evaluate:
#'
#' - We evaluate the function intially at key positions that help narrow the search or which define
#' known boundaries between regions of the function that behave differently. For example, between
#' \eqn{[0,0]} (the origin of all height functions) and a binwidth of `resolution(x)`, the height
#' function will be linear, so evaluating the height at `resolution(x)` allows linear interpolation
#' to find the correct `binwidth` if it is less than `resolution(x)`.
#'
#' - For certain layouts, e.g. `layout_bin()`, because the height is a function of the number of
#' dots in a particular bin, there are a finite number of `binwidth`s that can have *exactly* the
#' requested `maxheight`. We use binary search over these points to attempt to find an exact
#' solution.
#'
#' - If binary search fails to find an exact solution, we repeatedly use interpolation with the
#' points we have evaluated so far to generate candidate `binwidth`s to check. Intead of using
#' piecewise linear interpolation, we assume that the region around a point \eqn{(w, h)} (until a
#' discontinuity halfway between that point and the next evaluated point) has a slope of
#' \eqn{\frac{h}{w}}. This is justifiable because locally a dotplot's height will be directly
#' proportional to changes in `binwidth` up to the point where the new `binwidth` causes dots to be
#' added to or removed from the tallest bin.
#'
#' If we fail to find a `binwidth` yielding a height within \eqn{\pm \epsilon} of `maxheight`, the
#' `binwidth` yielding the largest dotplot height less than or equal to `maxheight` + \eqn{\epislon}
#' is returned.
#'
#' This algorithm is used by [geom_dotsinterval()] (and its variants) to automatically select bin
#' widths. Unless you are manually implementing your own dotplot [`grob`] or
#' [`geom`][ggplot2::Geom], you probably do not need to use this function directly.
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
#' # we can manually plot the dotplot above, though this is only recommended
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
  ...,
  group = 1L,
  heightratio = 1,
  stackratio = 1,
  layout = c("bin", "weave", "hex", "swarm", "bar"),
  side = c("topright", "top", "right", "bottomleft", "bottom", "left", "topleft", "bottomright", "both")
) {
  out = .find_dotplot_binwidth(
    x = x,
    maxheight = maxheight,
    group = group,
    heightratio = heightratio,
    stackratio = stackratio,
    layout = layout,
    side = side,
    ...
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
  layout = c("bin", "weave", "hex", "swarm", "bar"),
  side = c("topright", "top", "right", "bottomleft", "bottom", "left", "topleft", "bottomright", "both"),
  ...
) {
  # orientation doesn't matter for finding binwidth, so we can arbitrarily assign it to be "horizontal"
  side = switch_side(match.arg(side), orientation = "horizontal",
    topright = "top",
    bottomleft = "bottom",
    both = "both"
  )

  dots = setup_dots(x = as.numeric(x), y = 0, group = group)
  x = dots$x
  group = dots$group

  layout = match_function(layout, "layout_")(
    dots = dots,
    maxheight = maxheight,
    heightratio = heightratio,
    stackratio = stackratio,
    side = side,
    ...
  )

  # find the implementation of setup_dotplot for this layout type so
  # we don't have to incur method lookup cost during each optimization step
  `setup_dotplot<layout>` = method(setup_dotplot, object = layout)
  setup_dotplot_ = function(binwidth) `setup_dotplot<layout>`(layout, binwidth = binwidth)

  max_binwidth = max(diff(range(x)), maxheight / stackratio / heightratio)
  max_dotplot = setup_dotplot_(max_binwidth)
  min_binwidth = 0

  eps = .Machine$double.eps^0.2
  height_eps = maxheight * eps
  binwidth_eps = height_eps / heightratio / sqrt(length(x))
  if (isTRUE(max_dotplot$height <= maxheight + height_eps)) {
    # if the max dotplot (i.e. the dotplot at the upper limit of the height we will allow)
    # is valid, then we don't need to search and can just use it.
    binwidth = max_dotplot$binwidth
    height = max_dotplot$height

    iter = data.frame(
      x = binwidth,
      y = height,
      method = "max"
    )
  } else {
    # set up initial guesses for the search
    iter = data.frame(
      x = c(0, max_binwidth),
      y = c(0, max_dotplot$height),
      method = c("min", "max")
    )
    add_guess = function(binwidth, method) {
      dotplot = setup_dotplot_(binwidth)
      iter <<- rbind(
        iter,
        data.frame(
          x = dotplot$binwidth,
          y = dotplot$height,
          method = method
        )
      )
      dotplot$height
    }

    # TODO: re-order all the guesses here to do something like:
    # min, resolution, max_tallest_bin_n, density
    # and only keep adding guesses if we haven't hit one above the target height

    strata = if (prop_exists(layout, "strata")) layout@strata else 1
    strata = min(strata, 8)

    # number of elements in a bin of equal values stacked to exactly maxheight -> binwidth
    # (when strata = 1)
    n_to_binwidth = \(n_in_bin, .strata = strata) {
      (maxheight / heightratio) / (n_in_bin / .strata - 1 + 1/stackratio)
    }
    # binwidth -> number of elements in a bin of equal values stacked to exactly maxheight
    # (when strata = 1)
    binwidth_to_n = \(binwidth, .strata = strata) {
      (maxheight / heightratio / binwidth + 1 - 1/stackratio) * .strata
    }

    # make a guess using a density estimator
    # the rough idea here is to estimate the maximum density of the data
    # using a kernel density estimator, then back out a binwidth that
    # would produce the desired maxheight assuming that density
    if (length(x) >= 2) {
      max_density = max(density(x)$y)
      binwidth_dens = (
        sqrt(4 * max_density * maxheight * length(x) * stackratio^2 + heightratio * (stackratio - 1)^2) +
        sqrt(heightratio) * (stackratio - 1)
      ) /
        (2 * max_density * sqrt(heightratio) * length(x) * stackratio)
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
    binwidth_max_n = n_to_binwidth(length(x), .strata = 1)
    if (binwidth_max_n < max_binwidth) add_guess(binwidth_max_n, "max_tallest_bin_n")

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
    # dotplot_2 = setup_dotplot_(binwidth = binwidth_2)

    # binwidth_1 = binwidth_2 / 2
    # binwidth_1 = resolution(x, FALSE, FALSE)
    # dotplot_1 = setup_dotplot_(binwidth = binwidth_1)

    # search for a reasonable binwidth
    # print(binwidth_eps)
    iter = max_f_lte_y(
      function(x) setup_dotplot_(binwidth = x)$height,
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
    #   candidate_binwidths = c(zero$x_1, zero$x_2, zero$x_best) #c(min_dotplot$binwidth, max_dotplot$binwidth, dotplot$binwidth)
    #   if (length(unique(candidate_binwidths)) != 1) {
    #     opt = optimize(
    #       function(binwidth) {
    #         dotplot = setup_dotplot_(binwidth = binwidth)
    #         abs(dotplot$height - maxheight)
    #       },
    #       candidate_binwidths,
    #       tol = binwidth_eps
    #     )
    #     new_dotplot = setup_dotplot_(binwidth = opt$minimum)

    #     # approximate test that dotplot is valid, used here to tolerate approximation with optimize()
    #     new_err = new_dotplot$height - maxheight
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

  # check if the selected dotplot is valid....
  # cat("Total iterations:", length(widths), "\n")
  # print(dotplot$binwidth)
  # print(dotplot$height - maxheight)
  # if (isTRUE(dotplot$height <= maxheight + height_eps)) {
  #   dotplot$binwidth
  # } else {
  #   # ... if it isn't, this means we've ended up with some bin that's too
  #   # tall, probably because we have discrete data --- we'll just
  #   # conservatively shrink things down so they fit by backing out a bin
  #   # width that works with the tallest bin
  #   dotplot$binwidth * maxheight / dotplot$height
  # }
  structure(
    binwidth,
    layout = layout,
    iterations = data.frame(i = seq_len(nrow(iter)), width = iter$x, height = iter$y, method = iter$method, chosen = iter$x == binwidth),
    binwidth_eps = binwidth_eps,
    height_err = abs(height - maxheight),
    height_eps = height_eps
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
    # TODO: ensure stepped is a subset of stepped_mono then remove the stepped stuff above
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

n_to_binwidth = \(n_in_bin, layout, strata = if (prop_exists(layout, "strata")) layout@strata else 1) {
  (layout@maxheight / layout@heightratio) / (n_in_bin / strata - 1 + 1/layout@stackratio)
}
binwidth_to_n = \(binwidth, layout, strata = if (prop_exists(layout, "strata")) layout@strata else 1) {
  (layout@maxheight / layout@heightratio / binwidth + 1 - 1/layout@stackratio) * strata
}
pseudo_n_to_binwidth = \(pseudo_n, layout, eps) {
  n_in_bin = floor(pseudo_n + 0.5)
  rel_pos = 1 - 2 * ((pseudo_n + 0.5) %% 1)
  n_to_binwidth(n_in_bin, layout) * (1 + rel_pos * eps)
}
binwidth_to_pseudo_n = \(binwidth, layout, eps) {
  n_in_bin = round(binwidth_to_n(binwidth, layout))
  ref_binwidth = n_to_binwidth(n_in_bin, layout)
  rel_pos = pmin(1, pmax(-1, (binwidth - ref_binwidth) / (ref_binwidth * eps)))
  n_in_bin - rel_pos / 2
}
pseudo_binwidth_to_binwidth = \(pseudo_binwidth, layout, eps) {
  pseudo_n_to_binwidth(binwidth_to_n(pseudo_binwidth, layout), layout, eps)
}
binwidth_to_pseudo_binwidth = \(binwidth, layout, eps) {
  n_to_binwidth(binwidth_to_pseudo_n(binwidth, layout, eps), layout)
}

plot_fdb = function(fdb, zoom = .85, ...) {
  iters = attr(fdb, "iterations")
  layout = attr(fdb, "layout")
  height_eps = attr(fdb, "height_eps")
  p_range_around = \(x, i, p) {
    center = x[i][[1]]
    range = quantile(x[x >= center], p) - quantile(x[x <= center], 1 - p)
    c(max(0, min(center - range/2, quantile(x, (1 - p)/2))), min(max(x), max(center + range/2, quantile(x, (1 + p)/2))))
  }
  xlim = p_range_around(iters$width, iters$chosen, zoom)
  ylim = p_range_around(iters$height, iters$chosen, zoom)

  high_res_curve = tibble(
    width = seq(xlim[1], xlim[2], length.out = 50),
    height = sapply(width, \(x) setup_dotplot(layout, binwidth = x)$height)
  ) |>
    vctrs::vec_rbind(iters[c("width", "height")]) |>
    vctrs::vec_sort()

  strata = if (prop_exists(layout, "strata")) layout@strata else 1
  transform_n = scales::new_transform("binwidth", \(bw) binwidth_to_n(bw, layout), \(n) n_to_binwidth(n, layout))
  transform_pseudo_n = scales::new_transform("binwidth", \(bw) binwidth_to_pseudo_n(bw, layout, height_eps), \(n) pseudo_n_to_binwidth(n, layout, height_eps))
  transform_pseudo_binwidth = scales::new_transform("binwidth", \(bw) binwidth_to_pseudo_binwidth(bw, layout, height_eps), \(n) pseudo_binwidth_to_binwidth(n, layout, height_eps))

  iters |>
    dplyr::filter(...) |>
    ggplot(aes(width, height)) +
    annotate("ribbon", x = c(0.11, 0.12), ymin = layout@maxheight - attr(fdb, "height_eps"), ymax = layout@maxheight + attr(fdb, "height_eps"), alpha = 0.1) +
    geom_hline(yintercept = c(layout@maxheight - height_eps, layout@maxheight + height_eps), alpha = 0.5) +
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
      data = vctrs::vec_rbind(!!!split_monotonic(iters, "width", "height"), .names_to = "split"),
      color = "gray65"
    ) +
    geom_point(aes(color = method)) +
    geom_point(data = iters[iters$chosen, ], shape = 12, size = 3) +
    geom_hline(yintercept = layout@maxheight, linetype = "dashed") +
    geom_abline(intercept = layout@maxheight, slope = c(layout@heightratio, -layout@heightratio), linetype = "dotted") +
    coord_cartesian(xlim = xlim, ylim = ylim)
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
