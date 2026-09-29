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

  # set up initial guesses for the search
  max_binwidth = max(diff(range(x)), maxheight / stackratio / heightratio)
  max_dotplot = setup_dotplot_(max_binwidth)
  iter = data_frame0(
    x = c(0, max_binwidth),
    y = c(0, max_dotplot$height),
    method = c("min", "max")
  )

  eps = .Machine$double.eps^0.2
  height_eps = maxheight * eps
  binwidth_eps = height_eps / heightratio / sqrt(length(x))
  if (isTRUE(max_dotplot$height <= maxheight + height_eps)) {
    # if the max dotplot (i.e. the dotplot at the upper limit of the height we will allow)
    # is valid, then we don't need to search and can just use it.
    binwidth = max_binwidth
    height = max_dotplot$height
  } else {
    add_guess = function(binwidth, method) {
      dotplot = setup_dotplot_(binwidth)
      iter <<- vec_rbind(
        iter,
        data_frame0(
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
      binwidth_dens =
        (
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
    iterations = data_frame0(i = seq_len(nrow(iter)), width = iter$x, height = iter$y, method = iter$method, chosen = iter$x == binwidth),
    binwidth_eps = binwidth_eps,
    height_err = abs(height - maxheight),
    height_eps = height_eps
  )
}



max_f_lte_y = function(
  f, max_y, eps_x, eps_y,
  iter = data_frame0(x = numeric(), y = numeric(), method = character())
) {
  max_y_plus_eps = max_y + eps_y
  y_best = max(iter$y[iter$y <= max_y_plus_eps])
  x_best = iter$x[[which.max(iter$y == y_best)]]
  err_best = abs(y_best - max_y)

  for (i in 1:20) {
    if (err_best <= eps_y) break

    # get candidate xs using stepped linear approximation on monotonic subsets of already-checked
    # points: this tends to work well because of where discontinuities lie in the binwidth -> height
    # function, and even when the underlying assumptions about the shape of that function do not
    # hold, will amount to bisection by including the point halfway between points on either side of
    # the target y value.
    # x_cand = stepped_linear_approx_at_y(iter$x, iter$y, target_y = max_y, eps_x = eps_x)
    x_cand = split_monotonic(iter$x, iter$y) |>
      lapply(\(df) stepped_linear_approx_at_y(df$x, df$y, target_y = max_y, eps_x = eps_x)) |>
      unlist(recursive = FALSE)

    # drop non-finite and already-checked (within eps/2) xs
    x_cand = x_cand[is.finite(x_cand) & map_lgl_(x_cand, \(x) all(abs(x - iter$x) > eps_x/2))]
    if (length(x_cand) == 0) break

    # drop duplicate candidates (within eps)
    x_cand = sort(x_cand)
    x_cand = x_cand[c(TRUE, diff(x_cand) > eps_x)]

    # check new candidates
    for (x_new in x_cand) {
      y_new = f(x_new)
      err_new = abs(y_new - max_y)

      iter = vec_rbind(iter, data_frame0(x = x_new, y = y_new, method = "stepped_mono"))

      if (y_new <= max_y_plus_eps && err_new < err_best) {
        x_best = x_new
        y_best = y_new
        err_best = err_new
      }

      if (err_best <= eps_y) break
    }
  }

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
  iter = attr(fdb, "iterations")
  layout = attr(fdb, "layout")
  height_eps = attr(fdb, "height_eps")
  p_range_around = \(x, i, p) {
    center = x[i][[1]]
    range = quantile(x[x >= center], p) - quantile(x[x <= center], 1 - p)
    c(max(0, min(center - range/2, quantile(x, (1 - p)/2))), min(max(x), max(center + range/2, quantile(x, (1 + p)/2))))
  }
  xlim = p_range_around(iter$width, iter$chosen, zoom)
  ylim = p_range_around(iter$height, iter$chosen, zoom)

  high_res_curve = tibble(
    width = seq(xlim[1], xlim[2], length.out = 50),
    height = sapply(width, \(x) setup_dotplot(layout, binwidth = x)$height)
  ) |>
    vctrs::vec_rbind(iter[c("width", "height")]) |>
    vctrs::vec_sort()

  strata = if (prop_exists(layout, "strata")) layout@strata else 1
  transform_n = scales::new_transform("binwidth", \(bw) binwidth_to_n(bw, layout), \(n) n_to_binwidth(n, layout))
  transform_pseudo_n = scales::new_transform("binwidth", \(bw) binwidth_to_pseudo_n(bw, layout, height_eps), \(n) pseudo_n_to_binwidth(n, layout, height_eps))
  transform_pseudo_binwidth = scales::new_transform("binwidth", \(bw) binwidth_to_pseudo_binwidth(bw, layout, height_eps), \(n) pseudo_binwidth_to_binwidth(n, layout, height_eps))

  iter |>
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
      aes(x = x, y = y, group = split),
      data = vctrs::vec_rbind(!!!split_monotonic(iter$width, iter$height), .names_to = "split"),
      color = "gray65"
    ) +
    geom_point(aes(color = method)) +
    geom_point(data = iter[iter$chosen, ], shape = 12, size = 3) +
    geom_hline(yintercept = layout@maxheight, linetype = "dashed") +
    geom_abline(intercept = layout@maxheight, slope = c(layout@heightratio, -layout@heightratio), linetype = "dotted") +
    coord_cartesian(xlim = xlim, ylim = ylim)
}

#' Construct monotonic splits from a sequence.
#' Given a sequence of `(x, y)` pairs, construct the version of that sequence that is sorted by x
#' and which contains no duplicates. Then, return a list of all (not necessarily contiguous) subsets
#' of the sequence in which y is monotonic.
#' @param x,y <[numeric]> `(x,y)` pairs giving evaluations of a function. Must all be >= 0.
#' @returns <[list] of [data.frame]s> Each data frame in the output list:
#' - has columns `"x"` and `"y"` (both [numeric])
#' - contains only `x,y` pairs that appear in the input
#' - is monotonic increasing in both `x` and `y`
#' @noRd
split_monotonic = function(x, y) {
  # reverse iter because we are doing most things from the back (specifically cummin())
  ord = order(x, decreasing = TRUE)
  iter_rev = data_frame0(x = x[ord], y = y[ord])
  iter_rev = iter_rev[!duplicated(iter_rev$x), ]

  splits = list()
  repeat {
    # construct a monotonic split from the end
    max_y = cummin(iter_rev$y)
    in_split = iter_rev$y == max_y
    split = iter_rev[rev(which(in_split)), ]
    splits = c(splits, list(split))
    if (all(in_split)) break

    # remove any points after the last point not in the split
    # that are less than the last point not in the split
    last_not_in_split = iter_rev[which.min(in_split), ]
    iter_rev = iter_rev[
      iter_rev$x <= last_not_in_split$x | iter_rev$y >= last_not_in_split$y,
    ]
  }
  splits
}

#' Piecewise stepped linear approximation
#' Approximates a function by piecewise linear approximation in a stepped manner. Instead of linear
#' approximation between neighboring points (in order of `x`), constructs points such that a
#' piecewise linear approximation built on those points is a function where `y_new = f(x_new)` is
#' determined by a linear interpolation between the nearest point `(x, y)` and `(0, 0)`.
#' This effectively creates a "step" halfway between two points `x_1` and `x_2`. For each halfway
#' point `x_mid`, we create a slope from `x_mid - eps_x` to `x_mid + eps_x` and do linear
#' interpolation in that region.
#' @param x,y <[numeric]> Evaluations of `y = f(x)` to use to approximate `f`. Must all be >= 0.
#' @param eps_x <scalar [numeric]> Epsilon for `x` values used to determine precision of the
#' approximation.
#' @returns <[data.frame]> with columns `x` and `y` giving a superset of the input `(x, y)` pairs
#' defining a piecewise stepped linear approximation.
#' @noRd
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

  data_frame0(
    x = vec_interleave(x_1, x_2, x_3, x_4),
    y = vec_interleave(y_1, y_2, y_3, y_4)
  )
}

#' Find all x values where a piecewise stepped linear approximation intersects `target_y`.
#' Uses a stepped linear approximation (see `stepped_linear_approx()`) of `y = f(x)` to find
#' possible `x` values where `f(x) = target_y`.
#' @param x,y <[numeric]> Evaluations of `y = f(x)` to use to approximate `f`. Must all be >= 0.
#' @param target_y <scalar [numeric]> `y` value to attempt to find `x` values for.
#' @param eps_x <scalar [numeric]> Epsilon for `x` values used to determine precision of the
#' approximation.
#' @returns <[numeric]> with length `>= 0` giving `x` values such that `f(x) = target_y` using
#' a piecewise stepped linear approximation.
#' @noRd
stepped_linear_approx_at_y = function(x, y, target_y, eps_x = .Machine$double.eps^0.25) {
  approx = stepped_linear_approx(x, y, eps_x = eps_x)
  approx$y = approx$y - target_y

  n = nrow(approx)
  x_1 = approx$x[-n]
  y_1 = approx$y[-n]
  x_2 = approx$x[-1]
  y_2 = approx$y[-1]

  crosses_zero = which(sign(y_1) != sign(y_2))
  with(data.frame(x_1, y_1, x_2, y_2)[crosses_zero, ],
    (x_1 * y_2 - x_2 * y_1) / (y_2 - y_1)
  )
}
