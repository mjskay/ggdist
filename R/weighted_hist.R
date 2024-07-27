# Weighted histogram
#
# Author: mjskay
###############################################################################


#' @importFrom rlang as_label enexpr get_expr
weighted_hist = function(
  x, weights = NULL, breaks = "Scott", align = "none",
  right_closed = TRUE, outermost_closed = TRUE
) {
  x_label = as_label(enexpr(x))
  weights_label = as_label(enexpr(weights))
  label = if (is.null(weights)) {
    x_label
  } else {
    paste0("[", x_label, ", ", weights_label, "]")
  }
  if (length(x) < 1) cli_abort("{.fun ggdist::density_histogram} requires {.code length(x) >= 1}.")

  # figure out breaks
  binwidths = equidist = NULL
  c(breaks, binwidths, equidist) %<-% get_breaks(x, weights, breaks)
  # only apply bin alignment if bins are equidistant
  if (equidist) c(breaks, binwidths) %<-% align_breaks(x, breaks, binwidths, align)

  # bin x values
  bin = findInterval(x, breaks, rightmost.closed = outermost_closed, left.open = right_closed)

  # check for invalid binning
  if (min(bin) < 1 || max(bin) >= length(breaks)) {
    cli_abort(
      "The {.arg breaks} argument to {.fun ggdist::density_histogram} must cover all values of {.arg x}",
      class = "ggdist_invalid_breaks"
    )
  }

  # sum up weights in each bin
  weights = weights %||% rep(1, length(x))
  counts = rep(0, length(breaks) - 1)
  used_bins = unique(bin)
  counts[used_bins] = tapply(weights, factor(bin, used_bins), sum)

  structure(
    list(
      breaks = breaks,
      counts = counts,
      density = counts / binwidths / sum(weights),
      mids = (breaks[-length(breaks)] + breaks[-1])/2,
      xname = label,
      equidist = equidist
    ),
    class = "histogram"
  )
}


# breaks algorithms -------------------------------------------------------

#' Break (bin) selection algorithms for histograms
#'
#' Methods for determining breaks (bins) in histograms, as used in the `breaks`
#' argument to [density_histogram()].
#' @template description-auto-partial-waivable
#'
#' @param x <[numeric]> Sample values.
#' @param weights <[numeric] | [NULL]> Optional weights to apply to `x`, which
#' will be normalized to sum to 1.
#' @details
#' These functions take a sample and its weights and return a value suitable for
#' the `breaks` argument to [density_histogram()] that will determine the histogram
#' breaks.
#'
#'  - [breaks_fixed()] allows you to manually specify a fixed bin width.
#'  - [breaks_Sturges()], [breaks_Scott()], and [breaks_FD()] implement weighted
#'    versions of their corresponding base functions. They return a scalar
#'    numeric giving the number of bins. See [nclass.Sturges()], [nclass.scott()],
#'    and [nclass.FD()].
#'  - [breaks_quantiles()] constructs irregularly-sized bins using `max_n + 1`
#'    (possibly weighted) quantiles of `x`. The final number of bins is
#'    *at most* `max_n`, as small bins (ones whose bin width is less than half
#'    the range of the data divided by `max_n` times `min_width`) will be merged
#'    into adjacent bins.
#' @returns Either a single number (giving the number of bins) or a vector
#' giving the edges between bins.
#' @seealso [density_histogram()], [align]
#' @examples
#' library(ggplot2)
#'
#' set.seed(1234)
#' x = rnorm(2000, 1, 2)
#'
#' # Let's compare the different break-selection algorithms on this data:
#' ggplot(data.frame(x), aes(x)) +
#'   stat_slab(
#'     aes(y = "breaks_fixed(width = 0.5)"),
#'     density = "histogram",
#'     breaks = breaks_fixed(width = 0.5),
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   stat_slab(
#'     aes(y = "breaks_Sturges()\nor 'Sturges'"),
#'     density = "histogram",
#'     breaks = "Sturges",
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   stat_slab(
#'     aes(y = "breaks_Scott()\nor 'Scott'"),
#'     density = "histogram",
#'     breaks = "Scott",
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   stat_slab(
#'     aes(y = "breaks_FD()\nor 'FD'"),
#'     density = "histogram",
#'     breaks = "FD",
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   stat_slab(
#'     aes(y = "breaks_quantiles()\nor 'quantiles'"),
#'     density = "histogram",
#'     breaks = "quantiles",
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   geom_point(aes(y = 0.7), alpha = 0.5) +
#'   labs(
#'     subtitle = "ggdist::stat_slab(density = 'histogram', ...)",
#'     y = "breaks =",
#'     x = NULL
#'   )
#' @name breaks
NULL

## breaks_fixed ---------------------------------------------------------------
#' @rdname breaks
#' @param width <scalar [numeric]> For [breaks_fixed()], the desired bin width.
#' @export
breaks_fixed = auto_partial(name = "breaks_fixed", function(
  x, weights = NULL, width = 1
) {
  if (length(x) == 1) return(c(x - width/2, x + width/2))

  # determine amount we need to expand range by to make it a multiple of width
  x_range = range(x)
  expand = ((-diff(x_range)) %% width) / 2

  seq.int(x_range[[1]] - expand, x_range[[2]] + expand, by = width)
})

## breaks_Sturges ---------------------------------------------------------------
#' @rdname breaks
#' @export
breaks_Sturges = auto_partial(name = "breaks_Sturges", function(
  x, weights = NULL
) {
  weights = weights %||% rep(1, length(x))
  n = max(length(x), sum(weights))
  ceiling(log2(n) + 1)
})

## breaks_Scott ---------------------------------------------------------------
#' @rdname breaks
#' @export
breaks_Scott = auto_partial(name = "breaks_Scott", function(
  x, weights = NULL
) {
  weights = weights %||% rep(1, length(x))
  n = max(length(x), sum(weights))
  h = 3.5 * sqrt(weighted_var(x, weights)) * n^(-1/3)
  if (h > 0) {
    max(1, ceiling(diff(range(x))/h))
  } else {
    1L
  }
})

## breaks_FD ---------------------------------------------------------------
#' @rdname breaks
#' @param digits <scalar [numeric]> For [breaks_FD()], the number of significant digits to keep when
#'   rounding in the Freedman-Diaconis algorithm. For an explanation of this
#'   parameter, see the documentation of the corresponding parameter in
#'   [grDevices::nclass.FD()].
#' @export
breaks_FD = auto_partial(name = "breaks_FD", function(
  x, weights = NULL, digits = 5
) {
  .x = signif(x, digits = digits)
  weights = weights %||% rep(1, length(x))
  h = 2 * weighted_iqr(.x, weights)

  if (h == 0) {
    .x_order = order(.x)
    .x = .x[.x_order]
    .weights = weights[.x_order]

    quantile_fun = weighted_quantile_fun(.x, weights = .weights, n = "sum")
    for (al in 2^(-3:-9)) {
      h = diff(quantile_fun(c(al, 1 - al))) / (1 - 2 * al)
      if (h != 0) break
    }
  }

  if (h == 0) {
    h = 3.5 * sqrt(weighted_var(x, weights))
  }

  n = max(length(x), sum(weights))
  if (h > 0) {
    ceiling(diff(range(x))/h * n^(1/3))
  } else {
    1L
  }
})

## breaks_quantiles --------------------------------------------------------
#' @rdname breaks
#' @param max_n <scalar [numeric] | [function] | [string][character]>
#' For [breaks_quantiles()], either a scalar numeric giving the
#' maximum number of bins, or another breaks function (or string giving the
#' suffix of the name of a function prefixed with `"breaks_"`) that will
#' return the maximum number of bins. [breaks_quantiles()] will construct
#' *at most* `max_n` bins.
#' @param min_width <scalar [numeric]> For [breaks_quantiles()], a numeric
#' between `0` and `1` giving the minimum bin width as a proportion of
#' `diff(range(x)) / max_n`.
#' @export
breaks_quantiles = auto_partial(name = "breaks_quantiles", function(
  x, weights = NULL, max_n = "Scott", min_width = 0.5
) {
  max_n = get_raw_breaks(x, weights, max_n)
  stopifnot(is.numeric(max_n), length(max_n) == 1, max_n >= 1)

  breaks = weighted_quantile(x, ppoints(max_n + 1, a = 1), weights = weights, names = FALSE)

  # remove bins that are very small (less than half the bin width of the
  # bins that would be used in a regular bin spacing with `n` bins).
  min_binwidth = diff(range(x)) / max_n * min_width
  next_break = -Inf
  for (i in seq_len(max_n)) {
    if (breaks[[i]] >= next_break) {
      current_break = breaks[[i]]
      next_break = current_break + min_binwidth
    } else {
      breaks[[i]] = current_break
    }
  }
  breaks = vec_unrep(breaks)$key

  if (length(breaks) == 1) breaks = 1L
  breaks
})


# alignment algorithms ----------------------------------------------------

#' Break (bin) alignment methods
#'
#' Methods for aligning breaks (bins) in histograms, as used in the `align`
#' argument to [density_histogram()].
#' @template description-auto-partial-waivable
#'
#' @param breaks <[numeric]> A sorted vector of breaks (bin edges).
#' @param at <scalar [numeric]> The alignment point.
#'  - For [align_boundary()]: align breaks so that a bin edge lines up with `at`.
#'  - For [align_center()]: align breaks so that the center of a bin lines up with `at`.
#'
#' @details
#' These functions take a sorted vector of equally-spaced `breaks` giving
#' bin edges and return a numeric offset which, if subtracted from `breaks`,
#' will align them as desired:
#'
#'  - [align_none()] performs no alignment (it always returns `0`).
#'  - [align_boundary()] ensures that a bin edge lines up with `at`.
#'  - [align_center()] ensures that a bin center lines up with `at.`
#'
#' For [align_boundary()] (respectively [align_center()]), if no bin edge (or center) in the
#' range of `breaks` would line up with `at`, it ensures that `at` is an integer
#' multiple of the bin width away from a bin edge (or center).
#'
#' @returns A scalar numeric returning an offset to be subtracted from `breaks`.
#' @seealso [density_histogram()], [breaks]
#' @examples
#' library(ggplot2)
#'
#' set.seed(1234)
#' x = rnorm(200, 1, 2)
#'
#' # If we manually specify a bin width using breaks_fixed(), the default
#' # alignment (align_none()) will not align bin edges to any "pretty" numbers.
#' # Here is a comparison of the three alignment methods on such a histogram:
#' ggplot(data.frame(x), aes(x)) +
#'   stat_slab(
#'     aes(y = "align_none()\nor 'none'"),
#'     density = "histogram",
#'     breaks = breaks_fixed(width = 1),
#'     outline_bars = TRUE,
#'     # no need to specify align; align_none() is the default
#'     color = "black",
#'   ) +
#'   stat_slab(
#'     aes(y = "align_center(at = 0)\nor 'center'"),
#'     density = "histogram",
#'     breaks = breaks_fixed(width = 1),
#'     align = align_center(at = 0),   # or align = "center"
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   stat_slab(
#'     aes(y = "align_boundary(at = 0)\nor 'boundary'"),
#'     density = "histogram",
#'     breaks = breaks_fixed(width = 1),
#'     align = align_boundary(at = 0), # or align = "boundary"
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   geom_point(aes(y = 0.7), alpha = 0.5) +
#'   labs(
#'     subtitle = "ggdist::stat_slab(density = 'histogram', ...)",
#'     y = "align =",
#'     x = NULL
#'   ) +
#'   geom_vline(xintercept = 0, linetype = "22", color = "red")
#' @name align
#' @export
align_none = auto_partial(name = "align_none", function(breaks) {
  0
})

## align_boundary ----------------------------------------------------------
#' @rdname align
#' @export
align_boundary = auto_partial(name = "align_boundary", function(breaks, at = 0) {
  (breaks[[1]] - at) %% diff(breaks[1:2])
})

## align_center ------------------------------------------------------------
#' @rdname align
#' @export
align_center = auto_partial(name = "align_center", function(breaks, at = 0) {
  w = diff(breaks[1:2])
  (breaks[[1]] - at + w/2) %% w
})


# helpers -----------------------------------------------------------------

#' Given a dataset, weights, and breaks as passed to weighted_hist, return
#' a named list of breaks and whether or not the breaks are equidistant
#' @param x data
#' @param weights weights. vector same length as `x`, or `NULL`.
#' @param breaks breaks as passed to weighted_hist (e.g. function or name)
#' @returns list with these elements:
#'   - `breaks`: vector of breakpoints covering `x`
#'   - `binwidths`: vector of bin widths of length `length(breaks) - 1`
#'   - `equidist`: logical: are the breaks equidistant from each other?
#' @noRd
get_breaks = function(x, weights, breaks) {
  breaks = get_raw_breaks(x, weights, breaks)
  if (length(breaks) == 1) {
    unique_x = unique(x)
    if (length(unique_x) == 1) {
      breaks = c(unique_x - 0.5, unique_x + 0.5)
    } else {
      breaks = seq.int(min(x), max(x), length.out = max(breaks + 1, 2))
    }
    binwidths = diff(breaks)
    equidist = TRUE
  } else {
    breaks = sort(unique(breaks))
    binwidths = diff(breaks)
    equidist = diff(range(binwidths)) < 1e-7 * mean(binwidths)
  }

  list(breaks = breaks, binwidths = binwidths, equidist = equidist)
}

get_raw_breaks = function(x, weights, breaks) {
  if (is.character(breaks)) {
    breaks = match_function(breaks, prefix = "breaks_")
  }
  if (is.function(breaks)) {
    # don't pass NULL weights to breaks function for compatibility with breaks
    # functions from other packages that don't support weights; e.g. {scales}
    if (is.null(weights)) {
      breaks = breaks(x)
    } else {
      breaks = breaks(x, weights = weights)
    }
  }
  breaks
}

#' Given a dataset, breaks / binwidths, and an alignment function, returned
#' the modified breaks / binwidths
#' @param x data
#' @param breaks vector of breakpoints
#' @param binwidths widths of bins; i.e. `diff(breaks)`
#' @param align alignment function as passed to `weighted_hist` (e.g. function or name)
#' @returns list with modified breakpoints:
#'   - `breaks`: vector of breakpoints covering `x`
#'   - `binwidths`: vector of bin widths of length `length(breaks) - 1`
#' @noRd
align_breaks = function(x, breaks, binwidths, align, call = caller_env()) {
  if (is.character(align)) {
    align = match_function(align, prefix = "align_")
  }
  if (is.function(align)) {
    align = align(breaks)
  }
  if (align < 0 || align > binwidths[[1]]) {
    cli_abort(
      c(
        "{.arg align} must be between 0 and the bin width",
        "i" = "See the {.arg align} argument to {.fun ggdist::density_histogram}."
      ),
      call = call
    )
  }

  # we check for align != 0 even though in theory we could apply a 0 alignment
  # below and the result would be correct. We do this because then if someone
  # manually specifies the breaks and no alignment, exactly those breaks are used.
  if (align != 0) {
    breaks = breaks - align
    max_break = breaks[length(breaks)]

    if (max_break < max(x)) {
      breaks = c(breaks, max_break + binwidths[[1]])
      binwidths = c(binwidths, binwidths[[1]])
    }
    if (length(breaks) > 2 && breaks[[2]] <= min(x)) {
      breaks = breaks[-1]
      binwidths = binwidths[-1]
    }
  }

  list(breaks = breaks, binwidths = binwidths)
}


#' @importFrom stats weighted.mean
weighted_var = function(x, weights) {
  sum(weights * (x - weighted.mean(x, weights))^2) / sum(weights)
}

weighted_iqr = function(x, weights) {
  diff(weighted_quantile(as.numeric(x), c(0.25, 0.75), weights = weights, n = "sum", names = FALSE))
}
