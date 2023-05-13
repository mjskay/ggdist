# Weighted histogram
#
# Author: mjskay
###############################################################################


#' @importFrom rlang as_label enexpr get_expr
weighted_hist = function(
  x, weights = NULL, breaks = "Sturges", align = "none"
) {
  x_label = as_label(enexpr(x))
  weights_label = as_label(enexpr(weights))
  label = if (is.null(weights)) {
    x_label
  } else {
    paste0("[", x_label, ", ", weights_label, "]")
  }

  if (length(x) < 1) cli_abort("{.fun ggdist::density_histogram} requires {.code length(x) >= 1}.")

  weights = weights %||% rep(1, length(x))

  # figure out breaks
  if (is.character(breaks)) {
    breaks = match_function(breaks, prefix = "breaks_")
  }
  if (is.function(breaks)) {
    breaks = breaks(x, weights = weights)
  }
  if (length(breaks) == 1) {
    if (length(x) == 1) {
      breaks = c(x - 0.5, x + 0.5)
    } else {
      breaks = seq.int(min(x), max(x), length.out = breaks)
    }
    bin_width = diff(breaks)
    equidist = TRUE
  } else {
    breaks = sort(unique(breaks))
    bin_width = diff(breaks)
    equidist = diff(range(bin_width)) < 1e-7 * mean(bin_width)
  }

  # apply alignment if bins are equidistant
  if (equidist) {
    if (is.character(align)) {
      align = match_function(align, prefix = "align_")
    }
    if (is.function(align)) {
      align = align(breaks)
    }
    if (align < 0 || align > bin_width[[1]]) {
      cli_abort(c(
        "{.arg align} must be between 0 and the bin width",
        "i" = "See the {.arg align} argument to {.fun ggdist::density_histogram}."
      ))
    }

    # we check for align != 0 even though in theory we could apply a 0 alignment
    # below and the result would be correct. We do this because then if someone
    # manually specifies the breaks and no alignment, exactly those breaks are used.
    if (align != 0) {
      breaks = breaks - align
      max_break = breaks[length(breaks)]

      if (max_break < max(x)) {
        breaks = c(breaks, max_break + bin_width[[1]])
        bin_width = c(bin_width, bin_width[[1]])
      }
      if (length(breaks) > 2 && breaks[[2]] <= min(x)) {
        breaks = breaks[-1]
        bin_width = bin_width[-1]
      }
    }
  }

  # check for invalid binning
  if (min(x) < breaks[1] || max(x) > breaks[length(breaks)]) {
    cli_abort("The {.arg breaks} argument to {.fun ggdist::density_histogram} must cover all values of {.arg x}")
  }

  # bin x values
  bin = findInterval(x, breaks, rightmost.closed = TRUE, left.open = TRUE)

  # sum up weights in each bin
  counts = rep(0, length(breaks) - 1)
  used_bins = unique(bin)
  counts[used_bins] = tapply(weights, factor(bin, used_bins), sum)

  structure(
    list(
      breaks = breaks,
      counts = counts,
      density = counts / bin_width / sum(weights),
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
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @param x A numeric vector giving a sample.
#' @param weights A numeric vector of `length(x)` giving sample weights.
#' @param width For [breaks_fixed()], the desired bin width.
#' @param digits Number of significant digits to keep when rounding in the Freedman-Diaconis
#'   algorithm ([breaks_FD()]). For an explanation of this parameter, see the documentation
#'   of the corresponding parameter in [grDevices::nclass.FD()].
#'
#' @details
#' These functions take a sample and its weights and return a valuable suitable for
#' the `breaks` argument to [density_histogram()] that will determine the histogram
#' breaks.
#'
#'  - [breaks_fixed()] allows you to manually specify a fixed bin width.
#'  - [breaks_Sturges()], [breaks_Scott()], and [breaks_FD()] implement weighted
#'    versions of the corresponding base functions. See [nclass.Sturges()],
#'    [nclass.scott()], and [nclass.FD()].
#' @returns Either a single number (giving the number of bins) or a vector
#' giving the edges between bins.
#' @seealso [density_histogram()], [align]
#' @examples
#' library(ggplot2)
#'
#' set.seed(1234)
#' x = rnorm(200, 1, 2)
#'
#' # Let's compare the different break-selection algorithms on this data:
#' ggplot(data.frame(x), aes(x)) +
#'   stat_slab(
#'     aes(y = "fixed at 0.5"),
#'     density = "histogram",
#'     breaks = breaks_fixed(width = 0.5),
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   stat_slab(
#'     aes(y = "Sturges"),
#'     density = "histogram",
#'     breaks = "Sturges",
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   stat_slab(
#'     aes(y = "Scott"),
#'     density = "histogram",
#'     breaks = "Scott",
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   stat_slab(
#'     aes(y = "FD"),
#'     density = "histogram",
#'     breaks = "FD",
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   geom_point(aes(y = 0.7), alpha = 0.5)
#' @name breaks
#' @export
breaks_fixed = function(x, weights = NULL, width = 1) {
  if (missing(x)) return(partial_self("breaks_fixed"))

  if (length(x) == 1) return(c(x - width/2, x + width/2))

  # determine amount we need to expand range by to make it a multiple of width
  x_range = range(x)
  expand = ((-diff(x_range)) %% width) / 2

  seq.int(x_range[[1]] - expand, x_range[[2]] + expand, by = width)
}

#' @rdname breaks
#' @export
breaks_Sturges = function(x, weights = NULL) {
  if (missing(x)) return(partial_self("breaks_Sturges"))

  weights = weights %||% rep(1, length(x))
  n = max(length(x), sum(weights))
  ceiling(log2(n) + 1)
}

#' @rdname breaks
#' @export
breaks_Scott = function(x, weights = NULL) {
  if (missing(x)) return(partial_self("breaks_Scott"))

  weights = weights %||% rep(1, length(x))
  n = max(length(x), sum(weights))
  h = 3.5 * sqrt(weighted_var(x, weights)) * n^(-1/3)
  if (h > 0) {
    max(1, ceiling(diff(range(x))/h))
  } else {
    1L
  }
}

#' @rdname breaks
#' @export
breaks_FD = function(x, weights = NULL, digits = 5) {
  if (missing(x)) return(partial_self("breaks_FD"))

  weights = weights %||% rep(1, length(x))
  h = 2 * weighted_iqr(.x <- signif(x, digits = digits), weights)

  if (h == 0) {
    .x_order = order(.x)
    .x = .x[.x_order]
    .weights = weights[.x_order]
    al = 1/4
    al_min = 1/512

    quantile_fun = weighted_quantile_fun(.x, weights = .weights, n = "sum")
    while (h == 0 && (al <- al/2) >= al_min) {
      h = diff(quantile_fun(c(al, 1 - al))) / (1 - 2 * al)
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
}


# alignment algorithms ----------------------------------------------------

#' Break (bin) alignment methods
#'
#' Methods for aligning breaks (bins) in histograms, as used in the `align`
#' argument to [density_histogram()].
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @param breaks A sorted vector of breaks (bin edges).
#' @param at A scalar numeric giving an alignment point.
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
#'     aes(y = "none"),
#'     density = "histogram",
#'     breaks = breaks_fixed(width = 1),
#'     outline_bars = TRUE,
#'     # no need to specify align; align_none() is the default
#'     color = "black",
#'   ) +
#'   stat_slab(
#'     aes(y = "center at 0"),
#'     density = "histogram",
#'     breaks = breaks_fixed(width = 1),
#'     align = align_center(at = 0),   # or align = "center"
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   stat_slab(
#'     aes(y = "boundary at 0"),
#'     density = "histogram",
#'     breaks = breaks_fixed(width = 1),
#'     align = align_boundary(at = 0), # or align = "boundary"
#'     outline_bars = TRUE,
#'     color = "black",
#'   ) +
#'   geom_point(aes(y = 0.7), alpha = 0.5)
#' @name align
#' @export
align_none = function(breaks) {
  if (missing(breaks)) return(partial_self("align_none"))

  0
}

#' @rdname align
#' @export
align_boundary = function(breaks, at = 0) {
  if (missing(breaks)) return(partial_self("align_boundary"))

  (breaks[[1]] - at) %% diff(breaks[1:2])
}

#' @rdname align
#' @export
align_center = function(breaks, at = 0) {
  if (missing(breaks)) return(partial_self("align_center"))

  w = diff(breaks[1:2])
  (breaks[[1]] - at + w/2) %% w
}


# helpers -----------------------------------------------------------------

#' @importFrom stats weighted.mean
weighted_var = function(x, weights) {
  sum(weights * (x - weighted.mean(x, weights))^2) / sum(weights)
}

weighted_iqr = function(x, weights) {
  diff(weighted_quantile(as.numeric(x), c(0.25, 0.75), weights = weights, n = "sum"))
}
