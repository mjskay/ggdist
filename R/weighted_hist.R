# Weighted histogram
#
# Author: mjskay
###############################################################################


#' @importFrom rlang as_label enexpr get_expr
weighted_hist = function(x, weights = NULL, breaks = "Sturges") {
  x_label = as_label(enexpr(x))
  weights_label = as_label(enexpr(weights))
  label = if (is.null(weights)) {
    x_label
  } else {
    paste0("[", x_label, ", ", weights_label, "]")
  }

  weights = weights %||% rep(1, length(x))

  # figure out breaks
  if (is.character(breaks)) {
    breaks = match_function(breaks, prefix = "breaks_")
  }
  if (is.function(breaks)) {
    breaks = breaks(x, weights)
  }
  if (length(breaks) == 1) {
    breaks = pretty(x, breaks, min.n = 1)
    bin_width = diff(breaks)
    equidist = TRUE
  } else {
    breaks = sort(unique(breaks))
    bin_width = diff(breaks)
    equidist = diff(range(bin_width)) < 1e-7 * mean(bin_width)
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

breaks_Sturges = function(x, weights) {
  n = max(length(x), sum(weights))
  ceiling(log2(n) + 1)
}

breaks_Scott = function(x, weights) {
  n = max(length(x), sum(weights))
  h = 3.5 * sqrt(weighted_var(x, weights)) * n^(-1/3)
  if (h > 0) {
    max(1, ceiling(diff(range(x))/h))
  } else {
    1L
  }
}

breaks_FD = function(x, weights, digits = 5) {
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

#' @importFrom stats weighted.mean
weighted_var = function(x, weights) {
  sum(weights * (x - weighted.mean(x, weights))^2) / sum(weights)
}

weighted_iqr = function(x, weights) {
  diff(weighted_quantile(as.numeric(x), c(0.25, 0.75), weights = weights, n = "sum"))
}
