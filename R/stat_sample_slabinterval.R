# Distribution + interval stat for samples
#
# Author: mjskay
###############################################################################


# slab function for samples -------------------------

#' @importFrom stats approxfun
weighted_ecdf = function(x, weights = NULL) {
  n = length(x)
  if (n < 1) stop("Need at least 1 or more values to calculate an ECDF")

  #sort x
  sort_order = order(x)
  x = x[sort_order]

  # calculate weighted cumulative probabilities
  weights = if (is.null(weights)) rep(1, n) else weights
  weights = weights[sort_order]
  p = cumsum(weights) / sum(weights)

  # need to manually do tie removal before passing to approxfun, otherwise it
  # will fail when all x values are equal
  unique_x = unique(x)
  if (length(unique_x) < length(x)) {
    # if x[i] ... x[i + k] are all equal ("tied"), collapse to a single x
    # value and let corresponding value in p = max(p[i] ... p[i + k])
    p = as.vector(tapply(p, match(x, x), max))
    x = unique_x
    stopifnot(length(p) == length(x))
  }

  method = "constant"

  approxfun(x, p, yleft = 0, yright = 1, ties = "ordered", method = method)
}

sample_density = function(x, ...) {
  if (length(unique(x)) == 1) {
    list(x = x[[1]], y = Inf)
  } else {
    density(x, ...)
  }
}

compute_limits_sample = function(
  self, data, trans, orientation,
  p_limits, trim, adjust,
  ...
) {
  x = switch(orientation,
    y = ,
    horizontal = "x",
    x = ,
    vertical = "y"
  )

  # when trim is FALSE, limits of data will be expanded by 3 * the bandwidth
  expansion = if (trim) {
    0
  } else {
    bw = stats::bw.nrd0(data[[x]])
    bw * adjust * 3
  }

  data.frame(
    .lower = trans$inverse(min(data[[x]]) - expansion),
    .upper = trans$inverse(max(data[[x]]) + expansion)
  )
}

#' @importFrom rlang missing_arg
#' @importFrom stats ecdf density
#' @importFrom graphics hist
compute_slab_sample = function(
  self, data, trans, input, orientation,
  slab_type = "pdf", limits = NULL, n = 501,
  adjust = 1, trim = TRUE, expand = FALSE, breaks = "Sturges", outline_bars = FALSE,
  ...
) {
  x = switch(orientation,
    y = ,
    horizontal = "x",
    x = ,
    vertical = "y"
  )

  # calculate the density first, since we'll use the x values from it
  # to calculate the cdf
  slab_df = if (slab_type == "histogram") {
    # when using a histogram slab, that becomes the density function value
    # TODO: this is a hack for now until we make it so that density estimators
    # can be swapped out (which would be a better solution)
    h = hist(data[[x]], breaks = breaks, plot = FALSE)
    input_1 = h$breaks[-length(h$breaks)]  # first edge of bin
    input_2 = h$breaks[-1]                 # second edge of bin

    if (!outline_bars) {
      # as.vector(rbind(x, y)) interleaves vectors input_1 and input_2, giving
      # us the bin endpoints --- then just need to repeat the same value of density
      # for both endpoints of the same bin
      .input = trans$inverse(as.vector(rbind(input_1, input_2)))
      .value = rep(h$density, each = 2)
    } else {
      # have to return to 0 in between each bar so that bar outlines are drawn
      .input = trans$inverse(as.vector(rbind(input_1, input_1, input_2, input_2)))
      .value = as.vector(rbind(0, h$density, h$density, 0))
    }
    data.frame(
      .input = .input,
      pdf = .value
    )
  } else {
    # all other slab types use the density function as the pdf
    cut = if (trim) 0 else 3
    # calculate on the transformed scale to ensure density is correct
    d = sample_density(data[[x]], n = n, adjust = adjust, cut = cut)
    data.frame(
      .input = trans$inverse(d$x),
      pdf = d$y
    )
  }

  # calculate cdf
  trans_input = trans$transform(slab_df$.input)
  cdf_fun = weighted_ecdf(data[[x]])
  slab_df[["cdf"]] = cdf_fun(trans_input)

  if (expand) {
    # extend x values to the range of the plot. To do that we have to include
    # x values requested from the original `input` if they are outside the
    # range of the slab
    input_below_slab = input[input < min(slab_df$.input)]
    if (length(input_below_slab) > 0) {
      slab_df = rbind(data.frame(
        .input = input_below_slab,
        pdf = 0,
        cdf = 0
      ), slab_df)
    }

    input_above_slab = input[input > max(slab_df$.input)]
    if (length(input_above_slab) > 0) {
      slab_df = rbind(slab_df, data.frame(
        .input = input_above_slab,
        pdf = 0,
        cdf = 1
      ))
    }
  }

  slab_df[[".value"]] = switch(slab_type,
    histogram = ,
    pdf = slab_df$pdf,
    cdf = slab_df$cdf,
    ccdf = 1 - slab_df$cdf,
    stop0("Unknown `slab_type`: ", deparse0(slab_type), '. Must be "histogram", "pdf", "cdf", or "ccdf"')
  )

  slab_df$n = nrow(data)
  slab_df
}
