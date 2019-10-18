# Distribution + interval stat for analytical distributions
#
# Author: mjskay
###############################################################################


# limits, slab, and interval functions for distributions -------------------------

# translate arguments of the form `arg1` ... `arg9` (or from a list column, args) into a single list of arguments
args_from_aes = function(args = list(), ...) {
  dot_args = list(...)
  args_from_dots = list()
  for (i in 1:9) {
    arg_name = paste0("arg", i)
    if (arg_name %in% names(dot_args)) {
      args_from_dots[[i]] = dot_args[[arg_name]]
    }
  }

  c(args_from_dots, args)
}

#' @importFrom purrr pmap_dfr
dist_limits_function = function(df, p_limits = c(.001, .999), ...) {
  pmap_dfr(df, function(dist, ...) {
    if (is.na(dist)) {
      return(data.frame(.lower = NA, .upper = NA))
    }

    args = args_from_aes(...)
    quantile_fun = match.fun(paste0("q", dist))
    limits = do.call(quantile_fun, c(list(quote(p_limits)), args))

    data.frame(
      .lower = limits[[1]],
      .upper = limits[[2]]
    )
  })
}

# return a version of the provided density function f_X(...)
# transformed according to transformation trans
#' @importFrom stats numericDeriv
transform_pdf = function(f_X, y, trans, g_inverse_at_y = trans$inverse(y), ...) {
  # based on the fact that for Y = g(X),
  # f_Y(y) = f_X(g^−1(y)) * | g^-1'(y) |

  g_inverse = trans$inverse

  # need to convert y to numeric in case it's an integer (numericDeriv doesn't like ints)
  y = as.numeric(y)
  g_inverse_deriv_at_y = diag(attr(numericDeriv(quote(g_inverse(y)), "y"), "gradient"))

  f_X(g_inverse_at_y, ...) * abs(g_inverse_deriv_at_y)
}

#' @importFrom purrr pmap_dfr
dist_slab_function = function(
  df, input, slab_type = "pdf", limits = NULL, n = 501, trans = scales::identity_trans(), ...
) {
  pmap_dfr(df, function(dist, ...) {
    if (is.na(dist)) {
      return(data.frame(.input = NA, .value = NA))
    }

    args = args_from_aes(...)
    dist_fun = switch(slab_type,
      pdf = {
        pdf = match.fun(paste0("d", dist))
        if (trans$name == "identity") {
          pdf
        } else {
          # must transform the density according to the scale transformation
          function(x, ...) transform_pdf(pdf, trans$transform(x), trans, g_inverse_at_y = x, ...)
        }
      },
      cdf = match.fun(paste0("p", dist)),
      ccdf = {
        cdf = match.fun(paste0("p", dist));
        function (...) 1 - cdf(...)
      }
    )
    quantile_fun = match.fun(paste0("q", dist))

    data.frame(
      .input = input,
      .value = do.call(dist_fun, c(list(quote(input)), args))
    )
  })
}

#' @importFrom purrr pmap_dfr
dist_interval_function = function(df, .width, trans, ...) {
  pmap_dfr(df, function(dist, ...) {
    if (is.na(dist)) {
      return(data.frame(.value = NA, .lower = NA, .upper = NA, .width = .width))
    }

    args = args_from_aes(...)
    quantile_fun = match.fun(paste0("q", dist))

    intervals = map_dfr(.width, function(w) {
      quantile_args = c(list(c(0.5, (1 - w)/2, (1 + w)/2)), args)
      quantiles = do.call(quantile_fun, quantile_args)
      data.frame(
        .value = trans$transform(quantiles[[1]]),
        .lower = trans$transform(quantiles[[2]]),
        .upper = trans$transform(quantiles[[3]]),
        .width = w
      )
    })
  })
}


# stat_dist_slabinterval --------------------------------------------------

#' Distribution + interval plots (eye plots, half-eye plots, CCDF barplots, etc) for analytical distributions (ggplot stat)
#'
#' Stats for computing distribution functions (densities or CDFs) + intervals for use with
#' [geom_slabinterval()]. Uses `dist` aesthetic to specify a distribution name
#' and `arg1`, ... `arg9` aesthetics (or `args` as a list column) to specify distribution
#' arguments.
#'
#' A highly configurable stat for generating a variety of plots that combine a "slab"
#' that describes a distribution plus an interval. Several "shortcut" stats are provided
#' which combine multiple options to create useful geoms, particularly *eye plots*
#' (a combination of a violin plot and interval), *half-eye plots* (a density plus interval),
#' and *CCDF bar plots* (a complementary CDF plus interval).
#'
#' The shortcut stat names follow the pattern `stat_dist_[name][h|]`, where the trailing
#' `h` (if present) indicates the horizontal version of the stat.
#'
#' Stats include:
#'
#' \itemize{
#'   \item `stat_dist_eye` / `stat_dist_eyeh`: Eye plots (violin + interval)
#'   \item `stat_dist_halfeye` / `stat_dist_halfeyeh`: Half-eye plots (density + interval)
#'   \item `stat_dist_ccdfinterval` / `stat_dist_ccdfintervalh`: CCDF bar plots (CCDF + interval)
#'   \item `stat_dist_cdfinterval` / `stat_dist_cdfintervalh`: CDF bar plots (CDF + interval)
#'   \item `stat_dist_gradientinterval` / `stat_dist_gradientintervalh`: Density gradient + interval plots
#'   \item `stat_dist_pointinterval` / `stat_dist_pointintervalh`: Point + interval plots
#'   \item `stat_dist_interval` / `stat_dist_intervalh`: Interval plots
#' }
#'
#' These stats expect a `dist` aesthetic to specify a distribution name
#' and `arg1`, ... `arg9` aesthetics (or `args` as a list column) to specify distribution
#' arguments. Distribution names should correspond to R functions that have `"p"`, `"q"`, and
#' `"d"` functions; e.g. `"norm"` is a valid distribution name because R defines the
#' [pnorm()], [qnorm()], and [dnorm()] functions for Normal distributions.
#'
#' See the [parse_dist()] function for a useful way to generate `dist` and `args`
#' values from human-readable distribution specs (like `"normal(0,1)"`). Such specs are also
#' produced by other packages (like the [brms::get_prior()] function in brms); thus,
#' [parse_dist()] combined with the stats described here can help you visualize the output
#' of those functions.
#'
#' @eval rd_slabinterval_aesthetics(stat = StatDistSlabinterval)
#' @section Computed Variables:
#' \itemize{
#'   \item `x` or `y`: For slabs, the input values to the slab function.
#'     For intervals, the point summary from the interval function. Whether it is `x` or `y` depends on `orientation`
#'   \item `xmin` or `ymin`: For intervals, the lower end of the interval from the interval function.
#'   \item `xmax` or `ymax`: For intervals, the upper end of the interval from the interval function.
#'   \item `f`: For slabs, the output values from the slab function.
#' }
#'
#' @inheritParams stat_slabinterval
#' @inheritParams geom_slabinterval
#' @param slab_type The type of slab function to calculate: probability density (or mass) function (`"pdf"`),
#' cumulative distribution function (`"cdf"`), or complementary CDF (`"ccdf"`).
#' @param p_limits Probability limits (as a vector of size 2) used to determine the lower and upper
#' limits of the slab. E.g., if this is `c(.001, .999)` (the default), then a slab is drawn
#' for the distribution from the quantile at `p = .001` to the quantile at `p = .999`.
#' @param limits Manually-specified limits for the slab, as a vector of length two. These limits are combined with those
#' computed based on `p_limits` as well as the limits defined by the scales of the plot to determine the
#' limits used to draw the slab functions: these limits specify the maximal limits; i.e., if specified, the limits
#' will not be wider than these (but may be narrower).Use `NA` to leave a limit alone; e.g.
#' `limits = c(0, NA)` will ensure that the lower limit does not go below 0, but let the upper limit
#' be determined by either `p_limits` or the scale settings.
#' @param thickness Override for the `thickness` aesthetic in [geom_slabinterval()]: the thickness
#' of the slab at each x / y value of the slab (depending on `orientation`).
#' @seealso See [geom_slabinterval()] for more information on the geom these stats
#' use by default and some of the options they have. See [stat_sample_slabinterval()]
#' for the versions of these stats that can be used on samples.
#' See `vignette("slabinterval")` for a variety of examples of use.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' tribble(
#'   ~group, ~subgroup, ~mean, ~sd,
#'   "a",          "h",     5,   1,
#'   "b",          "h",     7,   1.5,
#'   "c",          "h",     8,   1,
#'   "c",          "i",     9,   1,
#'   "c",          "j",     7,   1
#' ) %>%
#'   ggplot(aes(x = group, dist = "norm", arg1 = mean, arg2 = sd, fill = subgroup)) +
#'   stat_dist_eye(position = "dodge")
#'
#' # the stat_dist_... family applies a Jacobian adjustment to densities
#' # when plotting on transformed scales in order to plot them correctly.
#' # For example, here is a log-Normal distribution plotted on the log
#' # scale, where it will appear Normal:
#' data.frame(dist = "lnorm") %>%
#'   ggplot(aes(y = 1, dist = dist, arg1 = log(10), arg2 = 2*log(10))) +
#'   stat_dist_halfeyeh() +
#'   scale_x_log10(breaks = 10^seq(-5,7, by = 2))
#'
#' # see vignette("slabinterval") for many more examples.
#'
#' @export
stat_dist_slabinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

  slab_type = c("pdf", "cdf", "ccdf"),
  p_limits = c(.001, .999),

  orientation = c("vertical", "horizontal"),
  limits = NULL,
  n = 501,
  .width = c(.66, .95),

  show_slab = TRUE,
  show_interval = TRUE,

  na.rm = FALSE,

  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
  orientation = match.arg(orientation)
  slab_type = match.arg(slab_type)

  layer(
    data = data,
    mapping = mapping,
    stat = StatDistSlabinterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      slab_type = slab_type,
      p_limits = p_limits,

      orientation = orientation,

      limits_function = dist_limits_function,
      limits_args = list(),
      limits = limits,

      slab_function = dist_slab_function,
      slab_args = list(),
      n = n,

      interval_function = dist_interval_function,
      interval_args = list(),
      point_interval = NULL,
      .width = .width,

      show_slab = show_slab,
      show_interval = show_interval,

      na.rm = na.rm,
      ...
    )
  )
}

#' @importFrom plyr defaults
StatDistSlabinterval = ggproto("StatDistSlabinterval", StatSlabinterval,
  default_aes = defaults(aes(
    dist = NULL,
    args = NULL,
    arg1 = NULL,
    arg2 = NULL,
    arg3 = NULL,
    arg4 = NULL,
    arg5 = NULL,
    arg6 = NULL,
    arg7 = NULL,
    arg8 = NULL,
    arg9 = NULL
  ), StatSlabinterval$default_aes),

  extra_params = c(
    StatSlabinterval$extra_params,
    "slab_type",
    "p_limits"
  ),

  # interval parameter used to determine if the stat re-groups
  # data by the combination of input group + dist + args. For
  # most use cases this should be TRUE, but for some corner
  # cases (like lineribbons), this needs to be FALSE or weird
  # things will happen.
  group_by_dist = TRUE,

  default_params = defaults(list(
    slab_type = "pdf",
    p_limits = c(.001, .999),

    limits_function = dist_limits_function,
    slab_function = dist_slab_function,
    interval_function = dist_interval_function
  ), StatSlabinterval$default_params),

  setup_params = function(self, data, params) {
    params = ggproto_parent(StatSlabinterval, self)$setup_params(data, params)

    params$limits_args = list(
      p_limits = params$p_limits %||% self$default_params$p_limits
    )

    params$slab_args = list(
      slab_type = params$slab_type %||% self$default_params$slab_type
    )

    params
  },

  setup_data = function(self, data, params) {
    data = ggproto_parent(StatSlabinterval, self)$setup_data(data, params)

    # ignore unknown distributions (with a warning)
    data$dist = check_dist_name(data$dist)

    if (self$group_by_dist) {
      # need to treat dist *and* args as grouping variables (else things will break)
      group_cols = intersect(c("dist", "args", paste0("arg", 1:9), "group"), names(data))
      # need to do as.character() here because list columns (as in args) won't work
      # with interaction()
      group_data = lapply(data[,group_cols], as.character)
      data$group = as.numeric(interaction(group_data))
    }

    data
  }
)


# shortcut stats ----------------------------------------------------------

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_halfeye = function(...) stat_dist_slabinterval(...)
#' @export
#' @rdname stat_dist_slabinterval
stat_dist_halfeyeh = function(..., orientation = "horizontal") stat_dist_slabinterval(..., orientation = orientation)

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_eye = function(..., side = "both") stat_dist_slabinterval(..., side = side)
#' @export
#' @rdname stat_dist_slabinterval
stat_dist_eyeh = function(..., side = "both", orientation = "horizontal")
  stat_dist_slabinterval(..., side = side, orientation = orientation)

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_ccdfinterval = function(...,
  slab_type = "ccdf", justification = 0.5, side = "left", normalize = "none"
) {
  stat_dist_slabinterval(..., slab_type = slab_type, justification = justification, side = side, normalize = normalize)
}
#' @export
#' @rdname stat_dist_slabinterval
stat_dist_ccdfintervalh = function(...,
  slab_type = "ccdf", justification = 0.5, side = "top", orientation = "horizontal", normalize = "none"
) {
  stat_dist_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_cdfinterval = function(...,
  slab_type = "cdf", justification = 0.5, side = "left", normalize = "none"
) {
  stat_dist_slabinterval(..., slab_type = slab_type, justification = justification, side = side, normalize = normalize)
}
#' @export
#' @rdname stat_dist_slabinterval
stat_dist_cdfintervalh = function(...,
  slab_type = "cdf", justification = 0.5, side = "top", orientation = "horizontal", normalize = "none"
) {
  stat_dist_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_gradientinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

  justification = 0.5,
  thickness = 1,

  show.legend = c(size = FALSE, slab_alpha = FALSE),
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDistGradientinterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      justification = justification,
      thickness = thickness,
      ...
    )
  )
}
#' @export
#' @rdname stat_dist_slabinterval
stat_dist_gradientintervalh = function(..., orientation = "horizontal") {
  stat_dist_gradientinterval(..., orientation = orientation)
}
StatDistGradientinterval = ggproto("StatDistGradientinterval", StatDistSlabinterval,
  default_aes = defaults(aes(
    thickness = 1,
    slab_alpha = stat(f)
  ), StatDistSlabinterval$default_aes),

  default_params = defaults(list(
    justification = 0.5
  ), StatDistSlabinterval$default_params)
)

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_pointinterval = function(..., show_slab = FALSE) stat_dist_slabinterval(..., show_slab = show_slab)
#' @export
#' @rdname stat_dist_slabinterval
stat_dist_pointintervalh = function(..., show_slab = FALSE, orientation = "horizontal") {
  stat_dist_slabinterval(..., show_slab = show_slab, orientation = orientation)
}

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_interval = function(
  mapping = NULL,
  data = NULL,
  geom = "interval",
  position = "identity",
  ...,

  show_slab = FALSE,
  show_point = FALSE,

  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDistInterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      show_point = show_point,
      show_slab = show_slab,
      ...
    )
  )
}
StatDistInterval = ggproto("StatDistInterval", StatDistSlabinterval,
  default_aes = defaults(aes(
    color = stat(level)
  ), StatDistSlabinterval$default_aes),

  default_params = defaults(list(
    show_slab = FALSE,
    show_point = FALSE,
    .width = c(.50, .80, .95)
  ), StatDistSlabinterval$default_params)
)
# have to remove this here instead of in call to defaults()
# because otherwise it stays in the list as a value = NULL
# instead of being removed
StatDistInterval$default_aes$size = NULL
#' @export
#' @rdname stat_dist_slabinterval
stat_dist_intervalh = function(..., orientation = "horizontal") {
  stat_dist_interval(..., orientation = orientation)
}

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_slab = function(
  mapping = NULL,
  data = NULL,
  geom = "slab",
  position = "identity",
  ...,

  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDistSlab,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      show_point = FALSE,
      show_interval = FALSE,
      ...
    )
  )
}
StatDistSlab = ggproto("StatDistSlab", StatDistSlabinterval,
  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), StatDistSlabinterval$default_params)
)
StatDistSlab$default_aes$size = NULL
#' @export
#' @rdname stat_dist_slabinterval
stat_dist_slabh = function(..., orientation = "horizontal") {
  stat_dist_slab(..., orientation = orientation)
}
