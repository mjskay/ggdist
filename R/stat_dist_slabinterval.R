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

dist_limits_function = function(df, p_limits = c(NA, NA), trans = scales::identity_trans(), ...) {
  pmap_dfr_(df, function(dist, ...) {
    if (is.null(dist) || anyNA(dist)) {
      return(data.frame(.lower = NA, .upper = NA))
    }

    args = args_from_aes(...)
    quantile_fun = distr_quantile(dist)

    # if the lower or upper p limit is NA, check to see if the dist has a
    # finite limit on the transformed drawing scale, otherwise use .001 or
    # .999 as p limit. This ensures that distributions with finite limits
    # can be displayed right up to their limits by default.
    if (is.na(p_limits[[1]])) {
      lower_limit = trans$transform(do.call(quantile_fun, c(0, args)))
      p_limits[[1]] = if (is.finite(lower_limit)) 0 else .001
    }
    if (is.na(p_limits[[2]])) {
      upper_limit = trans$transform(do.call(quantile_fun, c(1, args)))
      p_limits[[2]] = if (is.finite(upper_limit)) 1 else .999
    }

    limits = do.call(quantile_fun, c(list(quote(p_limits)), args))

    data.frame(
      .lower = limits[[1]],
      .upper = limits[[2]]
    )
  })
}

#' @importFrom dplyr lag
dist_slab_function = function(
  df, input, slab_type = "pdf", limits = NULL, n = 501, outline_bars = FALSE, trans = scales::identity_trans(), ...
) {
  pmap_dfr_(df, function(dist, ...) {
    if (is.null(dist) || anyNA(dist)) {
      return(data.frame(.input = NA, .value = NA))
    }

    args = args_from_aes(...)

    # calculate pdf and cdf
    cdf_fun = distr_cdf(dist)
    if (trans$name == "identity") {
      pdf_fun = distr_pdf(dist)
      if (distr_is_constant(dist, args)) {
        # for constant distributions, to reliably get the infinite point density
        # and a constant line in the CDF, need to manually pick input values
        quantile_fun = distr_quantile(dist)
        input_2 = do.call(quantile_fun, c(list(0.5), args))
        input_1 = min(input, input_2)
        input_3 = max(input, input_2)
        input = c(input_1, input_2, input_2, input_2, input_3)
        pdf = c(0, 0, Inf, 0, 0)
        cdf = c(0, 0, 1, 1, 1)
      } else if (distr_is_discrete(dist, args)) {
        # for discrete distributions, we have to adjust the positions of the x
        # values to create bin-like things
        input_ = unique(round(input))   # center of bin
        input_1 = input_ - 0.5          # first edge of bin
        input_2 = input_ + 0.5          # second edge of bin
        pdf = do.call(pdf_fun, c(list(quote(input_)), args))
        cdf = do.call(cdf_fun, c(list(quote(input_)), args))
        # we also need the lag of the cdf so we can make it a step function
        # at the midpoint of each bin
        lag_cdf_input = c(input_[[1]] - 1, input_[-length(input_)])
        lag_cdf = do.call(cdf_fun, c(list(quote(lag_cdf_input)), args))

        if (!outline_bars) {
          # as.vector(rbind(x, y, z, ...)) interleaves vectors x, y, z, ..., giving
          # us the bin endpoints and midpoints --- then just need to repeat the same
          # value of density for both endpoints of the same bin and to make sure the
          # cdf is a step function that steps at the midpoint of the bin
          input = as.vector(rbind(input_1, input_, input_, input_2))
          pdf = rep(pdf, each = 4)
          cdf = as.vector(rbind(lag_cdf, lag_cdf, cdf, cdf))
        } else {
          # have to return to 0 in between each bar so that bar outlines are drawn
          input = as.vector(rbind(input_1, input_1, input_, input_, input_2, input_2))
          pdf = as.vector(rbind(0, pdf, pdf, pdf, pdf, 0))
          cdf = as.vector(rbind(lag_cdf, lag_cdf, lag_cdf, cdf, cdf, cdf))
        }
      } else {
        pdf = do.call(pdf_fun, c(list(quote(input)), args))
        cdf = do.call(cdf_fun, c(list(quote(input)), args))
      }
    } else {
      # must transform the density according to the scale transformation
      pdf_fun = function(x, ...) transform_pdf(distr_pdf(dist), trans$transform(x), trans, g_inverse_at_y = x, ...)
      pdf = do.call(pdf_fun, c(list(quote(input)), args))
      cdf = do.call(cdf_fun, c(list(quote(input)), args))
    }

    value = switch(slab_type,
      pdf = pdf,
      cdf = cdf,
      ccdf = 1 - cdf
    )

    data.frame(
      .input = input,
      .value = value,
      pdf = pdf,
      cdf = cdf
    )
  })
}

dist_interval_function = function(df, .width, trans, ...) {
  pmap_dfr_(df, function(dist, ...) {
    if (is.null(dist) || anyNA(dist)) {
      return(data.frame(.value = NA, .lower = NA, .upper = NA, .width = .width))
    }

    args = args_from_aes(...)
    quantile_fun = distr_quantile(dist)

    intervals = map_dfr_(.width, function(w) {
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
#' [geom_slabinterval()]. Uses the `dist` aesthetic to specify a distribution using
#' objects from the [distributional](https://pkg.mitchelloharawild.com/distributional/) package,
#' or using distribution names and `arg1`, ... `arg9` aesthetics (or `args` as a list column)
#' to specify distribution arguments. See *Details*.
#'
#' A highly configurable stat for generating a variety of plots that combine a "slab"
#' that describes a distribution plus an interval. Several "shortcut" stats are provided
#' which combine multiple options to create useful geoms, particularly *eye plots*
#' (a combination of a violin plot and interval), *half-eye plots* (a density plus interval),
#' and *CCDF bar plots* (a complementary CDF plus interval).
#'
#' The shortcut stat names follow the pattern `stat_dist_[name]`.
#'
#' Stats include:
#'
#' \itemize{
#'   \item `stat_dist_eye`: Eye plots (violin + interval)
#'   \item `stat_dist_halfeye`: Half-eye plots (density + interval)
#'   \item `stat_dist_ccdfinterval`: CCDF bar plots (CCDF + interval)
#'   \item `stat_dist_cdfinterval`: CDF bar plots (CDF + interval)
#'   \item `stat_dist_gradientinterval`: Density gradient + interval plots
#'   \item `stat_dist_pointinterval`: Point + interval plots
#'   \item `stat_dist_interval`: Interval plots
#' }
#'
#' These stats expect a `dist` aesthetic to specify a distribution. This aesthetic
#' can be used in one of two ways:
#'
#'  - `dist` can be any distribution object from the [distributional](https://pkg.mitchelloharawild.com/distributional/)
#'    package, such as [dist_normal()], [dist_beta()], etc. Since these functions are vectorized,
#'    other columns can be passed directly to them in an [aes()] specification; e.g.
#'    `aes(dist = dist_normal(mu, sigma))` will work if `mu` and `sigma` are columns in the
#'    input data frame.
#'
#'  - `dist` can be a character vector giving the distribution name. Then the  `arg1`, ... `arg9`
#'    aesthetics (or `args` as a list column) specify distribution arguments. Distribution names
#'    should correspond to R functions that have `"p"`, `"q"`, and `"d"` functions; e.g. `"norm"`
#'    is a valid distribution name because R defines the [pnorm()], [qnorm()], and [dnorm()]
#'    functions for Normal distributions.
#'
#'    See the [parse_dist()] function for a useful way to generate `dist` and `args`
#'    values from human-readable distribution specs (like `"normal(0,1)"`). Such specs are also
#'    produced by other packages (like the `brms::get_prior` function in brms); thus,
#'    [parse_dist()] combined with the stats described here can help you visualize the output
#'    of those functions.
#'
#' @eval rd_slabinterval_computed_variables(stat_sample = FALSE)
#' @eval rd_slabinterval_aesthetics(stat = StatDistSlabinterval)
#'
#' @inheritParams stat_slabinterval
#' @inheritParams geom_slabinterval
#' @param slab_type The type of slab function to calculate: probability density (or mass) function (`"pdf"`),
#' cumulative distribution function (`"cdf"`), or complementary CDF (`"ccdf"`).
#' @param p_limits Probability limits (as a vector of size 2) used to determine the lower and upper
#' limits of the slab. E.g., if this is `c(.001, .999)`, then a slab is drawn
#' for the distribution from the quantile at `p = .001` to the quantile at `p = .999`. If the lower
#' (respectively upper) limit is `NA`, then the lower (upper) limit will be the minimum (maximum) of the
#' distribution's support if it is finite, and `0.001` (`0.999`) if it is not finite. E.g., if
#' `p_limits` is `c(NA, NA)` on a gamma distribution the effective value of `p_limits` would be
#' `c(0, .999)` since the gamma distribution is defined on `(0, Inf)`; whereas on a normal distribution
#' it would be equivalent to `c(.001, .999)` since the normal distribution is defined on `(-Inf, Inf)`.
#' @param outline_bars For discrete distributions (whose slabs are drawn as histograms), determines
#' if outlines in between the bars are drawn when the `slab_color` aesthetic is used. If `FALSE`
#' (the default), the outline is drawn only along the tops of the bars; if `TRUE`, outlines in between
#' bars are also drawn.
#' @param limits Manually-specified limits for the slab, as a vector of length two. These limits are combined with those
#' computed based on `p_limits` as well as the limits defined by the scales of the plot to determine the
#' limits used to draw the slab functions: these limits specify the maximal limits; i.e., if specified, the limits
#' will not be wider than these (but may be narrower).Use `NA` to leave a limit alone; e.g.
#' `limits = c(0, NA)` will ensure that the lower limit does not go below 0, but let the upper limit
#' be determined by either `p_limits` or the scale settings.
#' @return A [ggplot2::Stat] representing a slab or combined slab+interval geometry which can
#' be added to a [ggplot()] object.
#' @seealso See [geom_slabinterval()] for more information on the geom these stats
#' use by default and some of the options they have. See [stat_sample_slabinterval()]
#' for the versions of these stats that can be used on samples.
#' See `vignette("slabinterval")` for a variety of examples of use.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' library(distributional)
#'
#' theme_set(theme_ggdist())
#'
#' dist_df = tribble(
#'   ~group, ~subgroup, ~mean, ~sd,
#'   "a",          "h",     5,   1,
#'   "b",          "h",     7,   1.5,
#'   "c",          "h",     8,   1,
#'   "c",          "i",     9,   1,
#'   "c",          "j",     7,   1
#' )
#'
#' dist_df %>%
#'   ggplot(aes(x = group, dist = "norm", arg1 = mean, arg2 = sd, fill = subgroup)) +
#'   stat_dist_eye(position = "dodge")
#'
#' # Using functions from the distributional package (like dist_normal()) with the
#' # dist aesthetic can lead to more compact/expressive specifications
#'
#' dist_df %>%
#'   ggplot(aes(x = group, dist = dist_normal(mean, sd), fill = subgroup)) +
#'   stat_dist_eye(position = "dodge")
#'
#' # the stat_dist_... family applies a Jacobian adjustment to densities
#' # when plotting on transformed scales in order to plot them correctly.
#' # It determines the Jacobian using symbolic differentiation if possible,
#' # using stats::D(). If symbolic differentation fails, it falls back
#' # to numericDeriv(), which is less reliable; therefore, it is
#' # advisable to use scale transformation functions that are defined in
#' # terms of basic math functions so that their derivatives can be
#' # determined analytically (most of the transformation functions in the
#' # scales package currently have this property).
#' # For example, here is a log-Normal distribution plotted on the log
#' # scale, where it will appear Normal:
#' data.frame(dist = "lnorm", logmean = log(10), logsd = 2*log(10)) %>%
#'   ggplot(aes(y = 1, dist = dist, arg1 = logmean, arg2 = logsd)) +
#'   stat_dist_halfeye() +
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
  p_limits = c(NA, NA),
  outline_bars = FALSE,

  orientation = NA,
  limits = NULL,
  n = 501,
  .width = c(.66, .95),

  show_slab = TRUE,
  show_interval = TRUE,

  na.rm = FALSE,

  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
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
      outline_bars = outline_bars,

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

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
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
    "p_limits",
    "outline_bars"
  ),

  # interval parameter used to determine if the stat re-groups
  # data by the combination of input group + dist + args. For
  # most use cases this should be TRUE, but for some corner
  # cases (like lineribbons), this needs to be FALSE or weird
  # things will happen.
  group_by_dist = TRUE,

  default_params = defaults(list(
    slab_type = "pdf",
    p_limits = c(NA, NA),
    outline_bars = FALSE,

    limits_function = dist_limits_function,
    slab_function = dist_slab_function,
    interval_function = dist_interval_function
  ), StatSlabinterval$default_params),

  setup_params = function(self, data, params) {
    params = defaults(params, self$default_params)

    # detect orientation -- this must be done before calling up to StatSlabInterval
    # since auto-detection here is different (main_is_orthogonal needs to be FALSE)
    params$flipped_aes = get_flipped_aes(data, params,
      main_is_orthogonal = FALSE, range_is_orthogonal = TRUE, group_has_equal = TRUE, main_is_optional = TRUE
    )
    params$orientation = get_orientation(params$flipped_aes)

    params = ggproto_parent(StatSlabinterval, self)$setup_params(data, params)

    params$limits_args = list(
      p_limits = params$p_limits %||% self$default_params$p_limits
    )

    params$slab_args = list(
      slab_type = params$slab_type %||% self$default_params$slab_type,
      outline_bars = params$outline_bars %||% self$default_params$outline_bars
    )

    params
  },

  setup_data = function(self, data, params) {
    data = ggproto_parent(StatSlabinterval, self)$setup_data(data, params)
    define_orientation_variables(params$orientation)

    # pull out the x (secondary) axis into the dist aesthetic
    if (is_dist_like(data[[x]])) {
      data$dist = data[[x]]
    } else if (is.null(data$dist) && !is.null(data[[x]])) {
      # dist aesthetic is not provided but x aesthetic is, and x is not a dist
      # this means we need to wrap it as a dist_sample
      data = summarise_by(data, c("PANEL", y, "group"), function(d) {
        data.frame(dist = dist_sample(list(d[[x]])))
      })
    }

    # for x and y axes that have distributions mapped to them, replace them
    # with the medians of those distributions so that those scales are handled
    # appropriately by base ggplot x/y scale handling code
    for (xy in c("x", "y")) {
      if (is_dist_like(data[[xy]])) {
        data[[xy]] = median(data[[xy]])
      }
    }

    # ignore unknown distributions (with a warning)
    if (is.character(data$dist) || is.factor(data$dist)) {
      data$dist = check_dist_name(data$dist)
    }

    if (self$group_by_dist) {
      # Need to group by rows in the data frame to draw correctly, as
      # each output slab will need to be in its own group.
      # First check if we are grouped by rows already (in which case leave it)
      if (length(unique(data$group)) != nrow(data)) {
        # need to make new groups that ensure every row is unique but which
        # preserve old group order at the top level
        data$group = as.numeric(interaction(
          data$group, seq_len(nrow(data)), drop = TRUE, lex.order = TRUE
        ))
      }
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
stat_dist_eye = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDistEye,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      ...
    )
  )
}
StatDistEye = ggproto("StatDistEye", StatDistSlabinterval,
  default_aes = defaults(aes(
    side = stat("both"),
  ), StatDistSlabinterval$default_aes)
)

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_ccdfinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

  slab_type = "ccdf",
  normalize = "none",

  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDistCcdfInterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      slab_type = slab_type,
      normalize = normalize,
      ...
    )
  )
}
StatDistCcdfInterval = ggproto("StatDistCcdfInterval", StatDistSlabinterval,
  default_aes = defaults(aes(
    justification = stat(0.5),
    side = stat("topleft"),
  ), StatDistSlabinterval$default_aes),

  default_params = defaults(list(
    slab_type = "ccdf",
    normalize = "none"
  ), StatDistSlabinterval$default_params)
)

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_cdfinterval = function(...,
  slab_type = "cdf", normalize = "none"
) {
  stat_dist_ccdfinterval(..., slab_type = slab_type, normalize = normalize)
}

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_gradientinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

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
      ...
    )
  )
}
StatDistGradientinterval = ggproto("StatDistGradientinterval", StatDistSlabinterval,
  default_aes = defaults(aes(
    justification = stat(0.5),
    thickness = stat(1),
    slab_alpha = stat(f)
  ), StatDistSlabinterval$default_aes)
)

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_pointinterval = function(..., show_slab = FALSE) stat_dist_slabinterval(..., show_slab = show_slab)

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
