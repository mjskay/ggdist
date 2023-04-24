# Distribution + interval stat for analytical and sample-based distributions
#
# Author: mjskay
###############################################################################


# compute_limits ----------------------------------------------------------

#' StatSlabinterval$compute_limits()
#' @noRd
compute_limits_slabinterval = function(
  self, data, trans, orientation,
  p_limits,
  trim, adjust,
  ...
) {
  dist = check_one_dist(data$dist)
  if (distr_is_missing(dist)) {
    return(data.frame(.lower = NA, .upper = NA))
  }

  if (distr_is_factor_like(dist)) {
    # limits on factor-like dists are determined by the scale, which will
    # have been set earlier (in layer_slabinterval()), so we don't have to
    # do it here
    return(data.frame(.lower = NA, .upper = NA))
  }

  if (distr_is_constant(dist)) {
    median = distr_quantile(dist)(0.5)
    return(data.frame(.lower = median, .upper = median))
  }

  if (distr_is_sample(dist)) {
    sample = distr_get_sample(dist)
    return(compute_limits_sample(sample, trans, trim, adjust, ...))
  }

  quantile_fun = distr_quantile(dist)

  # if the lower or upper p limit is NA, check to see if the dist has a
  # finite limit on the transformed drawing scale, otherwise use .001 or
  # .999 as p limit. This ensures that distributions with finite limits
  # can be displayed right up to their limits by default.
  if (is.na(p_limits[[1]])) {
    lower_limit = trans$transform(quantile_fun(0))
    p_limits[[1]] = if (all(is.finite(lower_limit))) 0 else .001
  }
  if (is.na(p_limits[[2]])) {
    upper_limit = trans$transform(quantile_fun(1))
    p_limits[[2]] = if (all(is.finite(upper_limit))) 1 else .999
  }

  # need to use min / max here in case of multivariate distributions
  # (e.g. distributional::dist_multivariate_normal())
  lower_limit = min(quantile_fun(p_limits[[1]]))
  upper_limit = max(quantile_fun(p_limits[[2]]))

  data.frame(
    .lower = lower_limit,
    .upper = upper_limit
  )
}

#' compute limits of the provided sample
#' @param x sample data on **original** scale
#' @param trans scale transformation
#' @param trim/adjust see stat_slabinterval
#' @noRd
compute_limits_sample = function(x, trans, trim, adjust, ..., density = "auto") {
  density = match_function(density, "density_")

  # determine limits of data based on the density estimator
  x = trans$transform(x)
  x_range = range(density(x, n = 2, range_only = TRUE, trim = trim, adjust = adjust)$x)
  data.frame(
    .lower = trans$inverse(x_range[[1]]),
    .upper = trans$inverse(x_range[[2]])
  )
}


# compute_slab ------------------------------------------------------------

#' StatSlabinterval$compute_slab()
#' @noRd
compute_slab_slabinterval = function(
  self, data, scales, trans, input, orientation,
  slab_type, limits, n,
  adjust, trim, expand, breaks, align, outline_bars,
  ...
) {
  dist = data$dist
  # TODO: add support for multivariate distributions
  if (distr_is_missing(dist) || distr_is_multivariate(dist)) {
    return(data.frame(.input = NA_real_, f = NA_real_, n = NA_integer_))
  }

  # calculate pdf and cdf
  cdf_fun = distr_cdf(dist)
  if (distr_is_constant(dist)) {
    # for constant distributions, to reliably get the infinite point density
    # and a constant line in the CDF, need to manually pick input values
    quantile_fun = distr_quantile(dist)
    input_2 = quantile_fun(0.5)
    input_1 = min(input, input_2)
    input_3 = max(input, input_2)
    input = c(input_1, input_2, input_2, input_2, input_3)
    pdf = c(0, 0, Inf, 0, 0)
    cdf = c(0, 0, 1, 1, 1)
    if (!expand) {
      input = input[-c(1,5)]
      pdf = pdf[-c(1,5)]
      cdf = cdf[-c(1,5)]
    }
  } else if (!distr_is_factor_like(dist) && distr_is_sample(dist)) {
    return(compute_slab_sample(
      trans$transform(distr_get_sample(dist)), scales, trans, input,
      slab_type = slab_type, limits = limits, n = n,
      adjust = adjust, trim = trim, expand = expand,
      breaks = breaks, align = align, outline_bars = outline_bars,
      ...
    ))
  } else if (trans$name == "identity") {
    pdf_fun = distr_pdf(dist)
    if (distr_is_discrete(dist)) {
      # for discrete distributions, we have to adjust the positions of the x
      # values to create bin-like things
      input_ = unique(round(input))   # center of bin
      input_1 = input_ - 0.5          # first edge of bin
      input_2 = input_ + 0.5          # second edge of bin
      pdf = pdf_fun(input_)
      cdf = cdf_fun(input_)
      # we also need the lag of the cdf so we can make it a step function
      # at the midpoint of each bin
      lag_cdf_input = c(input_[[1]] - 1, input_[-length(input_)])
      lag_cdf = cdf_fun(lag_cdf_input)

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
      pdf = pdf_fun(input)
      cdf = cdf_fun(input)
    }
  } else {
    # must transform the density according to the scale transformation
    pdf_fun = function(x, ...) transform_pdf(distr_pdf(dist), trans$transform(x), trans, g_inverse_at_y = x, ...)
    pdf = pdf_fun(input)
    cdf = cdf_fun(input)
  }

  data.frame(
    .input = input,
    f = get_slab_function(slab_type, list(pdf = pdf, cdf = cdf)),
    pdf = pdf,
    cdf = cdf,
    n = if (distr_is_sample(dist)) length(distr_get_sample(dist)) else Inf
  )
}

#' compute slab functions for the provided sample
#' @param x sample data on **transformed** scale
#' @param trans scale transformation
#' @param (others) see stat_slabinterval
#' @importFrom stats density
#' @importFrom graphics hist
#' @noRd
compute_slab_sample = function(
  x, scales, trans, input,
  slab_type, limits, n,
  adjust, trim, expand, breaks, align, outline_bars,
  density,
  ...
) {

  if (is.integer(x) || inherits(x, "mapped_discrete")) {
    # discrete variables are always displayed as histograms
    slab_type = "histogram"
    breaks = seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE) + 1) - 0.5
  }

  if (slab_type == "histogram") density = "histogram"
  density = match_function(density, prefix = "density_")

  # calculate pdf and cdf
  # TODO: pass weights here
  d = density(
    x, n = n, adjust = adjust, trim = trim,
    breaks = breaks, align = align, outline_bars = outline_bars
  )
  slab_df = data.frame(
    .input = trans$inverse(d$x),
    pdf = d$y,
    # TODO: pass weights here
    cdf = d$cdf %||% weighted_ecdf(x)(d$x)
  )

  # extend x values to the range of the plot. To do that we have to include
  # x values requested from the original `input` if they are outside the
  # range of the slab
  expand = rep_len(expand, 2L)

  if (expand[[1]]) {
    input_below_slab = input[input < min(slab_df$.input) - .Machine$double.eps]
    if (length(input_below_slab) > 0) {
      slab_df = rbind(data.frame(
        .input = input_below_slab,
        pdf = 0,
        cdf = 0
      ), slab_df)
    }
  }
  if (expand[[2]]) {
    input_above_slab = input[input > max(slab_df$.input) + .Machine$double.eps]
    if (length(input_above_slab) > 0) {
      slab_df = rbind(slab_df, data.frame(
        .input = input_above_slab,
        pdf = 0,
        cdf = 1
      ))
    }
  }

  slab_df[["f"]] = get_slab_function(slab_type, slab_df)
  slab_df$n = length(x)
  slab_df
}


# compute_interval --------------------------------------------------------

#' StatSlabinterval$compute_interval()
#' @noRd
compute_interval_slabinterval = function(
  self, data, trans,
  orientation, point_interval,
  .width, na.rm,
  ...
) {
  if (is.null(point_interval)) return(data.frame())
  dist = data$dist
  if (distr_is_missing(dist)) {
    return(data.frame(.value = NA_real_, .lower = NA_real_, .upper = NA_real_, .width = .width))
  }

  distr_point_interval(dist, point_interval, trans = trans, .width = .width, na.rm = na.rm)
}


# stat_slabinterval --------------------------------------------------

#' Slab + interval plots for sample data and analytical distributions (ggplot stat)
#'
#' "Meta" stat for computing distribution functions (densities or CDFs) + intervals for use with
#' [geom_slabinterval()]. Useful for creating eye plots, half-eye plots, CCDF bar plots,
#' gradient plots, histograms, and more. Sample data can be supplied to the `x` and `y`
#' aesthetics or analytical distributions (in a variety of formats) can be supplied to the
#' `xdist` and `ydist` aesthetics.
#' See **Details**.
#'
#' A highly configurable stat for generating a variety of plots that combine a "slab"
#' that describes a distribution plus a point summary and any number of intervals.
#' Several "shortcut" stats are provided
#' which combine multiple options to create useful geoms, particularly *eye plots*
#' (a violin plot of density plus interval), *half-eye plots* (a density plot plus interval),
#' *CCDF bar plots* (a complementary CDF plus interval), and *gradient plots*
#' (a density encoded in color alpha plus interval).
#'
#' The shortcut stats include:
#'
#'  - [stat_eye()]: Eye plots (violin + interval)
#'  - [stat_halfeye()]: Half-eye plots (density + interval)
#'  - [stat_ccdfinterval()]: CCDF bar plots (CCDF + interval)
#'  - [stat_cdfinterval()]: CDF bar plots (CDF + interval)
#'  - [stat_gradientinterval()]: Density gradient + interval plots
#'  - [stat_slab()]: Density plots
#'  - [stat_histinterval()]: Histogram + interval plots
#'  - [stat_pointinterval()]: Point + interval plots
#'  - [stat_interval()]: Interval plots
#'
#' @template details-x-y-xdist-ydist
#' @eval rd_slabinterval_computed_variables(stat = StatSlabinterval)
#' @eval rd_slabinterval_aesthetics(stat = StatSlabinterval)
#' @eval rd_layer_params("slabinterval", stat = StatSlabinterval, as_dots = TRUE)
#'
#' @inheritParams geom_slabinterval
#' @inheritParams density_histogram
#' @param geom Use to override the default connection between
#' [stat_slabinterval()] and [geom_slabinterval()]
#' @param slab_type (deprecated) The type of slab function to calculate: probability density (or mass) function (`"pdf"`),
#' cumulative distribution function (`"cdf"`), or complementary CDF (`"ccdf"`). Instead of using `slab_type` to
#' change `f` and then mapping `f` onto an aesthetic, it is now recommended to simply map the corresponding
#' computed variable (e.g. `pdf`, `cdf`, or  `1 - cdf`) directly onto the desired aesthetic.
#' @param p_limits Probability limits (as a vector of size 2) used to determine the lower and upper
#' limits of the slab. E.g., if this is `c(.001, .999)`, then a slab is drawn
#' for the distribution from the quantile at `p = .001` to the quantile at `p = .999`. If the lower
#' (respectively upper) limit is `NA`, then the lower (upper) limit will be the minimum (maximum) of the
#' distribution's support if it is finite, and `0.001` (`0.999`) if it is not finite. E.g., if
#' `p_limits` is `c(NA, NA)` on a gamma distribution the effective value of `p_limits` would be
#' `c(0, .999)` since the gamma distribution is defined on `(0, Inf)`; whereas on a normal distribution
#' it would be equivalent to `c(.001, .999)` since the normal distribution is defined on `(-Inf, Inf)`.
#' @param outline_bars For sample data (if `density` is `"histogram"`) and for discrete analytical
#' distributions (whose slabs are drawn as histograms), determines
#' if outlines in between the bars are drawn when the `slab_color` aesthetic is used. If `FALSE`
#' (the default), the outline is drawn only along the tops of the bars; if `TRUE`, outlines in between
#' bars are also drawn. See [density_histogram()].
#' @param density Density estimator for sample data. One of:
#'  - A function which takes a numeric vector and returns a list with elements
#'    `x` (giving grid points for the density estimator) and `y` (the
#'    corresponding densities). \pkg{ggdist} provides a family of functions
#'    following this format, including [density_unbounded()] and
#'    [density_bounded()]. This format is also compatible with [stats::density()].
#'  - A string giving the suffix of a function name that starts with `"density_"`;
#'    e.g. `"bounded"` for `[density_bounded()]` or `"histogram"` for [density_histogram()].
#'    Defaults to `"auto"`, i.e. [density_auto()], which uses [density_bounded()]
#'    if `trim` is `TRUE` and [density_unbounded()] if `trim` is `FALSE`.
#' @param adjust Passed to `density`: the bandwidth for the density estimator for sample data
#' is adjusted by multiplying it by this value. See e.g. [density_auto()] for more information.
#' @param trim For sample data, should the density estimate be trimmed? How the estimate is
#' trimmed depends on the density estimator; see the `density` parameter. In the default
#' configuration (`density = "auto"`), `trim = TRUE` will use a bounded density estimator
#' ([density_bounded()]) and estimate the bounds from the data, and `trim = FALSE` will
#' use an unbounded density estimator ([density_unbounded()]). Default `TRUE`.
#' @param expand For sample data, should the slab be expanded to the limits of the scale? Default `FALSE`.
#' Can be length two to control expansion to the lower and upper limit respectively.
#' @param limits Manually-specified limits for the slab, as a vector of length two. These limits are combined with those
#' computed based on `p_limits` as well as the limits defined by the scales of the plot to determine the
#' limits used to draw the slab functions: these limits specify the maximal limits; i.e., if specified, the limits
#' will not be wider than these (but may be narrower). Use `NA` to leave a limit alone; e.g.
#' `limits = c(0, NA)` will ensure that the lower limit does not go below 0, but let the upper limit
#' be determined by either `p_limits` or the scale settings.
#' @param n Number of points at which to evaluate the function that defines the slab.
#' @param point_interval A function from the [point_interval()] family (e.g., `median_qi`,
#'   `mean_qi`, `mode_hdi`, etc), or a string giving the name of a function from that family
#'   (e.g., `"median_qi"`, `"mean_qi"`, `"mode_hdi"`, etc; if a string, the caller's environment is searched
#'   for the function, followed by the \pkg{ggdist} environment). This function determines the point summary
#'   (typically mean, median, or mode) and interval type (quantile interval, `qi`;
#'   highest-density interval, `hdi`; or highest-density continuous interval, `hdci`). Output will
#'   be converted to the appropriate `x`- or `y`-based aesthetics depending on the value of `orientation`.
#'   See the [point_interval()] family of functions for more information.
#' @param .width The `.width` argument passed to `point_interval`: a vector of probabilities to use
#' that determine the widths of the resulting intervals. If multiple probabilities are provided,
#' multiple intervals per group are generated, each with a different probability interval (and
#' value of the corresponding `.width` and `level` generated variables).
#' @param show.legend Should this layer be included in the legends? Default is `c(size = FALSE)`, unlike most geoms,
#' to match its common use cases. `FALSE` hides all legends, `TRUE` shows all legends, and `NA` shows only
#' those that are mapped (the default for most geoms).
#' @return A [ggplot2::Stat] representing a slab or combined slab+interval geometry which can
#' be added to a [ggplot()] object.
#' @seealso See [geom_slabinterval()] for more information on the geom these stats
#' use by default and some of the options it has.
#' See `vignette("slabinterval")` for a variety of examples of use.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' library(distributional)
#'
#' theme_set(theme_ggdist())
#'
#'
#' # EXAMPLES ON SAMPLE DATA
#' set.seed(1234)
#' df = data.frame(
#'   group = c("a", "b", "c", "c", "c"),
#'   value = rnorm(2500, mean = c(5, 7, 9, 9, 9), sd = c(1, 1.5, 1, 1, 1))
#' )
#'
#' # here are vertical eyes:
#' df %>%
#'   ggplot(aes(x = group, y = value)) +
#'   stat_eye()
#'
#' # note the sample size is not automatically incorporated into the
#' # area of the densities in case one wishes to plot densities against
#' # a reference (e.g. a prior distribution).
#' # But you may wish to account for sample size if using these geoms
#' # for something other than visualizing posteriors; in which case
#' # you can use after_stat(f*n):
#' df %>%
#'   ggplot(aes(x = group, y = value)) +
#'   stat_eye(aes(thickness = after_stat(pdf*n)))
#'
#'
#' # EXAMPLES ON ANALYTICAL DISTRIBUTIONS
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
#' # Using functions from the distributional package (like dist_normal()) with the
#' # dist aesthetic can lead to more compact/expressive specifications
#'
#' dist_df %>%
#'   ggplot(aes(x = group, ydist = dist_normal(mean, sd), fill = subgroup)) +
#'   stat_eye(position = "dodge")
#'
#' # using the old character vector + args approach
#' dist_df %>%
#'   ggplot(aes(x = group, dist = "norm", arg1 = mean, arg2 = sd, fill = subgroup)) +
#'   stat_eye(position = "dodge")
#'
#' # the stat_slabinterval family applies a Jacobian adjustment to densities
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
#'   stat_halfeye() +
#'   scale_x_log10(breaks = 10^seq(-5,7, by = 2))
#'
#' # see vignette("slabinterval") for many more examples.
#'
#' @name stat_slabinterval
#' @importFrom distributional dist_wrap dist_missing dist_sample is_distribution
NULL

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSlabinterval = ggproto("StatSlabinterval", AbstractStatSlabinterval,
  aes_docs = defaults(list(
    x = 'x position of the geometry (when orientation = `"vertical"`); or sample data to be summarized
    (when `orientation = "horizontal"` with sample data).',
    y = 'y position of the geometry (when orientation = `"horizontal"`); or sample data to be summarized
    (when `orientation = "vertical"` with sample data).',
    xdist = 'When using analytical distributions, distribution to map on the x axis: a \\pkg{distributional}
    object (e.g. [dist_normal()]) or a [posterior::rvar()] object.',
    ydist = 'When using analytical distributions, distribution to map on the y axis: a \\pkg{distributional}
    object (e.g. [dist_normal()]) or a [posterior::rvar()] object.',
    dist = 'When using analytical distributions, a name of a distribution (e.g. `"norm"`), a
    \\pkg{distributional} object (e.g. [dist_normal()]), or a [posterior::rvar()] object. See **Details**.',
    args = 'Distribution arguments (`args` or `arg1`, ... `arg9`). See **Details**.'
  ), AbstractStatSlabinterval$aes_docs),

  default_aes = defaults(aes(
    xdist = NULL,
    ydist = NULL,
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
  ), AbstractStatSlabinterval$default_aes),

  # interval parameter used to determine if the stat re-groups
  # data by the combination of input group + dist + args. For
  # most use cases this should be TRUE, but for some corner
  # cases (like lineribbons), this needs to be FALSE or weird
  # things will happen.
  group_by_dist = TRUE,

  default_params = defaults(list(
    p_limits = c(NA, NA),
    density = "auto",
    adjust = 1,
    trim = TRUE,
    expand = FALSE,
    breaks = "Sturges",
    align = "none",
    outline_bars = FALSE,

    point_interval = "median_qi",

    # deprecated parameters
    slab_type = NULL   # deprecated, set by default_slab_type (below)
  ), AbstractStatSlabinterval$default_params),

  layer_function = "layer_slabinterval",

  # overrides slab_type in setup_params() when slab_type is NULL (the default).
  # This allows us to detect if the user sets slab_type (which is deprecated)
  # and throw a warning.
  default_slab_type = "pdf",

  # orientation auto-detection here is different from base AbstractStatSlabinterval
  # (main_is_orthogonal needs to be FALSE)
  orientation_options = defaults(list(
    secondary_is_dist = TRUE
  ), AbstractStatSlabinterval$orientation_options),

  setup_data = function(self, data, params) {
    data = ggproto_parent(AbstractStatSlabinterval, self)$setup_data(data, params)
    define_orientation_variables(params$orientation)

    # check for dist-like objects in x / y axis: these are likely user errors
    # caused by assigning a distribution to x / y instead of xdist / ydist
    dist_like_cols = c("x","y")[map_lgl_(c("x", "y"), function(col) is_dist_like(data[[col]]))]
    if (length(dist_like_cols) > 0) {
      s = if (length(dist_like_cols) > 1) "s"
      stop0(
        "Cannot use distribution or rvar objects with the ", paste0("`", dist_like_cols, "`", collapse = " or "),
        " aesthetic", s, ". Use the ", paste0("`", dist_like_cols, "dist`", collapse = " or "),
        " aesthetic", s, " instead."
      )
    }

    # pull out the x (secondary) dist axis into the dist aesthetic
    if (!is.null(data[[xdist]])) {
      #TODO: reverse this: move dist into xdist and use xdist elsewhere
      data$dist = data[[xdist]]
    }

    if (is.character(data$dist) || is.factor(data$dist)) {
      # ignore unknown distributions (with a warning)
      data$dist = check_dist_name(data$dist)

      # convert character/factor dist aesthetic into distributional objects
      arg_cols = names(data)[startsWith(names(data), "arg")]
      data$dist = pmap_(data[, c("dist", arg_cols)], function(dist, ...) {
        if (is.na(dist)) {
          dist_missing()
        } else {
          args = args_from_aes(...)
          do.call(dist_wrap, c(list(dist), args))
        }
      })
    }

    if (!is.null(data$dist) && self$group_by_dist) {
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
  },

  setup_params = function(self, data, params) {
    params = ggproto_parent(AbstractStatSlabinterval, self)$setup_params(data, params)

    # override deprecated slab_type with default_slab_type
    if (is.null(params$slab_type)) {
      params$slab_type = self$default_slab_type
    } else {
      cli_warn(c(
        'The {.arg slab_type} parameter for {.pkg ggdist} stats is deprecated.',
        'i' = 'Instead of using {.arg slab_type}, use {.topic after_stat} to
          map the desired computed variable, e.g. {.code pdf} or {.code cdf}, onto
          an aesthetic, e.g. {.code aes(thickness = after_stat(pdf))}. Specifically:',
        '*' = 'To replace {.code slab_type = "pdf"}, map {.code after_stat(pdf)} onto an aesthetic.',
        '*' = 'To replace {.code slab_type = "cdf"}, map {.code after_stat(cdf)} onto an aesthetic.',
        '*' = 'To replace {.code slab_type = "ccdf"}, map {.code after_stat(1 - cdf)} onto an aesthetic.',
        '*' = 'To replace {.code slab_type = "histogram"}, map {.code after_stat(pdf)} onto an aesthetic and
          pass {.code density = "histogram"} to the stat.',
        'i' = 'For more information, see the {.emph Computed Variables} section of {.topic stat_slabinterval}.'
      ))
    }

    params
  },

  compute_panel = function(self, data, scales,
    orientation,
    na.rm,
    ...
  ) {
    define_orientation_variables(orientation)

    # convert x/y secondary positional aesthetic into dist_sample
    # must do this here instead of in setup_data so that we can invert
    # the scale transformation --- this ensures that data supplied via dist_sample
    # and data supplied via the x/y positional aesthetics can all be treated
    # as being on the untransformed scale in any subsequent code
    if (is.null(data$dist) && is.numeric(data[[x]])) {
      trans = scales[[x]]$trans %||% scales::identity_trans()

      data = remove_missing(data, na.rm, x, name = "stat_slabinterval")

      if (inherits(data[[x]], "mapped_discrete") && is_integerish(data[[x]])) {
        # integer-like mapped discrete data needs to be converted to integer here
        # so that it will be treated as a discrete distribution later
        # (e.g. by distr_is_discrete())
        data[[x]] = as.integer(data[[x]])
      }

      # dist aesthetic is not provided but x aesthetic is, and x is not a dist
      # this means we need to wrap it as a dist_sample
      data = summarise_by(data, c("PANEL", y, "group"), function(d) {
        data.frame(dist = dist_sample(list(trans$inverse(d[[x]]))))
      })
      data[[x]] = median(data$dist)
    }

    # handle logical distributions: logical distributions don't play well with
    # numeric scales, so we use a bit of a hack and convert them to integers
    # by adding 0L to them
    if (is_distribution(data$dist)) {
      is_logical = map_lgl_(data$dist, distr_is_logical)
      data$dist[is_logical] = data$dist[is_logical] + 0L
    }

    # handle rvar factors / categorical dists: our modified version of Layer$compute_aesthetics will
    # already have ensured the scale is discrete with appropriate limits, we
    # just need to adjust the rvar (or wrap the distribution) to have the same levels as the scale limits
    # (in case levels have been re-ordered / added / removed)
    if (inherits(data$dist, "rvar_factor")) {
      data$dist = posterior::rvar_factor(
        data$dist,
        levels = scales[[x]]$get_limits(),
        ordered = inherits(data$dist, "rvar_ordered")
      )
    } else if (distr_is_factor_like(data$dist)) {
      new_levels = scales[[x]]$get_limits()
      data$dist = .dist_wrapped_categorical(
        data$dist,
        new_levels = new_levels
      )
    }

    ggproto_parent(AbstractStatSlabinterval, self)$compute_panel(
      data, scales,
      orientation = orientation,
      na.rm = na.rm,
      ...
    )
  },

  # workaround (#84)
  compute_limits = function(self, ...) compute_limits_slabinterval(self, ...),
  compute_slab = function(self, ...) compute_slab_slabinterval(self, ...),
  compute_interval = function(self, ...) compute_interval_slabinterval(self, ...)
)

#' @export
#' @rdname stat_slabinterval
stat_slabinterval = make_stat(StatSlabinterval, geom = "slabinterval")


# layer for slabinterval --------------------------------------------------

#' Alternative to ggplot2::layer() which adds necessary hooks to the Layer for
#' stat_slabinterval. See the layer_function property of StatSlabinterval
#' @noRd
layer_slabinterval = function(...) {
  l = layer(...)
  ggproto(NULL, l,
    compute_aesthetics = function(self, data, plot) {
      data = ggproto_parent(l, self)$compute_aesthetics(data, plot)

      # factor-like dists: must be handled in a two-step process: first, here
      # we have to ensure the x/y scale is setup correctly as a discrete scale
      # with levels from the distribution. Then, in compute_panel, we will convert
      # factor-like dists and rvars to integers using the x/y scale.
      for (xy in c("x", "y")) {
        dist = paste0(xy, "dist")
        if (distr_is_factor_like(data[[dist]])) {
          # ensure a discrete scale has been added to the plot with appropriate limits
          scale = plot$scales$get_scales(xy)
          if (is.null(scale)) {
            scale = utils::getFromNamespace(paste0("scale_", xy, "_discrete"), "ggplot2")()
            plot$scales$add(scale)
          }
          scale$limits = scale$limits %||% distr_levels(data[[dist]])
          scale$train(posterior::draws_of(data[[dist]]))
        }
      }
      data
    }
  )
}


# shortcut stats ----------------------------------------------------------

StatHalfeye = StatSlabinterval
#' @eval rd_slabinterval_shortcut_stat("halfeye", "half-eye (density + interval)", geom_name = "slabinterval", describe = FALSE)
#' @description
#' Equivalent to [stat_slabinterval()], whose default settings create half-eye (density + interval) plots.
#' @export
stat_halfeye = stat_slabinterval

StatEye = ggproto("StatEye", StatSlabinterval,
  default_aes = defaults(aes(
    side = after_stat("both"),
  ), StatSlabinterval$default_aes)
)
#' @eval rd_slabinterval_shortcut_stat("eye", "eye (violin + interval)", geom_name = "slabinterval")
#' @export
stat_eye = make_stat(StatEye, geom = "slabinterval")

StatCcdfinterval = ggproto("StatCcdfinterval", StatSlabinterval,
  default_aes = defaults(aes(
    thickness = after_stat(thickness(1 - cdf)),
    justification = after_stat(0.5),
    side = after_stat("topleft"),
  ), StatSlabinterval$default_aes),

  default_params = defaults(list(
    normalize = "none",
    expand = TRUE
  ), StatSlabinterval$default_params),

  default_slab_type = "ccdf"
)
#' @eval rd_slabinterval_shortcut_stat("ccdfinterval", "CCDF bar", geom_name = "slabinterval", example_layers = "expand_limits(x = 0)")
#' @export
stat_ccdfinterval = make_stat(StatCcdfinterval, geom = "slabinterval")

StatCdfinterval = ggproto("StatCdfinterval", StatCcdfinterval,
  default_aes = defaults(aes(
    thickness = after_stat(thickness(cdf)),
  ), StatCcdfinterval$default_aes),

  default_slab_type = "cdf"
)
#' @eval rd_slabinterval_shortcut_stat("cdfinterval", "CDF bar", geom_name = "slabinterval")
#' @export
stat_cdfinterval = make_stat(StatCdfinterval, geom = "slabinterval")

StatGradientinterval = ggproto("StatGradientinterval", StatSlabinterval,
  default_aes = defaults(aes(
    justification = after_stat(0.5),
    thickness = after_stat(thickness(1)),
    slab_alpha = after_stat(f)
  ), StatSlabinterval$default_aes),

  default_params = defaults(list(
    fill_type = "auto"
  ), StatSlabinterval$default_params),

  layer_args = defaults(list(
    show.legend = c(size = FALSE, slab_alpha = FALSE)
  ), StatSlabinterval$layer_args)
)
#' @eval rd_slabinterval_shortcut_stat("gradientinterval", "gradient + interval", geom_name = "slabinterval")
#' @description
#' If your graphics device supports it, it is recommended to use this stat
#' with `fill_type = "gradient"` (see the description of that parameter). On R >= 4.2,
#' support for `fill_type = "gradient"` should be auto-detected based on the
#' graphics device you are using.
#' @export
stat_gradientinterval = make_stat(StatGradientinterval, geom = "slabinterval")

StatHistinterval = ggproto("StatHistinterval", StatSlabinterval,
  default_params = defaults(list(
    density = "histogram"
  ), StatSlabinterval$default_params),

  default_slab_type = "histogram"
)
#' @eval rd_slabinterval_shortcut_stat("histinterval", "histogram + interval", geom_name = "slabinterval")
#' @export
stat_histinterval = make_stat(StatHistinterval, geom = "slabinterval")

StatSlab = ggproto("StatSlab", StatSlabinterval,
  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), StatSlabinterval$default_params),

  layer_args = defaults(list(
    show.legend = NA
  ), StatSlabinterval$layer_args),

  hidden_params = union(c(
    "show_slab", "show_point", "show_interval",
    "point_interval", ".width"
  ), StatSlabinterval$hidden_params)
)
StatSlab$default_aes$size = NULL
#' @eval rd_slabinterval_shortcut_stat("slab", "slab (ridge)", geom_name = "slab")
#' @examples
#'
#' # RIDGE PLOTS
#' # "ridge" plots can be created by expanding the slabs to the limits of the plot
#' # (expand = TRUE), allowing the density estimator to be nonzero outside the
#' # limits of the data (trim = FALSE), and increasing the height of the slabs.
#' data.frame(
#'   group = letters[1:3],
#'   value = rnorm(3000, 3:1)
#' ) %>%
#'   ggplot(aes(y = group, x = value)) +
#'   stat_slab(color = "black", expand = TRUE, trim = FALSE, height = 2)
#' @export
stat_slab = make_stat(StatSlab, geom = "slab")


# helpers -----------------------------------------------------------------

#' translate distribution arguments of the form `arg1` ... `arg9` (or from a
#' list column, `args`) into a single list of arguments
#' @param args list-column of arguments
#' @param ... other columns, including `arg1` ... `arg9`
#' @noRd
args_from_aes = function(args = list(), ...) {
  args_names = names(args)
  if (length(args_names > 0)) {
    named_args_i = nzchar(args_names)
    named_args = args[named_args_i]
    unnamed_args = args[!named_args_i]
  } else {
    named_args = list()
    unnamed_args = args
  }

  dot_args = list(...)
  for (i in 1:9) {
    arg_name = paste0("arg", i)
    if (arg_name %in% names(dot_args)) {
      unnamed_args[[i]] = dot_args[[arg_name]]
    }
  }

  c(unnamed_args, named_args)
}

#' Given the slab_type parameter and a data frame or list containing `pdf` and
#' `cdf` computed variables, return the value of the slab function (the `f`
#' computed variable)
#' @param slab_type string: `"pdf"`, `"cdf"`, `"histogram"`, or `"ccdf"`. If
#' the `slab_type` is not valid, an appropriate error will be raised.
#' @param slab_df a list or data frame containing elements `"pdf"` and `"cdf"`.
#' @noRd
get_slab_function = function(slab_type, slab_df) {
  switch(slab_type,
    histogram = ,
    pdf = slab_df$pdf,
    cdf = slab_df$cdf,
    ccdf = 1 - slab_df$cdf,
    stop0("Unknown `slab_type`: ", deparse0(slab_type), '. Must be "histogram", "pdf", "cdf", or "ccdf"')
  )
}
