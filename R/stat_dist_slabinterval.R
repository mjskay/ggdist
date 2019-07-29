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

#' @importFrom purrr pmap_dfr
dist_slab_function = function(
  df, input, slab_type = "pdf", limits = NULL, n = 501, ...
) {
  pmap_dfr(df, function(dist, ...) {
    if (is.na(dist)) {
      return(data.frame(.input = NA, .value = NA))
    }

    args = args_from_aes(...)
    dist_fun = switch(slab_type,
      pdf = match.fun(paste0("d", dist)),
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
dist_interval_function = function(df, .width, ...) {
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
        .value = quantiles[[1]],
        .lower = quantiles[[2]],
        .upper = quantiles[[3]],
        .width = w
      )
    })
  })
}


# stat_dist_slabinterval --------------------------------------------------

#' Distribution + interval plots (eye plots, half-eye plots, CCDF barplots, etc) for analytical distributions (ggplot stat)
#'
#' Stats for computing distribution functions (densities or CDFs) + intervals for use with
#' \code{\link{geom_slabinterval}}. Uses \code{dist} aesthetic to specify a distribution name
#' and \code{arg1}, ... \code{arg9} aesthetics (or \code{args} as a list column) to specify distribution
#' arguments.
#'
#' A highly configurable stat for generating a variety of plots that combine a "slab"
#' that describes a distribution plus an interval. Several "shortcut" stats are provided
#' which combine multiple options to create useful geoms, particularly \emph{eye plots}
#' (a combination of a violin plot and interval), \emph{half-eye plots} (a density plus interval),
#' and \emph{CCDF bar plots} (a complementary CDF plus interval).
#'
#' The shortcut stat names follow the pattern \code{stat_dist_[name][h|]}, where the trailing
#' \code{h} (if present) indicates the horizontal version of the stat.
#'
#' Stats include:
#'
#' \itemize{
#'   \item \code{stat_dist_eye} / \code{stat_dist_eyeh}: Eye plots (violin + interval)
#'   \item \code{stat_dist_halfeye} / \code{stat_dist_halfeyeh}: Half-eye plots (density + interval)
#'   \item \code{stat_dist_ccdfinterval} / \code{stat_dist_ccdfintervalh}: CCDF bar plots (CCDF + interval)
#'   \item \code{stat_dist_cdfinterval} / \code{stat_dist_cdfintervalh}: CDF bar plots (CDF + interval)
#'   \item \code{stat_dist_gradientinterval} / \code{stat_dist_gradientintervalh}: Density gradient + interval plots
#' }
#'
#' These stats expect a \code{dist} aesthetic to specify a distribution name
#' and \code{arg1}, ... \code{arg9} aesthetics (or \code{args} as a list column) to specify distribution
#' arguments. Distribution names should correspond to R functions that have \code{"p"}, \code{"q"}, and
#' \code{"d"} functions; e.g. \code{"norm"} is a valid distribution name because R defines the
#' \code{\link{pnorm}}, \code{\link{qnorm}}, and \code{\link{dnorm}} functions for Normal distributions.
#'
#' See the \code{\link{parse_dist}} function for a useful way to generate \code{dist} and \code{args}
#' values from human-readable distribution specs (like \code{"normal(0,1)"}). Such specs are also
#' produced by other packages (like the \code{\link[brms]{get_prior}} function in brms); thus,
#' \code{\link{parse_dist}} combined with the stats described here can help you visualize the output
#' of those functions.
#'
#' @inheritParams stat_slabinterval
#' @inheritParams geom_slabinterval
#' @param slab_type The type of slab function to calculate: probability density (or mass) function (\code{"pdf"}),
#' cumulative distribution function (\code{"cdf"}), or complementary CDF (\code{"ccdf"}).
#' @param p_limits Probability limits (as a vector of size 2) used to determine the lower and upper
#' limits of the slab. E.g., if this is \code{c(.001, .999)} (the default), then a slab is drawn
#' for the distribution from the quantile at \code{p = .001} to the quantile at \code{p = .999}.
#' @param limits Manually-specified limits for the slab, as a vector of length two. These limits are combined with those
#' computed based on \code{p_limits} as well as the limits defined by the scales of the plot to determine the
#' limits used to draw the slab functions: these limits specify the maximal limits; i.e., if specified, the limits
#' will not be wider than these (but may be narrower).Use \code{NA} to leave a limit alone; e.g.
#' \code{limits = c(0, NA)} will ensure that the lower limit does not go below 0, but let the upper limit
#' be determined by either \code{p_limits} or the scale settings.
#' @param thickness Override for the \code{thickness} aesthetic in \code{\link{geom_slabinterval}}: the thickness
#' of the slab at each x / y value of the slab (depending on \code{orientation}).
#' @seealso See \code{\link{geom_slabinterval}} for more information on the geom these stats
#' use by default and some of the options they have. See \code{\link{stat_sample_slabinterval}}
#' for the versions of these stats that can be used on samples.
#' @examples
#'
#' #TODO
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
StatDistSlabinterval <- ggproto("StatDistSlabinterval", StatSlabinterval,
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

    # need to treat dist *and* args as grouping variables (else things will break)
    group_cols = intersect(c("dist", "args", paste0("arg", 1:9), "group"), names(data))
    # need to do as.character() here because list columns (as in args) won't work
    # with interaction()
    group_data = lapply(data[,group_cols], as.character)
    data$group = as.numeric(interaction(group_data))

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
  p_limits = c(0.005, 0.995),

  show.legend = c(size = FALSE),
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
      p_limits = p_limits,
      ...
    )
  )
}
#' @export
#' @rdname stat_dist_slabinterval
stat_dist_gradientintervalh = function(..., orientation = "horizontal") {
  stat_dist_gradientinterval(..., orientation = orientation)
}
StatDistGradientinterval <- ggproto("StatDistGradientinterval", StatDistSlabinterval,
  default_aes = defaults(aes(
    alpha = stat(f)
  ), StatDistSlabinterval$default_aes)
)
