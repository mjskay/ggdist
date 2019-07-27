# Distribution + interval stat for analytical distributions
#
# Author: mjskay
###############################################################################


#' Distribution + interval plots (eye plots, half-eye plots, CCDF barplots, etc) for analytical distributions (ggplot stat)
#'
#' Stats for computing distribution functions (densities or CDFs) + intervals for use with
#' \code{\link{geom_slabinterval}}. Uses \code{dist} aesthetic to specify a distribution name
#' and \code{arg1}, ... \code{arg9} aesthetics (or \code{args} as a list column) to specify distribution
#' arguments.
#'
#' @inheritParams stat_slabinterval
#' @param p_limits Probability limits (as a vector of size 2) used to determine the lower and upper
#' limits of the slab. E.g., if this is \code{c(.001, .999)} (the default), then a slab is drawn
#' for the distribution from the quantile at \code{p = .001} to the quantile at \code{p = .999}.
#' @param limits Manually-specified limits for the slab, as a vector of length two. These limits are combined with those
#' computed based on \code{p_limits} as well as the limits defined by the scales of the plot to determine the
#' limits used to draw the slab functions: these limits specify the maximal limits; i.e., if specified, the limits
#' will not be wider than these (but may be narrower).Use \code{NA} to leave a limit alone; e.g.
#' \code{limits = c(0, NA)} will ensure that the lower limit does not go below 0, but let the upper limit
#' be determined by either \code{p_limits} or the scale settings.
#' @seealso See \code{\link{geom_slabinterval}} for more information of the geom this stat
#' uses by default and some of the options it has.
#' @examples
#'
#' #TODO
#'
#' @importFrom rlang as_function
#' @importFrom dplyr bind_rows
#' @keywords internal
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

      na.rm = na.rm,
      ...
    )
  )
}

StatDistSlabinterval <- ggproto("StatDistSlabinterval", StatSlabinterval,
  optional_aes = c(
    "dist",
    "args",
    paste0("arg", 1:9)
  ),

  aesthetics = function(self) {
    # for some reason ggplot2::Stat doesn't obey optional_aes the way Geoms do,
    # so we'll implement that ourselves
    union(self$optional_aes, ggproto_parent(StatSlabinterval, self)$aesthetics())
  },

  extra_params = c(
    StatSlabinterval$extra_params,
    "slab_type",
    "p_limits"
  ),

  setup_params = function(self, data, params) {
    params = ggproto_parent(StatSlabinterval, self)$setup_params(data, params)

    params$limits_args = list(
      p_limits = params$p_limits %||% c(.001, .999)
    )

    params$slab_args = list(
      slab_type = params$slab_type %||% "pdf"
    )

    params
  },

  setup_data = function(self, data, params) {
    data = ggproto_parent(StatSlabinterval, self)$setup_data(data, params)

    # need to treat dist *and* args as grouping variables (else things will break)
    group_cols = intersect(c("dist", "args", paste0("arg", 1:9), "group"), names(data))
    data$group = as.numeric(interaction(data[,group_cols]))

    data
  }
)

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
stat_dist_ccdfbar = function(...,
  slab_type = "ccdf", justification = 0.5, side = "left", normalize = "none"
) {
  stat_dist_slabinterval(..., slab_type = slab_type, justification = justification, side = side, normalize = normalize)
}
#' @export
#' @rdname stat_dist_slabinterval
stat_dist_ccdfbarh = function(...,
  slab_type = "ccdf", justification = 0.5, side = "top", orientation = "horizontal", normalize = "none"
) {
  stat_dist_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @export
#' @rdname stat_dist_slabinterval
stat_dist_cdfbar = function(...,
  slab_type = "cdf", justification = 0.5, side = "left", normalize = "none"
) {
  stat_dist_slabinterval(..., slab_type = slab_type, justification = justification, side = side, normalize = normalize)
}
#' @export
#' @rdname stat_dist_slabinterval
stat_dist_cdfbarh = function(...,
  slab_type = "cdf", justification = 0.5, side = "top", orientation = "horizontal", normalize = "none"
) {
  stat_dist_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}


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

dist_slab_function = function(
  df, input, slab_type = "pdf", limits = NULL, n = 201, ...
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
