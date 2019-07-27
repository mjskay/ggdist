# Distribution + interval stat for analytical distributions
#
# Author: mjskay
###############################################################################


#' Distribution + interval plots (eye plots, half-eye plots, CCDF barplots, etc) for analytical distributions (ggplot stat)
#'
#' Stats for computing distribution functions (densities or CDFs) + intervals for use with
#' \code{\link{geom_slabinterval}}. Uses \code{dist} aesthetic to specify a distribution name
#' and \code{arg1}, ... \code{arg9} (or \code{args} as a list column) to specify distribution
#' arguments.
#'
#' @inheritParams stat_slabinterval
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
stat_distinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

  orientation = c("vertical", "horizontal"),
  slab_type = c("pdf", "cdf", "ccdf"),
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
    stat = StatDistinterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      slab_type = slab_type,

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

StatDistinterval <- ggproto("StatDistinterval", StatSlabinterval,
  default_aes = aes(
    datatype = "slab",
    size = stat(-.width)
  ),

  setup_params = function(data, params) {
    params$slab_args = list(
      slab_type = params$slab_type %||% "pdf"
    )

    params
  },

  extra_params = c(
    StatSlabinterval$extra_params,
    "slab_type"
  )
)

#' @export
#' @rdname stat_distinterval
stat_dist_halfeye = function(...) stat_distinterval(...)
#' @export
#' @rdname stat_distinterval
stat_dist_halfeyeh = function(..., orientation = "horizontal") stat_distinterval(..., orientation = orientation)
#' @export
#' @rdname stat_distinterval
stat_dist_eye = function(..., side = "both") stat_distinterval(..., side = side)
#' @export
#' @rdname stat_distinterval
stat_dist_eyeh = function(..., side = "both", orientation = "horizontal")
  stat_distinterval(..., side = side, orientation = orientation)
#' @export
#' @rdname stat_distinterval
stat_dist_ccdfbar = function(...,
  slab_type = "ccdf", justification = 0.5, side = "left", normalize = "none"
) {
  stat_distinterval(..., slab_type = slab_type, justification = justification, side = side, normalize = normalize)
}
#' @export
#' @rdname stat_distinterval
stat_dist_ccdfbarh = function(...,
  slab_type = "ccdf", justification = 0.5, side = "top", orientation = "horizontal", normalize = "none"
) {
  stat_distinterval(...,
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
