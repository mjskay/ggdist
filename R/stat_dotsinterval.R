# stats for dotsintervals
#
# Author: mjskay
###############################################################################


# slab function for samples -------------------------

#' @importFrom stats ppoints
compute_slab_dots_sample = function(
  self, data, trans, input,
  quantiles = NA, orientation = NA, na.rm = FALSE,
  ...
) {
  x = switch(orientation,
    y = ,
    horizontal = "x",
    x = ,
    vertical = "y"
  )

  if (is.null(quantiles) || is.na(quantiles)) {
    input = data[[x]]
  } else {
    # ppoints() with a = 1/2 corresponds to quantile() with type = 5
    # and ensures that if quantiles == length(data[[x]]) then input == data[[x]]
    input = quantile(data[[x]], ppoints(quantiles, a = 1/2), type = 5, na.rm = na.rm)
  }

  data.frame(
    .input = trans$inverse(input),
    .value = 1,
    n = length(input)
  )
}


# slab functions for distributions -------------------------

#' @importFrom stats ppoints
compute_slab_dots_dist = function(
  self, data, trans, input,
  quantiles = 100,
  ...
) {
  pmap_dfr_(data, function(dist, ...) {
    if (is.null(dist) || anyNA(dist)) {
      return(data.frame(.input = NA, .value = NA))
    }

    args = c(
      list(ppoints(quantiles, a = 1/2)),
      args_from_aes(...)
    )
    quantile_fun = distr_quantile(dist)
    input = do.call(quantile_fun, args)

    data.frame(
      .input = input,
      .value = 1,
      n = length(input)
    )
  })
}


# stat_dotsinterval ------------------------------------------------

StatDotsinterval = ggproto("StatDotsinterval", StatSlabinterval,
  default_params = defaults(list(
    quantiles = NA,
    point_interval = "median_qi"
  ), StatSlabinterval$default_params),

  hidden_params = union(c(
    "limits", "n"
  ), StatSlabinterval$hidden_params),

  compute_slab = compute_slab_dots_sample
)
#' @rdname geom_dotsinterval
#' @export
stat_dotsinterval = make_stat(StatDotsinterval, geom = "dotsinterval")

StatDots = ggproto("StatDots", StatDotsinterval,
  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), StatDotsinterval$default_params),

  layer_args = defaults(list(
    show.legend = NA
  ), StatDotsinterval$layer_args),

  hidden_params = union(c(
    "show_slab", "show_point", "show_interval",
    "point_interval", ".width"
  ), StatDotsinterval$hidden_params)
)
StatDots$default_aes$size = NULL
#' @rdname geom_dotsinterval
#' @export
stat_dots = make_stat(StatDots, geom = "dots")


# stat_dist_dotsinterval -----------------------------------------------

StatDistDotsinterval = ggproto("StatDistDotsinterval", StatDistSlabinterval,
  default_params = defaults(list(
    quantiles = 100
  ), StatDistSlabinterval$default_params),

  hidden_params = union(c(
    "limits", "n",
    "p_limits", "slab_type", "outline_bars",
    "adjust", "trim", "expand", "breaks"
  ), StatSlabinterval$hidden_params),

  compute_slab = compute_slab_dots_dist
)
#' @rdname geom_dotsinterval
#' @export
stat_dist_dotsinterval = make_stat(StatDistDotsinterval, geom = "dotsinterval")

StatDistDots = ggproto("StatDistDots", StatDistDotsinterval,
  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), StatDistDotsinterval$default_params),

  layer_args = defaults(list(
    show.legend = NA
  ), StatDistDotsinterval$layer_args),

  hidden_params = union(c(
    "show_slab", "show_point", "show_interval",
    "point_interval", ".width"
  ), StatDistDotsinterval$hidden_params)
)
StatDistDots$default_aes$size = NULL
#' @rdname geom_dotsinterval
#' @export
stat_dist_dots = make_stat(StatDistDots, geom = "dots")
