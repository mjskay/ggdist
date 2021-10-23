# stats for dotsintervals
#
# Author: mjskay
###############################################################################


# slab function for samples -------------------------

#' @importFrom stats ppoints
compute_slab_dots_sample = function(
  data, input, trans,
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
  data, input, trans,
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

#' @rdname geom_dotsinterval
#' @export
stat_dotsinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "dotsinterval",
  position = "identity",
  ...,

  quantiles = NA,
  point_interval = median_qi,

  na.rm = FALSE,

  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDotsinterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      quantiles = quantiles,
      point_interval = point_interval,

      na.rm = na.rm,
      ...
    )
  )
}
StatDotsinterval = ggproto("StatDotsinterval", StatSlabinterval,
  extra_params = c(
    StatSlabinterval$extra_params,
    "quantiles"
  ),

  default_params = defaults(list(
    quantiles = NA,
    point_interval = median_qi
  ), StatSlabinterval$default_params),

  compute_slab = compute_slab_dots_sample
)

#' @export
#' @rdname geom_dotsinterval
stat_dots = function(
  mapping = NULL,
  data = NULL,
  geom = "dots",
  position = "identity",
  ...,

  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDots,
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
StatDots = ggproto("StatDots", StatDotsinterval,
  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), StatDotsinterval$default_params)
)
StatDots$default_aes$size = NULL


# stat_dist_dotsinterval -----------------------------------------------

#' @rdname geom_dotsinterval
#' @export
stat_dist_dotsinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "dotsinterval",
  position = "identity",
  ...,

  quantiles = 100,

  na.rm = FALSE,

  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDistDotsinterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      quantiles = quantiles,

      interval_function = dist_interval_function,
      interval_args = list(),
      point_interval = NULL,

      na.rm = na.rm,
      ...
    )
  )
}
StatDistDotsinterval = ggproto("StatDistDotsinterval", StatDistSlabinterval,
  extra_params = c(
    StatDistSlabinterval$extra_params,
    "quantiles"
  ),

  default_params = defaults(list(
    quantiles = 100,

    interval_function = dist_interval_function
  ), StatDistSlabinterval$default_params),

  compute_slab = compute_slab_dots_dist
)

#' @export
#' @rdname geom_dotsinterval
stat_dist_dots = function(
  mapping = NULL,
  data = NULL,
  geom = "dots",
  position = "identity",
  ...,

  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDistDots,
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
StatDistDots = ggproto("StatDistDots", StatDistDotsinterval,
  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), StatDistDotsinterval$default_params)
)
StatDistDots$default_aes$size = NULL
