# stats for dotsintervals
#
# Author: mjskay
###############################################################################


# slab function for samples -------------------------

#' @importFrom stats ppoints
dots_sample_slab_function = function(
  df, input, limits = NULL, quantiles = NA, orientation = "vertical",
  trans = scales::identity_trans(), ...
) {
  x = switch(orientation,
    horizontal = "x",
    vertical = "y"
  )

  if (is.null(quantiles) || is.na(quantiles)) {
    input = df[[x]]
  } else {
    # ppoints() with a = 1/2 corresponds to quantile() with type = 5
    # and ensures that if quantiles == length(df[[x]]) then input == df[[x]]
    input = quantile(df[[x]], ppoints(quantiles, a = 1/2), type = 5)
  }

  data.frame(
    .input = trans$inverse(input),
    .value = 1,
    n = length(input)
  )
}


# slab functions for distributions -------------------------

#' @importFrom stats ppoints
#' @importFrom purrr pmap_dfr
dots_dist_slab_function = function(
  df, input, quantiles = 100, trans = scales::identity_trans(), ...
) {
  pmap_dfr(df, function(dist, ...) {
    if (is.na(dist)) {
      return(data.frame(.input = NA, .value = NA))
    }

    args = c(
      list(p = ppoints(quantiles, a = 1/2)),
      args_from_aes(...)
    )
    quantile_fun = match.fun(paste0("q", dist))
    input = do.call(quantile_fun, args)

    data.frame(
      .input = input,
      .value = 1,
      n = length(input)
    )
  })
}


# stat_dotsinterval[h] ------------------------------------------------

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

      slab_function = dots_sample_slab_function,
      slab_args = list(),

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

    slab_function = dots_sample_slab_function,
    point_interval = median_qi
  ), StatSlabinterval$default_params),

  setup_params = function(self, data, params) {
    params = ggproto_parent(StatSlabinterval, self)$setup_params(data, params)

    params$slab_args = list(
      quantiles = params$quantiles %||% self$default_params$quantiles
    )

    params
  }
)
#' @export
#' @rdname geom_dotsinterval
stat_dotsintervalh = function(..., orientation = "horizontal") {
  stat_dotsinterval(..., orientation = orientation)
}

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
#' @export
#' @rdname geom_dotsinterval
stat_dotsh = function(..., orientation = "horizontal") {
  stat_dots(..., orientation = orientation)
}


# stat_dist_dotsinterval[h] -----------------------------------------------

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

      limits_function = NULL,

      slab_function = dots_dist_slab_function,
      slab_args = list(),

      interval_function = dist_interval_function,
      interval_args = list(),
      point_interval = NULL,

      na.rm = na.rm,
      ...
    )
  )
}
#' @importFrom plyr defaults
StatDistDotsinterval = ggproto("StatDistDotsinterval", StatDistSlabinterval,
  extra_params = c(
    StatDistSlabinterval$extra_params,
    "quantiles"
  ),

  default_params = defaults(list(
    quantiles = 100,

    limits_function = NULL,
    slab_function = dots_dist_slab_function,
    interval_function = dist_interval_function
  ), StatDistSlabinterval$default_params),

  setup_params = function(self, data, params) {
    params = ggproto_parent(StatSlabinterval, self)$setup_params(data, params)

    params$slab_args = list(
      quantiles = params$quantiles %||% self$default_params$quantiles
    )

    params
  }
)
#' @export
#' @rdname geom_dotsinterval
stat_dist_dotsintervalh = function(..., orientation = "horizontal") {
  stat_dist_dotsinterval(..., orientation = orientation)
}

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
#' @export
#' @rdname geom_dotsinterval
stat_dist_dotsh = function(..., orientation = "horizontal") {
  stat_dist_dots(..., orientation = orientation)
}


