# stats for heapintervals
#
# Author: mjskay
###############################################################################


# slab function for samples -------------------------

#' @importFrom stats ppoints
heap_sample_slab_function = function(
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
heap_dist_slab_function = function(
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


# stat_heapinterval[h] ------------------------------------------------

#' @rdname geom_heapinterval
#' @export
stat_heapinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "heapinterval",
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
    stat = StatHeapinterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      quantiles = quantiles,

      slab_function = heap_sample_slab_function,
      slab_args = list(),

      point_interval = point_interval,

      na.rm = na.rm,
      ...
    )
  )
}
StatHeapinterval = ggproto("StatHeapinterval", StatSlabinterval,
  extra_params = c(
    StatSlabinterval$extra_params,
    "quantiles"
  ),

  default_params = defaults(list(
    quantiles = NA,

    slab_function = heap_sample_slab_function,
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
#' @rdname geom_heapinterval
stat_heapintervalh = function(..., orientation = "horizontal") {
  stat_heapinterval(..., orientation = orientation)
}

#' @export
#' @rdname geom_heapinterval
stat_heap = function(
  mapping = NULL,
  data = NULL,
  geom = "heap",
  position = "identity",
  ...,

  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatHeap,
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
StatHeap = ggproto("StatHeap", StatHeapinterval,
  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), StatHeapinterval$default_params)
)
StatHeap$default_aes$size = NULL
#' @export
#' @rdname geom_heapinterval
stat_heaph = function(..., orientation = "horizontal") {
  stat_heap(..., orientation = orientation)
}


# stat_dist_heapinterval[h] -----------------------------------------------

#' @rdname geom_heapinterval
#' @export
stat_dist_heapinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "heapinterval",
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
    stat = StatDistHeapinterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      quantiles = quantiles,

      limits_function = NULL,

      slab_function = heap_dist_slab_function,
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
StatDistHeapinterval = ggproto("StatDistHeapinterval", StatDistSlabinterval,
  extra_params = c(
    StatDistSlabinterval$extra_params,
    "quantiles"
  ),

  default_params = defaults(list(
    quantiles = 100,

    limits_function = NULL,
    slab_function = heap_dist_slab_function,
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
#' @rdname geom_heapinterval
stat_dist_heapintervalh = function(..., orientation = "horizontal") {
  stat_dist_heapinterval(..., orientation = orientation)
}

#' @export
#' @rdname geom_heapinterval
stat_dist_heap = function(
  mapping = NULL,
  data = NULL,
  geom = "heap",
  position = "identity",
  ...,

  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatDistHeap,
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
StatDistHeap = ggproto("StatDistHeap", StatDistHeapinterval,
  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), StatDistHeapinterval$default_params)
)
StatDistHeap$default_aes$size = NULL
#' @export
#' @rdname geom_heapinterval
stat_dist_heaph = function(..., orientation = "horizontal") {
  stat_dist_heap(..., orientation = orientation)
}


