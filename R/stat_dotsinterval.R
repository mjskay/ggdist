# stats for dotsintervals
#
# Author: mjskay
###############################################################################


# compute_slab ------------------------------------------------------------

#' StatDotsinterval$compute_slab()
#' @importFrom stats ppoints
#' @noRd
compute_slab_dots = function(
  self, data, trans, input, orientation,
  quantiles,
  na.rm,
  ...
) {
  dist = data$dist
  if (distr_is_missing(dist)) {
    return(data.frame(.input = NA_real_, f = NA_real_, n = NA_integer_))
  }

  quantiles = quantiles %||% NA
  quantiles_provided = !isTRUE(is.na(quantiles))
  dist_quantiles = if (quantiles_provided) quantiles else 100
  probs = ppoints(dist_quantiles, a = 1/2)

  if (distr_is_sample(dist)) {
    input = distr_get_sample(dist)
    if (quantiles_provided) {
      # ppoints() with a = 1/2 corresponds to quantile() with type = 5
      # (on continuous samples --- on discrete, we use type = 1)
      # and ensures that if quantiles == length(data[[x]]) then input == data[[x]]
      quantile_type = if (distr_is_discrete(dist)) 1 else 5
      input = quantile(input, ppoints(quantiles, a = 1/2), type = quantile_type, na.rm = na.rm)
    }
  } else {
    quantile_fun = distr_quantile(dist)
    input = quantile_fun(probs)
  }

  data.frame(
    .input = input,
    f = 1,
    n = length(input)
  )
}


# stat_dotsinterval ------------------------------------------------

StatDotsinterval = ggproto("StatDotsinterval", StatSlabinterval,
  default_params = defaults(list(
    quantiles = NA
  ), StatSlabinterval$default_params),

  hidden_params = union(c(
    "limits", "n",
    "p_limits", "slab_type", "outline_bars",
    "adjust", "trim", "expand", "breaks"
  ), StatSlabinterval$hidden_params),

  # workaround (#84)
  compute_slab = function(self, ...) compute_slab_dots(self, ...)
)
#' @rdname geom_dotsinterval
#' @export
stat_dotsinterval = make_stat(StatDotsinterval, geom = "dotsinterval")


# stat_dots ---------------------------------------------------------------

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
