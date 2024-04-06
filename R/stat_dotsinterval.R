# stats for dotsintervals
#
# Author: mjskay
###############################################################################


# compute_slab ------------------------------------------------------------

#' StatDotsinterval$compute_slab()
#' @importFrom stats ppoints
#' @noRd
compute_slab_dots = function(
  self, data, scales, trans, input, orientation,
  quantiles,
  na.rm,
  ...,
  compute_mcse = FALSE
) {
  define_orientation_variables(orientation)

  dist = data$dist
  if (distr_is_missing(dist)) {
    return(data_frame0(.input = NA_real_, f = NA_real_, n = NA_integer_))
  }

  quantiles = quantiles %||% NA
  quantiles_provided = !isTRUE(is.na(quantiles))

  map_character = if (distr_is_factor_like(dist) && !is.null(scales[[x]]) && scales[[x]]$is_discrete()) {
    # character or factor-like values need to be mapped back through the scale
    scales[[x]]$map
  } else {
    identity
  }

  if (distr_is_sample(dist)) {
    .sample = map_character(distr_get_sample(dist))
    .weights = distr_get_sample_weights(dist)
    if (quantiles_provided) {
      # ppoints() with a = 1/2 corresponds to quantile() with type = 5
      # (on continuous samples --- on discrete, we use type = 1)
      # and ensures that if quantiles == length(.sample) then input == .sample
      quantile_type = if (distr_is_discrete(dist)) 1 else 5
      probs = ppoints(quantiles, a = 0.5)
      input = weighted_quantile(
        .sample, probs, type = quantile_type, na.rm = na.rm, weights = .weights, names = FALSE
      )
      if (compute_mcse) {
        stop_if_not_installed("posterior", "{.help ggdist::stat_mcse_dots}")
        se = posterior::mcse_quantile(.sample, probs, names = FALSE)
      }
    } else {
      input = sort(.sample)
      if (compute_mcse) {
        stop_if_not_installed("posterior", "{.help ggdist::stat_mcse_dots}")
        se = posterior::mcse_quantile(.sample, ppoints(length(input), a = 0.5), names = FALSE)
      }
    }
  } else {
    dist_quantiles = if (quantiles_provided) quantiles else 100
    dist_probs = ppoints(dist_quantiles, a = 1/2)
    quantile_fun = distr_quantile(dist, categorical_okay = TRUE)
    input = map_character(quantile_fun(dist_probs))
    se = 0
  }

  out = data_frame0(
    .input = input,
    f = 1,
    n = length(input)
  )
  if (compute_mcse) out$se = se
  out
}


# stat_dotsinterval ------------------------------------------------

StatDotsinterval = ggproto("StatDotsinterval", StatSlabinterval,
  default_params = defaults(list(
    quantiles = NA
  ), StatSlabinterval$default_params),

  hidden_params = union(c(
    "limits", "n",
    "p_limits", "outline_bars",
    "density", "adjust", "trim", "expand", "breaks", "align"
  ), StatSlabinterval$hidden_params),

  # workaround (#84)
  compute_slab = function(self, ...) compute_slab_dots(self, ...)
)

#' @eval rd_dotsinterval_shortcut_stat("dotsinterval", "dots + point + interval")
#' @inheritParams stat_slabinterval
#' @param quantiles Setting this to a value other than `NA`
#' will produce a quantile dotplot: that is, a dotplot of quantiles from the sample or distribution
#' (for analytical distributions, the default of `NA` is taken to mean `100` quantiles). The value of
#' `quantiles` determines the number
#' of quantiles to plot. See Kay et al. (2016) and Fernandes et al. (2018) for more information on quantile dotplots.
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
    "point_interval", ".width"
  ), StatDotsinterval$hidden_params)
)
StatDots$default_aes$size = NULL

#' @eval rd_dotsinterval_shortcut_stat("dots", "dot")
#' @export
stat_dots = make_stat(StatDots, geom = "dots")
