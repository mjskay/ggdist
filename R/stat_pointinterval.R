# A stat_summary with a geom_pointinterval
#
# Author: mjskay
###############################################################################


#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPointinterval = ggproto("StatPointinterval", StatSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval"
  ), StatSlabinterval$default_aes),

  default_params = defaults(list(
    show_slab = FALSE
  ), StatSlabinterval$default_params),

  hidden_params = union(c(
    "density", "adjust", "trim", "expand", "breaks", "align", "outline_bars", "limits", "n", "p_limits"
  ), StatSlabinterval$hidden_params)
)

#' @eval rd_slabinterval_shortcut_stat("pointinterval", "point + multiple-interval")
#' @export
stat_pointinterval = make_stat(StatPointinterval, geom = "pointinterval")
