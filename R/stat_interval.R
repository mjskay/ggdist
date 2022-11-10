# A stat_summary with a geom_interval
#
# Author: mjskay
###############################################################################


#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatInterval = ggproto("StatInterval", StatPointinterval,
  default_aes = defaults(aes(
    color = after_stat(level)
  ), StatPointinterval$default_aes),

  default_params = defaults(list(
    show_point = FALSE,
    .width = c(.50, .80, .95)
  ), StatPointinterval$default_params),

  layer_args = defaults(list(
    show.legend = NA
  ), StatPointinterval$layer_args)
)
# have to remove this here instead of in call to defaults()
# because otherwise it stays in the list as a value = NULL
# instead of being removed
StatInterval$default_aes$size = NULL

#' @eval rd_slabinterval_shortcut_stat("interval", "multiple-interval", example_layers = "scale_color_brewer()")
#' @export
stat_interval = make_stat(StatInterval, geom = "interval")
