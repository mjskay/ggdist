# A stat_summary with a geom_lineribbon
#
# Author: mjskay
###############################################################################


StatLineribbon = ggproto("StatLineribbon", StatPointinterval,
  default_aes = defaults(aes(
    datatype = "interval",
    group = stat(level),
    fill = stat(level)
  ), StatPointinterval$default_aes),

  default_params = defaults(list(
    .width = c(.50, .80, .95)
  ), StatPointinterval$default_params),

  layer_args = defaults(list(
    show.legend = NA
  ), StatPointinterval$layer_args),

  orientation_options = defaults(list(
    main_is_orthogonal = NA
  ), StatPointinterval$orientation_options),

  group_by_dist = FALSE
)
# have to remove this here instead of in call to defaults()
# because otherwise it stays in the list as a value = NULL
# instead of being removed
StatLineribbon$default_aes$size = NULL

#' @eval rd_lineribbon_shortcut_stat("lineribbon", "line + multiple-ribbon")
#' @export
stat_lineribbon = make_stat(StatLineribbon, geom = "lineribbon")
