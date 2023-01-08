# A multiple-interval geom
#
# Author: mjskay
###############################################################################


# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".width"))


#' @eval rd_slabinterval_shortcut_geom("interval", "multiple-interval")
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' theme_set(theme_ggdist())
#'
#' data(RankCorr_u_tau, package = "ggdist")
#'
#' # orientation is detected automatically based on
#' # use of xmin/xmax or ymin/ymax
#'
#' RankCorr_u_tau %>%
#'   group_by(i) %>%
#'   median_qi(.width = c(.5, .8, .95, .99)) %>%
#'   ggplot(aes(y = i, x = u_tau, xmin = .lower, xmax = .upper)) +
#'   geom_interval() +
#'   scale_color_brewer()
#'
#' RankCorr_u_tau %>%
#'   group_by(i) %>%
#'   median_qi(.width = c(.5, .8, .95, .99)) %>%
#'   ggplot(aes(x = i, y = u_tau, ymin = .lower, ymax = .upper)) +
#'   geom_interval() +
#'   scale_color_brewer()
#'
#' @import ggplot2
#' @name geom_interval
NULL

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomInterval = ggproto("GeomInterval", GeomSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval",
    side = "both"
  ), GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    size = 4,
    fill = NA
  ), GeomSlabinterval$default_key_aes),

  default_computed_aes = defaults(aes(
    color = fct_rev_(ordered(.width))
  ), GeomSlabinterval$default_computed_aes),

  hidden_aes = union(c(
    "datatype",
    "side", "scale", "justification", "thickness",
    "slab_size", "slab_linewidth", "slab_colour", "slab_fill", "slab_alpha", "slab_linetype",
    "point_colour", "point_fill", "point_alpha", "point_size", "shape"
  ), GeomSlabinterval$hidden_aes),

  default_params = defaults(list(
    orientation = NA,
    interval_size_range = c(1, 6),
    show_slab = FALSE,
    show_point = FALSE
  ), GeomSlabinterval$default_params),

  hidden_params = union(c(
    "show_slab", "show_point", "show_interval",
    "normalize", "fill_type",
    "fatten_point"
  ), GeomSlabinterval$hidden_params)
)

#' @rdname geom_interval
#' @export
geom_interval = make_geom(GeomInterval)
