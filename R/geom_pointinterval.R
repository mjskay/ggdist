# Like a geom_pointrange but with sensible defaults for displaying multiple intervals
#
# Author: mjskay
###############################################################################


# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".width"))


#' @eval rd_slabinterval_shortcut_geom("pointinterval", "point + multiple-interval")
#' @param show.legend Should this layer be included in the legends? Default is `c(size = FALSE)`, unlike most geoms,
#' to match its common use cases. `FALSE` hides all legends, `TRUE` shows all legends, and `NA` shows only
#' those that are mapped (the default for most geoms).
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' data(RankCorr_u_tau, package = "ggdist")
#'
#' # orientation is detected automatically based on
#' # use of xmin/xmax or ymin/ymax
#'
#' RankCorr_u_tau %>%
#'   group_by(i) %>%
#'   median_qi(.width = c(.8, .95)) %>%
#'   ggplot(aes(y = i, x = u_tau, xmin = .lower, xmax = .upper)) +
#'   geom_pointinterval()
#'
#' RankCorr_u_tau %>%
#'   group_by(i) %>%
#'   median_qi(.width = c(.8, .95)) %>%
#'   ggplot(aes(x = i, y = u_tau, ymin = .lower, ymax = .upper)) +
#'   geom_pointinterval()
#'
#' @import ggplot2
#' @name geom_pointinterval
NULL

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomPointinterval = ggproto("GeomPointinterval", GeomSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval",
    side = "both"
  ), GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    fill = NA
  ), GeomSlabinterval$default_key_aes),

  default_computed_aes = defaults(aes(
    size = -.width
  ), GeomSlabinterval$default_computed_aes),

  default_params = defaults(list(
    orientation = NA,
    show_slab = FALSE
  ), GeomSlabinterval$default_params),

  hidden_params = union(c(
    "show_slab", "show_point", "show_interval",
    "normalize", "fill_type"
  ), GeomSlabinterval$hidden_params),

  layer_args = defaults(list(
    show.legend = c(size = FALSE)
  ), GeomSlabinterval$layer_args)
)

#' @rdname geom_pointinterval
#' @export
geom_pointinterval = make_geom(GeomPointinterval)
