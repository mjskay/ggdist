# Shotcut geom for slab (ridge) plots
#
# Author: mjskay
###############################################################################


#' @eval rd_slabinterval_shortcut_geom("slab", "slab (ridge)")
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' theme_set(theme_ggdist())
#'
#' # we will manually demonstrate plotting a density with geom_slab(),
#' # though generally speaking this is easier to do using stat_slab(), which
#' # will determine sensible limits automatically and correctly adjust
#' # densities when using scale transformations
#' df = expand.grid(
#'     mean = 1:3,
#'     input = seq(-2, 6, length.out = 100)
#'   ) %>%
#'   mutate(
#'     group = letters[4 - mean],
#'     density = dnorm(input, mean, 1)
#'   )
#'
#' # orientation is detected automatically based on
#' # use of x or y
#' df %>%
#'   ggplot(aes(y = group, x = input, thickness = density)) +
#'   geom_slab()
#'
#' df %>%
#'   ggplot(aes(x = group, y = input, thickness = density)) +
#'   geom_slab()
#'
#' # RIDGE PLOTS
#' # "ridge" plots can be created by increasing the slab height and
#' # setting the slab color
#' df %>%
#'   ggplot(aes(y = group, x = input, thickness = density)) +
#'   geom_slab(height = 2, color = "black")
#'
#' @name geom_slab
NULL

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomSlab = ggproto("GeomSlab", GeomSlabinterval,
  default_key_aes = defaults(aes(
    linewidth = 1,
    colour = NA
  ), GeomSlabinterval$default_key_aes),

  hidden_aes = union(c(
    "datatype",
    "point_colour", "point_fill", "point_alpha", "point_size", "shape",
    "xmin", "xmax", "ymin", "ymax",
    "interval_colour", "interval_alpha", "interval_size", "interval_linetype"
  ), GeomSlabinterval$hidden_aes),

  rename_size = TRUE,

  override_slab_aesthetics = function(self, s_data) {
    # we define these differently from geom_slabinterval to make this easier to use on its own
    s_data$colour = s_data[["slab_colour"]] %||% s_data[["colour"]]
    s_data$colour = apply_colour_ramp(s_data[["colour"]], s_data[["colour_ramp"]])
    s_data$fill = s_data[["slab_fill"]] %||% s_data[["fill"]]
    s_data$fill = apply_colour_ramp(s_data[["fill"]], s_data[["fill_ramp"]])
    s_data$alpha = s_data[["slab_alpha"]] %||% s_data[["alpha"]]
    s_data$linewidth = s_data[["slab_linewidth"]] %||% s_data[["slab_size"]] %||% s_data[["linewidth"]] %||% s_data[["size"]]
    s_data$linetype = s_data[["slab_linetype"]] %||% s_data[["linetype"]]
    s_data
  },

  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), GeomSlabinterval$default_params),

  hidden_params = union(c(
    "show_slab", "show_point", "show_interval",
    "interval_size_domain", "interval_size_range", "fatten_point", "arrow"
  ), GeomSlabinterval$hidden_params),

  draw_key_slab = function(self, data, key_data, params, size) {
    # can drop all the complicated checks from this key since it's just one geom
    s_key_data = self$override_slab_aesthetics(key_data)

    # what point calls "stroke" is what we call "size", since "size" is determined automatically
    if (is.na(data$colour) && (!is.na(data$size) || !is.na(data$linetype))) {
      # because the default colour is NA, if we want to draw a key for size / linetype we need to
      # reset the colour to something reasonable
      s_key_data$colour = "black"
    }
    draw_key_polygon(s_key_data, params, size)
  }
)
# have to unset these here because defaults() does not treat NULLs as unsetting values
GeomSlab$default_key_aes$slab_colour = NULL
GeomSlab$default_key_aes$slab_size = NULL

#' @rdname geom_slab
#' @export
geom_slab = make_geom(GeomSlab)
