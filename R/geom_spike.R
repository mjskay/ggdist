# spike geometry
#
# Author: mjskay
###############################################################################


#' Spike plot (ggplot2 geom)'),
#'
#' Geometry for drawing "spikes" (optionally with points on them) on top of
#' [geom_slabinterval()] geometries: this geometry understands the scaling and
#' positioning of the `thickness` aesthetic from [geom_slabinterval()], which
#' allows you to position spikes and points along a slab.
#'
#' @details
#' This geometry consists of a "spike" (vertical/horizontal line segment) and a
#' "point" (at the end of the line segment). It uses the `thickness` aesthetic
#' to determine where the endpoint of the line is, which allows it to be used
#' with [geom_slabinterval()] geometries for labelling specific values of the
#' thickness function.
#' @inheritParams geom_slabinterval
#' @return A [ggplot2::Geom] representing a spike geometry which can
#' be added to a [ggplot()] object.
#' rd_slabinterval_aesthetics(geom_name),
#' @seealso
#' See [stat_spike()] for the stat version, intended for
#' use on sample data or analytical distributions.
#' @family geom_slabinterval geoms
#' @examples
#'
#' #TODO
#'
#' @name geom_spike
NULL


# drawing functions -------------------------------------------------------

draw_slabs_spike = function(self, s_data, panel_params, coord,
  orientation, normalize, na.rm,
  ...
) {
  define_orientation_variables(orientation)

  s_data = self$override_slab_aesthetics(rescale_slab_thickness(
    s_data, orientation, normalize, height, y, ymin, ymax
  ))

  s_data[[xend]] = s_data[[x]]
  s_data[[y]] = s_data[[ymin]]
  s_data[[yend]] = s_data[[ymax]]
  list(GeomSegment$draw_panel(s_data, panel_params, coord))
}


# geom_spike --------------------------------------------------------------

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomSpike = ggproto("GeomSpike", GeomSlabinterval,
  default_key_aes = defaults(aes(
    size = 0.5,
    colour = "black"
  ), GeomSlabinterval$default_key_aes),

  override_slab_aesthetics = function(self, s_data) {
    s_data$colour = apply_colour_ramp(s_data[["colour"]], s_data[["colour_ramp"]])
    s_data$fill = apply_colour_ramp(s_data[["fill"]], s_data[["fill_ramp"]])
    s_data
  },

  override_point_aesthetics = function(self, p_data) {
    p_data$fill = p_data[["point_fill"]] %||% p_data[["fill"]]
    p_data$fill = apply_colour_ramp(s_data[["fill"]], s_data[["fill_ramp"]])
    ggproto_parent(GeomSlabinterval, self)$override_point_aesthetics(p_data)
  },

  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), GeomSlabinterval$default_params),

  hidden_params = union(c(
    "show_slab", "show_point", "show_interval",
    "interval_size_domain", "interval_size_range"
  ), GeomSlabinterval$hidden_params),

  draw_key_slab = function(self, data, key_data, params, size) {
    #TODO: update
    # can drop all the complicated checks from this key since it's just one geom
    s_key_data = self$override_slab_aesthetics(key_data)

    # what point calls "stroke" is what we call "size", since "size" is determined automatically
    if (is.na(data$colour) && (!is.na(data$size) || !is.na(data$linetype))) {
      # because the default colour is NA, if we want to draw a key for size / linetype we need to
      # reset the colour to something reasonable
      s_key_data$colour = "black"
    }
    draw_key_polygon(s_key_data, params, size)
  },

  # workaround (#84)
  draw_slabs = function(self, ...) draw_slabs_spike(self, ...)
)
# have to unset these here because defaults() does not treat NULLs as unsetting values
GeomSpike$default_key_aes$slab_colour = NULL
GeomSpike$default_key_aes$slab_size = NULL

#' @rdname geom_spike
#' @export
geom_spike = make_geom(GeomSpike)
