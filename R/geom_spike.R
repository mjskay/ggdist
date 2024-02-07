# spike geometry
#
# Author: mjskay
###############################################################################


#' Spike plot (ggplot2 geom)
#'
#' Geometry for drawing "spikes" (optionally with points on them) on top of
#' [geom_slabinterval()] geometries: this geometry understands the scaling and
#' positioning of the `thickness` aesthetic from [geom_slabinterval()], which
#' allows you to position spikes and points along a slab.
#'
#' @eval rd_layer_params("spike")
#' @eval rd_spike_aesthetics()
#' @details
#' This geometry consists of a "spike" (vertical/horizontal line segment) and a
#' "point" (at the end of the line segment). It uses the `thickness` aesthetic
#' to determine where the endpoint of the line is, which allows it to be used
#' with [geom_slabinterval()] geometries for labeling specific values of the
#' thickness function.
#' @inheritParams geom_slabinterval
#' @return A [ggplot2::Geom] representing a spike geometry which can
#' be added to a [ggplot()] object.
#' rd_slabinterval_aesthetics(geom_name),
#' @seealso
#' See [stat_spike()] for the stat version, intended for
#' use on sample data or analytical distributions.
#' @family slabinterval geoms
#' @examples
#' library(ggplot2)
#' library(distributional)
#' library(dplyr)
#'
#' # geom_spike is easiest to use with distributional or
#' # posterior::rvar objects
#' df = tibble(
#'   d = dist_normal(1:2, 1:2), g = c("a", "b")
#' )
#'
#' # annotate the density at the mean of a distribution
#' df %>% mutate(
#'   mean = mean(d),
#'   density(d, list(density_at_mean = mean))
#' ) %>%
#'   ggplot(aes(y = g)) +
#'   stat_slab(aes(xdist = d)) +
#'   geom_spike(aes(x = mean, thickness = density_at_mean)) +
#'   # need shared thickness scale so that stat_slab and geom_spike line up
#'   scale_thickness_shared()
#'
#' # annotate the endpoints of intervals of a distribution
#' # here we'll use an arrow instead of a point by setting size = 0
#' arrow_spec = arrow(angle = 45, type = "closed", length = unit(4, "pt"))
#' df %>% mutate(
#'   median_qi(d, .width = 0.9),
#'   density(d, list(density_lower = .lower, density_upper = .upper))
#' ) %>%
#'   ggplot(aes(y = g)) +
#'   stat_halfeye(aes(xdist = d), .width = 0.9, color = "gray35") +
#'   geom_spike(
#'     aes(x = .lower, thickness = density_lower),
#'     size = 0, arrow = arrow_spec, color = "blue", linewidth = 0.75
#'   ) +
#'   geom_spike(
#'     aes(x = .upper, thickness = density_upper),
#'     size = 0, arrow = arrow_spec, color = "red", linewidth = 0.75
#'   ) +
#'   scale_thickness_shared()
#'
#' @name geom_spike
NULL


# drawing functions -------------------------------------------------------

draw_slabs_spike = function(self, s_data, panel_params, coord,
  orientation, normalize, na.rm,
  arrow = NULL,
  ...
) {
  define_orientation_variables(orientation)

  # remove missing values - unlike slabinterval, thickness NAs not allowed here
  s_data = ggplot2::remove_missing(s_data, na.rm, "thickness", name = "geom_spike")

  subguide_params = NULL
  c(s_data, subguide_params) %<-% rescale_slab_thickness(
    s_data, orientation, normalize, na.rm, name = "geom_spike"
  )
  s_data = self$override_slab_aesthetics(s_data)

  s_data[[xend]] = s_data[[x]]
  s_data[[y]] = case_when_side(s_data$side, orientation,
    topright = s_data[[ymin]],
    bottomleft = s_data[[ymax]],
    both = s_data[[ymin]]
  )
  s_data[[yend]] = case_when_side(s_data$side, orientation,
    topright = s_data[[ymax]],
    bottomleft = s_data[[ymin]],
    both = s_data[[ymax]]
  )

  p_data = s_data
  p_data[[y]] = s_data[[yend]]
  p_data = p_data[!is.na(p_data$size) & p_data$size != 0, ]

  list(
    GeomSegment$draw_panel(s_data, panel_params, coord, arrow = arrow),
    if (nrow(p_data) > 0) GeomPoint$draw_panel(p_data, panel_params, coord)
  )
}


# geom_spike --------------------------------------------------------------

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomSpike = ggproto("GeomSpike", GeomSlab,

  ## aesthetics --------------------------------------------------------------

  aes_docs = {
    aes_docs = GeomSlab$aes_docs
    spike_aes_i = which(names(aes_docs) == "Slab-specific aesthetics")
    names(aes_docs)[[spike_aes_i]] = "Spike-specific (aka Slab-specific) aesthetics"
    modifyList(aes_docs, list(
      "Color aesthetics" = list(
        colour = '(or `color`) The color of the **spike** and **point** sub-geometries.',
        fill = 'The fill color of the **point** sub-geometry.',
        alpha = 'The opacity of the **spike** and **point** sub-geometries.'
      ),
      "Line aesthetics" = list(
        linewidth = 'Width of the line used to draw the **spike** sub-geometry.',
        size = 'Size of the **point** sub-geometry.',
        linetype = 'Type of line (e.g., `"solid"`, `"dashed"`, etc) used to draw the **spike**.'
      )
    ))
  },

  default_key_aes = defaults(aes(
    linewidth = 0.5,
    size = 1.5,
    colour = "black"
  ), GeomSlab$default_key_aes),

  hidden_aes = union(c(
    "justification",
    "slab_fill", "slab_colour", "slab_alpha", "slab_linewidth", "slab_linetype", "slab_size"
  ), GeomSlab$hidden_aes),

  override_slab_aesthetics = function(self, s_data) {
    s_data$colour = apply_colour_ramp(s_data[["colour"]], s_data[["colour_ramp"]])
    s_data$fill = apply_colour_ramp(s_data[["fill"]], s_data[["fill_ramp"]])
    s_data
  },

  rename_size = FALSE,


  ## params ------------------------------------------------------------------

  param_docs = defaults(list(
    # SLAB PARAMS
    arrow = '[grid::arrow()] giving the arrow heads to use on the spike, or `NULL` for no arrows.'
  ), GeomSlab$param_docs),

  hidden_params = setdiff(
    union("fill_type", GeomSlab$hidden_params),
    "arrow"
  ),


  ## other methods -----------------------------------------------------------

  draw_key_slab = function(self, data, key_data, params, size) {
    s_key_data = self$override_slab_aesthetics(key_data)

    show_spike_when_present = c("colour", "colour_ramp", "alpha", "linewidth", "linetype")
    spike_key = if (!all(is.na(data[show_spike_when_present]))) {
      line_key = if (params$orientation %in% c("y", "horizontal")) {
        draw_key_vpath
      } else {
        draw_key_path
      }
      line_key(s_key_data, params, size)
    }

    point_key = if (
      !all(is.na(s_key_data$size) | s_key_data$size == 0) && (
        !all(is.na(data[c("size", "stroke", "shape", "alpha")])) ||
        # only draw point for `fill` aesthetic if a shape that has a fill colour is used
        (!all(is.na(data[c("fill", "fill_ramp")])) && length(intersect(data$shape, 21:25)) > 0) ||
        (!all(is.na(data[c("fill", "fill_ramp")])) && length(intersect(data$shape, 21:25)) > 0)
      )
    ) {
      draw_key_point(s_key_data, params, size)
    }

    grobTree(spike_key, point_key)
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
