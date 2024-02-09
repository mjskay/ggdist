# Key glyph for geom_slabinterval and related geoms
#
# Author: mjskay
###############################################################################

#' Key glyph for geom_slabinterval and related geoms
#'
#' Glyph drawing function for use with [geom_slabinterval()]. Automatically determines
#' what portions of the key to draw based on what aesthetics are actually mapped.
#'
#' @inheritParams ggplot2::draw_key
#' @param self A geom
#' @author Matthew Kay
#' @seealso See [geom_slabinterval()].
#' @keywords internal
#' @noRd
draw_key_slabinterval_ = function(self, data, params, size) {
  k = prepare_key_data_(self, data, params, size)
  data = k$data
  key_data = k$key_data
  params = k$params

  slab_grob = if (params$show_slab) self$draw_key_slab(data, key_data, params, size)
  interval_grob = if (params$show_interval) self$draw_key_interval(data, key_data, params, size)
  point_grob = if (params$show_point) self$draw_key_point(data, key_data, params, size)

  grobTree(slab_grob, interval_grob, point_grob)
}



# keys for sub-parts of the geom ------------------------------------------

draw_key_slab_ = function(self, data, key_data, params, size) {
  # size is not in this list because if size it set but colour is not then there's nothing to draw,
  # so use size can only occur in cases where colour is alos set (so we can just check colour)
  show_slab_when_present = c(
    "fill", "fill_ramp", "slab_fill",
    "alpha", "slab_alpha",
    "slab_colour",
    "slab_linewidth", "slab_size", "slab_linetype"
  )
  if (params$show_slab && !all(is.na(data[show_slab_when_present]))) {
    s_key_data = self$override_slab_aesthetics(key_data)

    if (!all(is.na(data[c("slab_linewidth", "slab_size", "slab_linetype")])) && is.na(s_key_data$colour)) {
      # because the default slab_colour is NA, if we want to draw a key for size / linetype
      # we need to reset the colour to its default
      s_key_data$colour = self$default_key_aes$colour
    }
    draw_key_polygon(s_key_data, params, size)
  }
}

draw_key_interval_ = function(self, data, key_data, params, size) {
  show_interval_when_present = c(
    "colour", "colour_ramp", "interval_colour",
    "alpha", "interval_alpha",
    "size", "linewidth", "linetype", "interval_size", "interval_linetype"
  )
  if (params$show_interval && !all(is.na(data[show_interval_when_present]))) {
    i_key_data = self$override_interval_aesthetics(key_data, params$interval_size_domain, params$interval_size_range)
    line_key = if (params$orientation %in% c("y", "horizontal")) {
      draw_key_path
    } else {
      draw_key_vpath
    }
    line_key(i_key_data, params, size)
  }
}

draw_key_point_ = function(self, data, key_data, params, size) {
  show_point_when_present = c(
    "shape",
    "stroke",
    "colour", "colour_ramp", "point_colour", "point_fill",
    "alpha", "point_alpha",
    "size", "point_size"
  )
  if (
    params$show_point && (
      !all(is.na(data[show_point_when_present])) ||
        # only draw point for `fill` aesthetic if a shape that has a fill colour is used
        (!is.na(data$fill) && length(intersect(data$shape, 21:25)) > 0) ||
        (!is.na(data$point_fill) && length(intersect(data$shape, 21:25)) > 0)
    )
  ) {
    p_key_data = self$override_point_aesthetics(
      key_data, params$interval_size_domain, params$interval_size_range, params$fatten_point
    )
    draw_key_point(p_key_data, params, size)
  }
}

# helpers for slabinterval keys -------------------------------------------

# prepares data used to generate keys. returns a list with:
# - key_data: data with default NULL values overriden with the defaults for the key
# - data: data with default NULL (or missing) values overriden with NA
# - params: params with default NULL values overriden with default params for geom
prepare_key_data_ = function(self, data, params, size) {
  # we will compute two datasets: one used for determining which keys to draw (data)
  # and the other to actually draw them if they are present (key_data)
  key_data = data
  for (aesthetic in names(self$default_key_aes)) {
    key_data[[aesthetic]] = key_data[[aesthetic]] %||% self$default_key_aes[[aesthetic]]
  }
  for (aesthetic in self$aesthetics()) {
    data[[aesthetic]] = data[[aesthetic]] %||% NA
  }

  list(
    key_data = key_data,
    data = data,
    params = defaults(params, self$default_params)
  )
}
