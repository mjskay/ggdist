# Key glyph for geom_slabinterval and related geoms
#
# Author: mjskay
###############################################################################

#' Key glyph for geom_slabinterval and related geoms
#'
#' Glyph drawing function for use with \code{\link{geom_slabinterval}}. Automatically determines
#' what portions of the key to draw based on what aesthetics are actually mapped.
#'
#' @inheritParams ggplot2::draw_key
#' @param self A geom
#' @author Matthew Kay
#' @seealso See \code{\link{geom_slabinterval}}.
#' @keywords internal
#' @importFrom grid grobTree
draw_key_slabinterval = function(self, data, params, size) {
  # we will compute two datasets: one used for determining which keys to draw (data)
  # and the other to actually draw them if they are present (key_data)
  key_data = data
  for (aesthetic in names(self$default_key_aes)) {
    key_data[[aesthetic]] = key_data[[aesthetic]] %||% self$default_key_aes[[aesthetic]]
    data[[aesthetic]] = data[[aesthetic]] %||% NA
  }

  params = defaults(params, self$default_params)

  line_key = switch(params$orientation,
    horizontal = draw_key_path,
    vertical = draw_key_vpath
  )

  s_data = override_slab_aesthetics(data)
  # size is not in this list because if size it set but colour is not then there's nothing to draw,
  # so use size can only occur in cases where colour is alos set (so we can just check colour)
  slab_grob = if (params$show_slab && any(!is.na(s_data[c("colour","fill","alpha","linetype")]))) {
    s_key_data = override_slab_aesthetics(key_data)
    if (!is.na(s_key_data$size) && !is.na(s_key_data$colour)) {
      draw_key_polygon(s_key_data, params, size)
    } else {
      draw_key_rect(s_key_data, params, size)
    }
  }

  i_data = override_interval_aesthetics(data, params$interval_size_domain, params$interval_size_range)
  interval_grob = if(params$show_interval && any(!is.na(i_data[c("colour","alpha","size","linetype")]))) {
    i_key_data = override_interval_aesthetics(key_data, params$interval_size_domain, params$interval_size_range)
    line_key(i_key_data, params, size)
  }

  p_data = override_point_aesthetics(data, params$interval_size_domain, params$interval_size_range, params$fatten_point)
  point_grob = if(params$show_point && (
      any(!is.na(p_data[c("shape","stroke","colour","alpha","size")])) ||
      # only draw point for `fill` aesthetic if a shape that has a fill colour is used
      (!is.na(p_data$fill) && length(intersect(p_data$shape, c(21:25))) > 0)
  )) {
    p_key_data = override_point_aesthetics(key_data,
      params$interval_size_domain, params$interval_size_range, params$fatten_point)
    draw_key_point(p_key_data, params, size)
  }

  grobTree(slab_grob, interval_grob, point_grob)
}
