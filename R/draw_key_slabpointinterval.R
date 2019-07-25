# Key glyphs for geom_slabinterval and related geoms
#
# Author: mjskay
###############################################################################

make_draw_key_slabpointinterval_ = function(slab_ = TRUE, point_ = TRUE, interval_ = TRUE) {
  function(self, data, params, size) {
    params = modifyList(self$default_params, params)

    s_data = override_slab_aesthetics(data)
    slab_grob = if(slab_ && params$show_slab && (!is.na(s_data$colour) || !is.na(s_data$fill))) {
      draw_key_rect(s_data, params, size)
    }

    i_data = override_interval_aesthetics(data, params$interval_size_domain, params$interval_size_range)
    interval_grob = if(interval_ && params$show_interval && !is.na(i_data$colour)) {
      orientation = params$orientation
      interval_key = switch(orientation,
        horizontal = draw_key_path,
        vertical = draw_key_vpath
      )
      interval_key(i_data, params, size)
    }

    p_data = override_point_aesthetics(data, params$interval_size_domain, params$interval_size_range, params$fatten_point)
    point_grob = if(point_ && params$show_point && !is.na(p_data$colour)) {
      draw_key_point(p_data, params, size)
    }

    gTree(children = gList(slab_grob, interval_grob, point_grob))
  }
}

#' Key glyphs for geom_slabinterval and related geoms
#'
#' Glyph drawing functions for use with \code{\link{geom_slabinterval}}, which draw different combinations
#' of the glyph for the slab, point, and/or interval portion of the geom. Key glyphs can be customized for
#' individual geoms by providing a geom with the \code{key_glyph} argument (see \code{\link{layer}}).
#'
#' The name of these key functions indicates what of the three components of a \code{\link{geom_slabinterval}} it
#' draws in the key glyph: the slab, point, and/or interval.
#'
#' @inheritParams ggplot2::draw_key
#' @param self A geom
#' @author Matthew Kay
#' @seealso See \code{\link{geom_slabinterval}}.
#' @examples
#'
#' # TODO
#'
#' @export
draw_key_slabpointinterval = make_draw_key_slabpointinterval_(TRUE,  TRUE,  TRUE )
#' @rdname draw_key_slabpointinterval
#' @export
draw_key_pointinterval     = make_draw_key_slabpointinterval_(FALSE, TRUE,  TRUE )
#' @rdname draw_key_slabpointinterval
#' @export
draw_key_slabinterval      = make_draw_key_slabpointinterval_(TRUE,  FALSE, TRUE )
#' @rdname draw_key_slabpointinterval
#' @export
draw_key_interval          = make_draw_key_slabpointinterval_(FALSE, FALSE, TRUE )
#' @rdname draw_key_slabpointinterval
#' @export
draw_key_slabpoint         = make_draw_key_slabpointinterval_(TRUE,  TRUE,  FALSE)
# draw_key_point           = make_draw_key_slabpointinterval_(FALSE, TRUE,  FALSE) # in base ggplot
#' @rdname draw_key_slabpointinterval
#' @export
draw_key_slab              = make_draw_key_slabpointinterval_(TRUE,  FALSE, FALSE)
