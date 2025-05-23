# theme_ggdist: Simple ggplot2 theme for ggdist
#
# Author: mjskay
###############################################################################

#' Simple, light ggplot2 theme for ggdist and tidybayes
#'
#' A simple, relatively minimalist ggplot2 theme, and some helper functions to go with it.
#'
#' This is a relatively minimalist ggplot2 theme, intended to be used for making publication-ready plots.
#' It is currently based on [ggplot2::theme_light()].
#'
#' A word of warning: this theme may (and very likely will) change in the future as I tweak it to my taste.
#'
#' [theme_ggdist()] and [theme_tidybayes()] are aliases.
#'
#' @inheritParams ggplot2::theme_light
#' @return A named list in the format of [ggplot2::theme()]
#' @author Matthew Kay
#' @seealso [ggplot2::theme()], [ggplot2::theme_set()]
#' @examples
#'
#' library(ggplot2)
#'
#' theme_set(theme_ggdist())
#'
#' @import ggplot2
#' @export
theme_ggdist = function(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/22,
  base_rect_size = base_size/22
) {
  theme_light(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) + theme(
    axis.line.x = element_line(color = "gray70", linewidth = rel(0.5)),
    axis.line.y = element_line(color = "gray70", linewidth = rel(0.5)),
    axis.title.x = element_text(margin = margin(t = 7)),
    axis.title.y = element_text(margin = margin(r = 7)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    strip.text = element_text(color = "black", margin = margin(6, 6, 6, 6)),
    strip.background = element_rect(fill = "gray90")
  )
}

#' @rdname theme_ggdist
#' @export
theme_tidybayes = theme_ggdist

#' @rdname theme_ggdist
#' @export
facet_title_horizontal = function() {
  theme(
    strip.text.y.right = element_text(angle = 0),
    strip.text.y.left = element_text(angle = 0)
  )
}

#' @rdname theme_ggdist
#' @export
axis_titles_bottom_left = function() {
  theme(
    axis.title.y = element_text(angle = 0, vjust = 0),
    axis.title.x = element_text(hjust = 0)
  )
}

#' @rdname theme_ggdist
#' @export
facet_title_left_horizontal = function() {
  .Deprecated("facet_title_horizontal")  # nocov
  facet_title_horizontal()               # nocov
}

#' @rdname theme_ggdist
#' @export
facet_title_right_horizontal = function() {
  .Deprecated("facet_title_horizontal")  # nocov
  facet_title_horizontal()               # nocov
}
