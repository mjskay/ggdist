#' Axis sub-guide for thickness scales
#'
#' This is a sub-guide intended for annotating the [thickness] aesthetic
#' in \pkg{ggdist}. It can be used with the `subguide` parameter of
#' [geom_slabinterval()].
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @param scale A [ggplot2::Scale], typically an instance of [scale_thickness_shared()].
#' @param title The title of the scale shown on the sub-guide's axis.
#' @param just Numeric value between `0` and `1` giving the justification of the
#'    guide relative to the axis: `0` causes the sub-guide to be drawn on the
#'    left or bottom depending on if `orientation` is `"horizontal"` or `"vertical"`,
#'    and `1` causes the sub-guide to be drawn on the top or right depending on
#'    if `orientation` is `"horizontal"` or `"vertical"`.
#' @param label_side Which side of the axis to draw the ticks and labels on.
#'    `"topright"`, `"top"`, and `"right"` are synonyms which cause the labels
#'    to be drawn on the top or the right depending on if `orientation` is
#'    `"horizontal"` or `"vertical"`. `"bottomleft"`, `"bottom"`, and `"left"`
#'    are synonyms which cause the labels to be drawn on the bottom or the left
#'    depending on if `orientation` is `"horizontal"` or `"vertical"`.
#'    `"topleft"` causes the labels to be drawn on the top or the left, and
#'    `"bottomright"` causes the labels to be drawn on the bottom or the right.
#' @param orientation Orientation of the geometry this sub-guide is for. One
#'    of `"horizontal"` (`"y"`) or `"vertical"` (`"x"`). See the `orientation`
#'    parameter to [geom_slabinterval()].
#' @param theme A [ggplot2::theme] object used to determine the style that the
#'    sub-guide elements are drawn in. The title label is drawn using the
#'    `"axis.title.x"` or `"axis.title.y"` theme setting, and the axis line,
#'    ticks, and tick labels are drawn using [guide_axis()], so the same theme
#'    settings that normally apply to axis guides will be followed.
#' @family sub-guides
#' @export
subguide_axis = auto_partial(name = "subguide_axis", function(
  scale,
  title = NULL,
  just = 0,
  label_side = "topright",
  orientation = "horizontal",
  theme = theme_get()
) {
  define_orientation_variables(orientation)
  grob_width = switch(width., width = grobWidth, height = grobHeight)

  break_positions = as.numeric(scale$map(scale$get_breaks()))
  break_labels = scale$get_labels()

  axis_position = get_subguide_position(label_side, orientation)
  axis_just = if (axis_position %in% c("left", "bottom")) just - 1 else just
  axis_grob = ggplot2:::draw_axis(break_positions = break_positions, break_labels = break_labels, axis_position = axis_position, theme = theme)
  axis_width = grob_width(axis_grob)

  title_just = if (axis_position %in% c("left", "bottom")) just else just - 1
  title_element = calc_element(paste0("axis.title.", y), theme)
  title_margin = max(title_element$margin)
  title_grob = element_grob(title_element, label = title)
  title_width = grob_width(title_grob)

  width = title_width + title_margin * 2 + axis_width

  axis_viewport = viewport(just = c(0, 0))
  axis_viewport[[x]] = unit(axis_just, "npc") - width * axis_just
  axis_viewport[[y]] = unit(0, "npc")

  title_viewport = viewport(just = c(0, 0))
  title_viewport[[x]] = unit(just, "npc") - axis_width * title_just - title_margin * (2 * just - 1) - title_width * just +
    if (orientation %in% c("x", "vertical")) unit(-1, "npc") + title_width else unit(0, "npc")
  title_viewport[[y]] = unit(0, "npc")

  grobTree(grobTree(axis_grob, vp = axis_viewport), grobTree(title_grob, vp = title_viewport))
})


#' Empty sub-guide for thickness scales
#'
#' This is a blank sub-guide that omits annotations for the [thickness] aesthetic
#' in \pkg{ggdist}. It can be used with the `subguide` parameter of
#' [geom_slabinterval()].
#'
#' @param ... ignored.
#' @family sub-guides
#' @export
subguide_none = function(...) {
  zeroGrob()
}


# helpers -----------------------------------------------------------------

get_subguide_position = function(side, orientation) {
  switch(orientation,
    y = ,
    horizontal = switch(side,
      top = ,
      topright = ,
      bottomright = ,
      right = "right",

      bottom = ,
      bottomleft = ,
      topleft = ,
      left = "left",

      stop0("Unknown side: ", deparse0(side))
    ),
    x = ,
    vertical = switch(side,
      right = ,
      topright = ,
      topleft = ,
      top = "top",

      left = ,
      bottomright = ,
      bottomleft = ,
      bottom = "bottom",

      stop0("Unknown side: ", deparse0(side))
    ),
    stop0("Unknown orientation: ", deparse0(orientation))
  )
}
