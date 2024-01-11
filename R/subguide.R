#' Axis sub-guide for thickness scales
#'
#' This is a sub-guide intended for annotating the [thickness] aesthetic
#' in \pkg{ggdist}. It can be used with the `subguide` parameter of
#' [geom_slabinterval()].
#' Supports [automatic partial function application][automatic-partial-functions].
#'
#' @param scale A [ggplot2::Scale], typically an instance of [scale_thickness_shared()].
#' @param title The title of the scale shown on the sub-guide's axis.
#' @param position Numeric value between `0` and `1` giving the position of the
#'    guide relative to the axis: `0` causes the sub-guide to be drawn on the
#'    left or bottom depending on if `orientation` is `"horizontal"` or `"vertical"`,
#'    and `1` causes the sub-guide to be drawn on the top or right depending on
#'    if `orientation` is `"horizontal"` or `"vertical"`. May also be a string
#'    indicating the position: `"top"`, `"right"`, `"bottom"`, `"left"`,
#'    `"topright"`, `"topleft"`, `"bottomright"`, or `"bottomleft"`.
#' @param just Numeric value between `0` and `1` giving the justification of the
#'    guide relative to its position: 0 means aligned towards the inside of the
#'    axis edge, 1 means aligned towards the outside of the axis edge.
#' @param label_side Which side of the axis to draw the ticks and labels on.
#'    `"topright"`, `"top"`, and `"right"` are synonyms which cause the labels
#'    to be drawn on the top or the right depending on if `orientation` is
#'    `"horizontal"` or `"vertical"`. `"bottomleft"`, `"bottom"`, and `"left"`
#'    are synonyms which cause the labels to be drawn on the bottom or the left
#'    depending on if `orientation` is `"horizontal"` or `"vertical"`.
#'    `"topleft"` causes the labels to be drawn on the top or the left, and
#'    `"bottomright"` causes the labels to be drawn on the bottom or the right.
#'    `"inside"` causes the labels to be drawn on the side closest to the inside
#'    of the chart, depending on `position`, and `"outside"` on the side closest
#'    to the outside of the chart.
#' @param orientation Orientation of the geometry this sub-guide is for. One
#'    of `"horizontal"` (`"y"`) or `"vertical"` (`"x"`). See the `orientation`
#'    parameter to [geom_slabinterval()].
#' @param theme A [ggplot2::theme] object used to determine the style that the
#'    sub-guide elements are drawn in. The title label is drawn using the
#'    `"axis.title.x"` or `"axis.title.y"` theme setting, and the axis line,
#'    ticks, and tick labels are drawn using [guide_axis()], so the same theme
#'    settings that normally apply to axis guides will be followed.
#' @param ... Arguments passed to other functions, typically back to
#'    `subguide_axis()` itself.
#' @family sub-guides
#' @export
subguide_axis = auto_partial(name = "subguide_axis", function(
  scale,
  title = NULL,
  position = 0,
  just = 0,
  label_side = "topright",
  orientation = "horizontal",
  theme = theme_get()
) {
  define_orientation_variables(orientation)
  grob_width = switch(width., width = grobWidth, height = grobHeight)
  position = get_subguide_position(position, orientation)

  break_positions = as.numeric(scale$map(scale$get_breaks()))
  break_labels = scale$get_labels()

  axis_position = get_subguide_axis_position(label_side, position, orientation)
  axis_is_topleft = axis_position %in% c("left", "top")
  #TODO: can't use unexported function here
  axis_grob = ggplot2:::draw_axis(break_positions = break_positions, break_labels = break_labels, axis_position = axis_position, theme = theme)
  axis_width = grob_width(axis_grob)

  title_element = calc_element(paste0("axis.title.", y), theme)
  title_margin = max(title_element$margin)
  title_grob = element_grob(title_element, label = title)
  title_width = grob_width(title_grob)

  # determine positions of title and axis grobs in the table layout
  col_widths = unit.c(title_margin, title_width, title_margin, axis_width)
  if (axis_is_topleft) {
    title_i = 2
    axis_i = 4
  } else {
    title_i = 3
    axis_i = 1
    col_widths = rev(col_widths)
  }
  table_width = sum(col_widths)

  # adjust table position per its justification
  vp = viewport()
  vp[[x]] = unit(position, "npc") + table_width * (just - 0.5) * (position - 0.5) * 2

  # construct table
  gt = gtable::gtable(widths = col_widths, heights = unit(1, "npc"), vp = vp)
  gt = gtable::gtable_add_grob(gt, axis_grob, 1, axis_i)
  gt = gtable::gtable_add_grob(gt, title_grob, 1, title_i)
  if (orientation %in% c("x", "vertical")) gt = t(gt)
  gt
})

#' @details
#' [subguide_inside()] is a shortcut for `subguide_axis(label_side = "inside")`
#' @rdname subguide_axis
#' @export
subguide_inside = subguide_axis(label_side = "inside")

#' @details
#' [subguide_outside()] is a shortcut for `subguide_axis(label_side = "outside", just = 1)`
#' @rdname subguide_axis
#' @export
subguide_outside = subguide_axis(label_side = "outside", just = 1)

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

#' Transform `position` into a numeric position in [0, 1]
#' @noRd
get_subguide_position = function(position, orientation) {
  if (is.numeric(position)) {
    position
  } else {
    switch(orientation,
      y = ,
      horizontal = switch(position,
        top = ,
        topright = ,
        bottomright = ,
        right = 1,

        bottom = ,
        bottomleft = ,
        topleft = ,
        left = 0,

        stop0("Unknown position: ", deparse0(position))
      ),
      x = ,
      vertical = switch(position,
        right = ,
        topright = ,
        topleft = ,
        top = 1,

        left = ,
        bottomright = ,
        bottomleft = ,
        bottom = 0,

        stop0("Unknown position: ", deparse0(position))
      ),
      stop0("Unknown orientation: ", deparse0(orientation))
    )
  }
}

#' Transform the combination of `position` and `side` into an axis position;
#' i.e. one of `"left"` or `"right"`.
get_subguide_axis_position = function(side, position, orientation) {
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

      inside = if (position < 0.5) "right" else "left",
      outside = if (position < 0.5) "left" else "right",

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

      inside = if (position < 0.5) "top" else "bottom",
      outside = if (position < 0.5) "bottom" else "top",

      stop0("Unknown side: ", deparse0(side))
    ),
    stop0("Unknown orientation: ", deparse0(orientation))
  )
}
