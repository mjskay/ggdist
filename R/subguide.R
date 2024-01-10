subguide_left = auto_partial(name = "subguide_left", function(
  scale,
  title = "thickness",
  theme = theme_get(),
  side = "right",
  just = 0.01,
  orientation = "horizontal"
) {
  define_orientation_variables(orientation)
  grob_width = switch(width., width = grobWidth, height = grobHeight)

  break_positions = as.numeric(scale$map(scale$get_breaks()))
  break_labels = scale$get_labels()

  axis_position = get_subguide_position(side, orientation)
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
