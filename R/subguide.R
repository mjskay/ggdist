#' Axis sub-guide for thickness scales
#'
#' This is a sub-guide intended for annotating the `thickness` aesthetic
#' in \pkg{ggdist}. It can be used with the `subguide` parameter of
#' [geom_slabinterval()].
#' @template description-auto-partial
#'
#' @inheritParams scale_thickness
#' @param values Values used to construct the scale used for this guide.
#'    Typically provided automatically by [geom_slabinterval()].
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
#' @seealso The [thickness] datatype.
#' @seealso The `thickness` aesthetic of [geom_slabinterval()].
#' @seealso [scale_thickness_shared()], for setting a `thickness` scale across
#' all geometries using the `thickness` aesthetic.
#' @seealso [subscale_thickness()], for setting a `thickness` sub-scale within
#' a single [geom_slabinterval()].
#' @examples
#' library(ggplot2)
#' library(distributional)
#'
#' df = data.frame(d = dist_normal(2:3, 2:3), g = c("a", "b"))
#'
#' # subguides allow you to label thickness axes
#' ggplot(df, aes(xdist = d, y = g)) +
#'   stat_slabinterval(subguide = "inside")
#'
#' # they respect normalization and use of scale_thickness_shared()
#' ggplot(df, aes(xdist = d, y = g)) +
#'   stat_slabinterval(subguide = "inside", normalize = "groups")
#'
#' # they can also be positioned outside the plot area, though
#' # this typically requires manually adjusting plot margins
#' ggplot(df, aes(xdist = d, y = g)) +
#'   stat_slabinterval(subguide = subguide_outside(title = "density", position = "right")) +
#'   theme(plot.margin = margin(5.5, 50, 5.5, 5.5))
#'
#' # any of the subguide types will also work to indicate bin counts in
#' # geom_dots(); subguide_integer() and subguide_count() can be useful for
#' # dotplots as they only label integers / whole numbers:
#' df = data.frame(d = dist_gamma(2:3, 2:3), g = c("a", "b"))
#' ggplot(df, aes(xdist = d, y = g)) +
#'   stat_dots(subguide = subguide_count(label_side = "left", title = "count")) +
#'   scale_y_discrete(expand = expansion(add = 0.1)) +
#'   scale_x_continuous(expand = expansion(add = 0.5))
#'
#' @importFrom scales oob_discard
#' @export
subguide_axis = auto_partial(name = "subguide_axis", function(
  values,
  title = NULL,
  breaks = waiver(),
  labels = waiver(),
  position = 0,
  just = 0,
  label_side = "topright",
  orientation = "horizontal",
  theme = theme_get()
) {
  define_orientation_variables(orientation)
  grob_width = switch(width., width = grobWidth, height = grobHeight)
  position = get_subguide_position(position, orientation)

  limits = range(values)
  scale = scale_thickness_shared(breaks = breaks, labels = labels, limits = limits)
  scale$train(values)

  breaks = oob_discard(scale$get_breaks(), limits)
  break_positions = as.numeric(scale$map(breaks))
  break_labels = scale$get_labels(breaks)

  axis_position = get_subguide_axis_position(label_side, position, orientation)
  axis_is_topleft = axis_position %in% c("left", "top")
  axis_grob = draw_subguide_axis(
    break_positions = break_positions,
    break_labels = break_labels,
    aes = y,
    opp = x,
    axis_position = axis_position,
    theme = theme
  )
  axis_width = grob_width(axis_grob)

  title_element = calc_element(paste0("axis.title.", y), theme)
  title_margin = max(title_element$margin)
  title_element$margin = margin(0, 0, 0, 0)
  title_grob = element_grob(title_element, label = title)
  title_width = grob_width(title_grob)

  # determine positions of title and axis grobs in the table layout
  col_widths = unit.c(title_margin, title_width, if (!is.null(title)) title_margin else unit(0, "npc"), axis_width)
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
#' [subguide_inside()] is a shortcut for drawing labels inside of the chart
#' region.
#' @rdname subguide_axis
#' @export
subguide_inside = function(..., label_side = "inside") {
  subguide_axis(..., label_side = label_side)
}

#' @details
#' [subguide_outside()] is a shortcut for drawing labels outside of the chart
#' region.
#' @rdname subguide_axis
#' @export
subguide_outside = function(..., label_side = "outside", just = 1) {
  subguide_axis(..., label_side = label_side, just = just)
}

#' @details
#' [subguide_integer()] only draws breaks that are integer values, useful for
#' labeling counts in [geom_dots()].
#' @rdname subguide_axis
#' @export
subguide_integer = function(..., breaks = scales::breaks_extended(Q = c(1, 5, 2, 4, 3))) {
  force(breaks)
  breaks_fun = function(x, ...) {
    x = x[is.finite(x)]
    if (length(x) == 0) return(numeric())

    b = breaks(x, ...)
    b = b[is_integerish(b)]
    if (length(b) == 0) b = unique(round(range(x)))
    b
  }
  subguide_axis(..., breaks = breaks_fun)
}

#' @details
#' [subguide_count()] is a shortcut for drawing labels where *every* whole number
#' is labeled, useful for labeling counts in [geom_dots()]. If your max count is
#' large, [subguide_integer()] may be better.
#' @rdname subguide_axis
#' @export
subguide_count = function(..., breaks = scales::breaks_width(1)) {
  subguide_axis(..., breaks = breaks)
}

#' Empty sub-guide for thickness scales
#'
#' This is a blank sub-guide that omits annotations for the `thickness` aesthetic
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
      stop0("Unknown orientation: ", deparse0(orientation))  # nocov
    )
  }
}

#' Transform the combination of `position` and `side` into an axis position;
#' i.e. one of `"left"` or `"right"`.
#' @noRd
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
    stop0("Unknown orientation: ", deparse0(orientation))  # nocov
  )
}

#' modified version of ggplot2:::draw_axis for use by subguide_axis
#' @importFrom rlang :=
#' @noRd
draw_subguide_axis = function(
  break_positions, break_labels, aes, opp, axis_position, theme,
  check.overlap = FALSE, angle = NULL, n.dodge = 1
) {
  guide = guide_axis(
    check.overlap = check.overlap,
    angle = angle,
    n.dodge = n.dodge,
    position = axis_position
  )
  params = guide$params
  params$key = data_frame0(
    !!aes := break_positions,
    .value = break_positions,
    .label = break_labels
  )
  params$decor = data_frame0(
    !!aes := c(0, 1),
    !!opp := if (axis_position %in% c("top", "right")) 0 else 1
  )
  guide$draw(theme, params = params)
}
