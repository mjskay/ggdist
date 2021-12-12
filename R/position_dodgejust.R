# This file is based on position-collide.R and position-dodge.R from ggplot2
# (https://github.com/tidyverse/ggplot2), which is under the MIT license:
#
# Copyright (c) 2020 ggplot2 authors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


# position_dodgejust ------------------------------------------------------

#' Dodge overlapping objects side-to-side, preserving justification
#'
#' A justification-preserving variant of [ggplot2::position_dodge()] which preserves the
#' vertical position of a geom while adjusting the horizontal position (or vice
#' versa when in a horizontal orientation). Unlike [ggplot2::position_dodge()],
#' [position_dodgejust()] attempts to preserve the "justification" of `x`
#' positions relative to the bounds containing them (`xmin`/`xmax`) (or `y`
#' positions relative to `ymin`/`ymax` when in a horizontal orientation). This
#' makes it useful for dodging annotations to geoms and stats from the
#' [geom_slabinterval()] family, which also preserve the justification of their
#' intervals relative to their slabs when dodging.
#'
#' @inheritParams ggplot2::position_dodge
#' @param justification Justification of the point position (`x`/`y`) relative
#'   to its bounds (`xmin`/`xmax` or `ymin`/`ymax`), where `0` indicates
#'   bottom/left justification and `1` indicates top/right justification
#'   (depending on `orientation`). This is only used if `xmin`/`xmax`/`ymin`/`ymax`
#'   are not supplied; in that case, `justification` will be used along with
#'   `width` to determine the bounds of the object prior to dodging.
#'
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' library(distributional)
#'
#' dist_df = tribble(
#'   ~group, ~subgroup, ~mean, ~sd,
#'   1,          "h",     5,   1,
#'   2,          "h",     7,   1.5,
#'   3,          "h",     8,   1,
#'   3,          "i",     9,   1,
#'   3,          "j",     7,   1
#' )
#'
#' # An example with normal "dodge" positioning
#' # Notice how dodge points are placed in the center of their bounding boxes,
#' # which can cause slabs to be positioned outside their bounds.
#' dist_df %>%
#'   ggplot(aes(
#'     x = factor(group), ydist = dist_normal(mean, sd),
#'     fill = subgroup
#'   )) +
#'   stat_halfeye(
#'     position = "dodge"
#'   ) +
#'   geom_rect(
#'     aes(xmin = group, xmax = group + 1, ymin = 2, ymax = 13, color = subgroup),
#'     position = "dodge",
#'     data = . %>% filter(group == 3),
#'     alpha = 0.1
#'   ) +
#'   geom_point(
#'     aes(x = group, y = 7.5, color = subgroup),
#'     position = position_dodge(width = 1),
#'     data = . %>% filter(group == 3),
#'     shape = 1,
#'     size = 4,
#'     stroke = 1.5
#'   ) +
#'   scale_fill_brewer(palette = "Set2") +
#'   scale_color_brewer(palette = "Dark2")
#'
#' # This same example with "dodgejust" positioning. For the points we
#' # supply a justification parameter to position_dodgejust which mimics the
#' # justification parameter of stat_halfeye, ensuring that they are
#' # placed appropriately. On slabinterval family geoms, position_dodgejust()
#' # will automatically detect the appropriate justification.
#' dist_df %>%
#'   ggplot(aes(
#'     x = factor(group), ydist = dist_normal(mean, sd),
#'     fill = subgroup
#'   )) +
#'   stat_halfeye(
#'     position = "dodgejust"
#'   ) +
#'   geom_rect(
#'     aes(xmin = group, xmax = group + 1, ymin = 2, ymax = 13, color = subgroup),
#'     position = "dodgejust",
#'     data = . %>% filter(group == 3),
#'     alpha = 0.1
#'   ) +
#'   geom_point(
#'     aes(x = group, y = 7.5, color = subgroup),
#'     position = position_dodgejust(width = 1, justification = 0),
#'     data = . %>% filter(group == 3),
#'     shape = 1,
#'     size = 4,
#'     stroke = 1.5
#'   ) +
#'   scale_fill_brewer(palette = "Set2") +
#'   scale_color_brewer(palette = "Dark2")
#'
#'
#'
#' @export
position_dodgejust <- function(width = NULL, preserve = c("total", "single"), justification = NULL) {
  ggproto(NULL, PositionDodgejust,
    width = width,
    preserve = match.arg(preserve),
    justification = justification
  )
}

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionDodgejust <- ggproto("PositionDodgejust", Position,
  width = NULL,
  preserve = "total",
  justification = NULL,

  setup_params = function(self, data) {
    flipped_aes = has_flipped_aes(data)
    data = flip_data(data, flipped_aes)

    if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warning0("Width not defined. Set with `position_dodgejust(width = ?)`")
    }

    list(
      width = self$width,
      preserve = self$preserve,
      justification = self$justification,
      flipped_aes = flipped_aes
    )
  },

  setup_data = function(self, data, params) {
    data = flip_data(data, params$flipped_aes)

    just = data$justification %||% params$justification %||% 0.5
    if (!"x" %in% names(data) && all(c("xmin", "xmax") %in% names(data))) {
      data$x = (data$xmax - data$xmin) * just + data$xmin
    }

    flip_data(data, params$flipped_aes)
  },

  compute_panel = function(data, params, scales) {
    data = flip_data(data, params$flipped_aes)
    collided = collide(
      data,
      params$width,
      name = "position_dodgejust",
      strategy = pos_dodgejust,
      preserve = params$preserve,
      justification = params$justification,
      x_name = if (isTRUE(params$flipped_aes)) "y" else "x"
    )
    flip_data(collided, params$flipped_aes)
  }
)

# Justification-preserving dodge of overlapping intervals.
# Assumes that each set has the same horizontal position.
# df must have x, xmin, and xmax defined
pos_dodgejust = function(df, width, n = NULL) {
  n = n %||% length(unique(df$group))

  if (n == 1) return(df)

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx = match(df$group, sort(unique(df$group)))

  # Find the justification of each point so we can preserve it
  x_width = df$xmax - df$xmin
  just = ifelse(x_width == 0, 0, (df$x - df$xmin) / x_width)

  # Find the center for each group, then use that to calculate xmin and xmax
  max_width = max(x_width)
  df$x = df$x + width * ((groupidx - 0.5) / n - .5)
  df$xmin = df$x - max_width / n / 2
  df$xmax = df$x + max_width / n / 2

  # Reset x position based on justification
  df$x = df$xmin + just * (df$xmax - df$xmin)

  df
}


# helper functions originally based on position-collide ------------------------

# Setup data for collision detection by making sure width and xmin/xmax are set
collide_setup = function(data, width = NULL, justification = NULL) {
  # Determine width
  if (!is.null(width)) {
    # Width set manually
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      just = data$justification %||% justification %||% 0.5
      data$xmin = data$x - width * just
      data$xmax = data$x + width * (1 - just)
    }
  } else {
    if (!(all(c("xmin", "xmax") %in% names(data)))) {
      data$xmin = data$x
      data$xmax = data$x
    }

    # Width determined from data, must be floating point constant
    widths = unique(data$xmax - data$xmin)
    widths = widths[!is.na(widths)]

    width = widths[1]
  }

  list(data = data, width = width)
}

# Collision detection
collide = function(
  data, width = NULL, name, strategy,
  preserve = "total", justification = NULL,
  x_name = "x"
) {
  dlist = collide_setup(data, width, justification)
  data = dlist$data
  width = dlist$width

  # Reorder by x position, then on group. The default stacking order reverses
  # the group in order to match the legend order.
  ord = order(data$xmin, -data$group)
  data = data[ord, ]

  # determine n
  n = switch(preserve,
    total = NULL,
    single = max(lengths(dlply_(data, "xmin", function(d) unique(d$group))))
  )

  # Check for overlap
  intervals = as.numeric(t(unique(data[c("xmin", "xmax")])))
  intervals = intervals[!is.na(intervals)]

  if (length(unique(intervals)) > 1 & any(diff(scale(intervals)) < -1e-6)) {
    warning0(paste0(name, " requires non-overlapping ", x_name, " intervals"))
  }

  # workaround so that mapped_discrete columns can be combined with numerics
  xy_cols = intersect(c("x","y","xmin","xmax","ymin","ymax"), names(data))
  data[xy_cols] = lapply(data[xy_cols], as.numeric)

  if (!is.null(data$ymax)) {
    data = ddply_(data, "xmin", strategy, n = n, width = width)
  } else if (!is.null(data$y)) {
    data$ymax = data$y
    data = ddply_(data, "xmin", strategy, n = n, width = width)
    data$y = data$ymax
  } else {
    stop0("Neither y nor ymax defined")
  }
  data[match(seq_along(ord), ord), ]
}
