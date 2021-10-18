# ggplot2 scales for distributional objects and rvars
#
# Author: mjskay
###############################################################################



# scales ------------------------------------------------------------------

#' Position scales for distributional data (x & y)
#'
#' These scales act nearly identically to `scale_x_continuous()` and `scale_y_continuous()`,
#' except that they allow `[distributional::distributional][distribution]`s and
#' `[rvar]`s to be passed to the `x` and `y` aesthetics when using [geom_slabinterval()]
#' and stats/geoms based on it (like eye plots).
#'
#' @inheritParams ggplot2::scale_x_continuous
#'
#' @return
#' A [ggplot2::Scale] representing one of the aesthetics used to target the appearance of specific parts of composite
#' `ggdist` geoms. Can be added to a [ggplot()] object.
#' @name scale_dist
#' @author Matthew Kay
#' @family ggdist scales
#' @seealso The ggplot2 positional scales: [scale_x_continuous()], [scale_y_continuous()], etc.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' library(distributional)
#'
#' data.frame(var = c("a", "b"), dist = dist_normal(c(-5,5), 1)) %>%
#'   ggplot(aes(x = var, y = dist)) +
#'   stat_dist_halfeye() +
#'   scale_y_dist(limits = c(-10, 10))
#'
#' @importFrom scales censor
#' @export
scale_x_dist = function (
  name = waiver(), breaks = waiver(), minor_breaks = waiver(),
  n.breaks = NULL, labels = waiver(), limits = NULL, expand = waiver(),
  oob = censor, na.value = NA_real_, trans = "identity",
  guide = waiver(), position = "bottom", sec.axis = waiver()
) {
  sc <- continuous_scale(
    "x",
    "position_dist", identity, name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = guide, position = position, super = ScaleDistPosition
  )

  set_sec_axis(sec.axis, sc)
}


#' @rdname scale_dist
#' @export
scale_y_dist <- function(
  name = waiver(), breaks = waiver(), minor_breaks = waiver(),
  n.breaks = NULL, labels = waiver(), limits = NULL, expand = waiver(),
  oob = censor, na.value = NA_real_, trans = "identity",
  guide = waiver(), position = "left", sec.axis = waiver()
) {
  sc <- continuous_scale(
    "y",
    "position_dist", identity, name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = guide, position = position, super = ScaleDistPosition
  )

  set_sec_axis(sec.axis, sc)
}


#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleDistPosition <- ggproto("ScaleDistPosition", ScaleContinuousPosition,
  map = function(self, x, ...) {
    if (is_dist_like(x)) {
      x
    } else {
      ggproto_parent(ScaleContinuousPosition, self)$map(x, ...)
    }
  },
  train = function(self, x) {
    if (is_dist_like(x)) x = median(x)
    ggproto_parent(ScaleContinuousPosition, self)$train(x)
  }
)


# scale types -------------------------------------------------------------

#' @export
scale_type.distribution = function(x) c("dist", "continuous")

#' @export
scale_type.rvar = function(x) c("dist", "continuous")


# helpers -----------------------------------------------------------------

#' helper function for setting secondary axes in scale creation
#' based on ggplot2/axis-secondary.R
#' @noRd
set_sec_axis = function(sec.axis, scale) {
  if (!inherits(sec.axis, "waiver")) {
    if (inherits(sec.axis, "formula")) sec.axis = sec_axis(sec.axis)
    if (!inherits(sec.axis, "AxisSecondary")) stop0("Secondary axes must be specified using 'sec_axis()'")
    scale$secondary.axis = sec.axis
  }

  scale
}
