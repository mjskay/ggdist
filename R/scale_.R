# Custom scales, largely for geom_area_interval and its derivatives
#
# Author: mjskay
###############################################################################

#' Custom ggplot scales for geom_area_interval (and derivatives)
#'
#' These scales allow more specific aesthetic mappings to be made when using \code{\link{geom_area_interval}}
#' and stats/geoms based on it (like eye plots).
#'
#' The following additional scales are defined:
#'
#' \enumerate{
#'   \item{\code{scale_point_color_*}}{Point color}
#' }
#'
#' See the corresponding scale documentation in ggplot for more information; e.g.
#' \code{\link{scale_color_discrete}}, \code{\link{scale_color_continuous}}, etc.
#'
#' Other scale functions can be used with the aesthetics/scales defined here by using the \code{aesthetics}
#' argument to that scale function. For example, to use color brewer scales with the \code{point_color} aesthetic:
#'
#' \code{scale_color_brewer(..., aesthetics = "point_color")}
#'
#' @name scale_
#' @author Matthew Kay
#' @seealso See \code{\link{geom_lineribbon}} for a similar geom designed for curves plus probability bands. See
#' \code{\link{geom_pointrange}} and \code{\link{geom_pointrangeh}} for the geoms these are based on.
#' @keywords manip
#' @examples
#'
#' # TODO
#'
#' @export
scale_point_colour_discrete =
  function(..., aesthetics = "point_colour") scale_colour_discrete(..., aesthetics = aesthetics)

#' @rdname scale_
#' @export
scale_point_color_discrete = scale_point_colour_discrete

#' @rdname scale_
#' @export
scale_point_colour_continuous = function(..., aesthetics = "point_colour", guide = "colorbar_generic") {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}

#' @rdname scale_
#' @export
scale_point_color_continuous = scale_point_colour_continuous

#' @rdname scale_
#' @export
scale_point_fill_continuous = function(..., aesthetics = "point_fill", guide = "colorbar_generic") {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}

#' @rdname scale_
#' @importFrom scales rescale_pal
#' @export
scale_point_size_continuous =
  function (..., range = c(1, 6)) continuous_scale("point_size", "point_size_c", area_pal(range), ...)

#' @rdname scale_
#' @export
scale_point_size_discrete = function(..., range = c(1, 6), na.translate = FALSE) {
  force(range)
  discrete_scale("point_size", "point_size_d", function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...)
}

#' @rdname scale_
#' @importFrom scales rescale_pal
#' @export
scale_interval_size_continuous =
  function (..., range = c(1.4, 0.6)) continuous_scale("interval_size", "interval_size_c", rescale_pal(range), ...)

#' @rdname scale_
#' @export
scale_interval_size_discrete = function(..., range = c(1.4, 0.6), na.translate = FALSE) {
  force(range)
  discrete_scale("interval_size", "interval_size_d", function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...)
}

#' @rdname scale_
#' @export
scale_interval_linetype = function(..., na.value = "blank") {
  discrete_scale("interval_linetype", "interval_linetype_d", linetype_pal(), na.value = na.value, ...)
}

#' @export
#' @rdname scale_
scale_interval_colour_discrete =
  function(..., aesthetics = "interval_colour") scale_colour_discrete(..., aesthetics = aesthetics)

#' @rdname scale_
#' @export
scale_interval_color_discrete = scale_interval_colour_discrete

#' @rdname scale_
#' @export
scale_interval_colour_continuous = function(..., aesthetics = "interval_colour", guide = "colorbar_generic") {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}

#' @rdname scale_
#' @export
scale_interval_color_continuous = scale_interval_colour_continuous

#' @export
#' @rdname scale_
scale_outside_colour_discrete =
  function(..., aesthetics = "outside_colour") scale_colour_discrete(..., aesthetics = aesthetics)

#' @rdname scale_
#' @export
scale_outside_color_discrete = scale_outside_colour_discrete

#' @rdname scale_
#' @export
scale_outside_colour_continuous = function(..., aesthetics = "outside_colour", guide = "colorbar_generic") {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}

#' @rdname scale_
#' @export
scale_outside_color_continuous = scale_outside_colour_continuous

#' @rdname scale_
#' @export
scale_outside_linetype = function(..., na.value = "blank") {
  discrete_scale("outside_linetype", "outside_linetype_d", linetype_pal(), na.value = na.value, ...)
}

#' @rdname scale_
#' @importFrom scales rescale_pal
#' @export
scale_outside_size_continuous =
  function (..., range = c(1, 6)) continuous_scale("outside_size", "outside_size_c", area_pal(range), ...)

#' @rdname scale_
#' @export
scale_outside_size_discrete = function(..., range = c(1, 6), na.translate = FALSE) {
  force(range)
  discrete_scale("outside_size", "outside_size_d", function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...)
}

#' @rdname scale_
#' @export
guide_colorbar_generic = function(...) {
  # the default colorbar throws an error when asked to draw color bars for these aesthetics
  # even though it seems perfectly capable of doing so. This fixes that...
  colorbar = ggplot2::guide_colorbar()
  colorbar$available_aes = union(colorbar$available_aes, c(
    "point_color",
    "point_fill",
    "interval_colour",
    "outside_colour"
  ))
  colorbar
}
