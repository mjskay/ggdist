# Custom scales, largely for geom_area_interval and its derivatives
#
# Author: mjskay
###############################################################################

#' Custom ggplot scales for geom_area_interval (and derivatives)
#'
#' These scales allow more specific aesthetic mappings to be made when using \code{\link{geom_area_interval}}
#' and stats/geoms based on it (like eye plots).
#'
#' The following additional scales / aesthetics are defined for use with \code{\link{geom_area_interval}} and
#' related geoms:
#'
#' \enumerate{
#'   \item{\code{scale_point_color_* }}{Point color}
#'   \item{\code{scale_point_fill_* }}{Point fill color}
#'   \item{\code{scale_point_size_* }}{Point size}
#'   \item{\code{scale_interval_color_* }}{Interval line color}
#'   \item{\code{scale_interval_size_* }}{Interval line width}
#'   \item{\code{scale_interval_linetype_* }}{Interval line type}
#'   \item{\code{scale_outside_color_* }}{Outside line color}
#'   \item{\code{scale_outside_linetype_* }}{Outside line type}
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
#' With continuous color scales, you may also need to provide a guide as the default guide does not work properly;
#' this is what \code{guide_colorbar2} is for:
#'
#' \code{scale_color_distiller(..., guide = "colorbar2", aesthetics = "point_color")}
#'
#' @param ... Arguments passed to underlying scale or guide functions. E.g. \code{scale_point_color_discrete}
#' passes arguments to \code{\link{scale_color_discrete}}. See those functions for more details.
#' @param aesthetics Names of aesthetics to set scales for.
#' @param guide Guide to use for legends for an aesthetic.
#' @param range a numeric vector of length 2 that specifies the minimum and maximum size of the plotting symbol
#' after transformation.
#' @param na.translate In discrete scales, should we show missing values?
#' @param na.value When na.translate is true, what value should be shown?
#' @name scales
#' @author Matthew Kay
#' @seealso \code{\link{scale_color_discrete}}, \code{\link{scale_color_continuous}}, etc.
#' @keywords manip
#' @examples
#'
#' # TODO
#'
#' @export
scale_point_colour_discrete =
  function(..., aesthetics = "point_colour") scale_colour_discrete(..., aesthetics = aesthetics)

#' @rdname scales
#' @export
scale_point_color_discrete = scale_point_colour_discrete

#' @rdname scales
#' @export
scale_point_colour_continuous = function(..., aesthetics = "point_colour", guide = "colourbar2") {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}

#' @rdname scales
#' @export
scale_point_color_continuous = scale_point_colour_continuous

#' @rdname scales
#' @export
scale_point_fill_continuous = function(..., aesthetics = "point_fill", guide = "colourbar2") {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}

#' @rdname scales
#' @importFrom scales area_pal
#' @export
scale_point_size_continuous =
  function (..., range = c(1, 6)) continuous_scale("point_size", "point_size_c", area_pal(range), ...)

#' @rdname scales
#' @export
scale_point_size_discrete = function(..., range = c(1, 6), na.translate = FALSE) {
  force(range)
  discrete_scale("point_size", "point_size_d", function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...)
}

#' @rdname scales
#' @importFrom scales area_pal
#' @export
scale_interval_size_continuous =
  function (..., range = c(1, 6)) continuous_scale("interval_size", "interval_size_c", area_pal(range), ...)

#' @rdname scales
#' @export
scale_interval_size_discrete = function(..., range = c(1, 6), na.translate = FALSE) {
  force(range)
  discrete_scale("interval_size", "interval_size_d", function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...)
}

#' @rdname scales
#' @importFrom scales linetype_pal
#' @export
scale_interval_linetype = function(..., na.value = "blank") {
  discrete_scale("interval_linetype", "interval_linetype_d", linetype_pal(), na.value = na.value, ...)
}

#' @export
#' @rdname scales
scale_interval_colour_discrete =
  function(..., aesthetics = "interval_colour") scale_colour_discrete(..., aesthetics = aesthetics)

#' @rdname scales
#' @export
scale_interval_color_discrete = scale_interval_colour_discrete

#' @rdname scales
#' @export
scale_interval_colour_continuous = function(..., aesthetics = "interval_colour", guide = "colourbar2") {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}

#' @rdname scales
#' @export
scale_interval_color_continuous = scale_interval_colour_continuous

#' @export
#' @rdname scales
scale_outside_colour_discrete =
  function(..., aesthetics = "outside_colour") scale_colour_discrete(..., aesthetics = aesthetics)

#' @rdname scales
#' @export
scale_outside_color_discrete = scale_outside_colour_discrete

#' @rdname scales
#' @export
scale_outside_colour_continuous = function(..., aesthetics = "outside_colour", guide = "colourbar2") {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}

#' @rdname scales
#' @export
scale_outside_color_continuous = scale_outside_colour_continuous

#' @rdname scales
#' @importFrom scales linetype_pal
#' @export
scale_outside_linetype = function(..., na.value = "blank") {
  discrete_scale("outside_linetype", "outside_linetype_d", linetype_pal(), na.value = na.value, ...)
}

#' @rdname scales
#' @importFrom scales area_pal
#' @export
scale_outside_size_continuous =
  function (..., range = c(1, 6)) continuous_scale("outside_size", "outside_size_c", area_pal(range), ...)

#' @rdname scales
#' @export
scale_outside_size_discrete = function(..., range = c(1, 6), na.translate = FALSE) {
  force(range)
  discrete_scale("outside_size", "outside_size_d", function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...)
}

#' @rdname scales
#' @export
guide_colourbar2 = function(...) {
  # the default colourbar throws an error when asked to draw color bars for these aesthetics
  # even though it seems perfectly capable of doing so. This fixes that...
  colourbar = ggplot2::guide_colourbar(...)
  colourbar$available_aes = union(colourbar$available_aes, c(
    "point_color",
    "point_fill",
    "interval_colour",
    "outside_colour"
  ))
  colourbar
}

#' @rdname scales
#' @export
guide_colorbar2 = guide_colourbar2
