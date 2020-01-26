# Custom scales, largely for geom_slabinterval and its derivatives
#
# Author: mjskay
###############################################################################


# scale_point_... ---------------------------------------------------------

#' Custom ggplot scales for geom_slabinterval (and derivatives)
#'
#' These scales allow more specific aesthetic mappings to be made when using [geom_slabinterval()]
#' and stats/geoms based on it (like eye plots).
#'
#' The following additional scales / aesthetics are defined for use with [geom_slabinterval()] and
#' related geoms:
#'
#' \enumerate{
#'   \item{`scale_point_color_* `}{Point color}
#'   \item{`scale_point_fill_* `}{Point fill color}
#'   \item{`scale_point_alpha_* `}{Point alpha level / opacity}
#'   \item{`scale_point_size_* `}{Point size}
#'   \item{`scale_interval_color_* `}{Interval line color}
#'   \item{`scale_interval_alpha_* `}{Interval alpha level / opacity}
#'   \item{`scale_interval_size_* `}{Interval line width}
#'   \item{`scale_interval_linetype_* `}{Interval line type}
#'   \item{`scale_slab_color_* `}{Slab outline color}
#'   \item{`scale_slab_fill_* `}{Slab fill color}
#'   \item{`scale_slab_alpha_* `}{Slab alpha level / opacity. The default settings of
#'   `scale_slab_alpha_continuous` differ from [scale_alpha_continuous()] and
#'   are designed for gradient plots (e.g. [stat_gradientinterval()]) by ensuring that
#'   densities of 0 get mapped to 0 in the output.}
#'   \item{`scale_slab_size_* `}{Slab outline line width}
#'   \item{`scale_slab_linetype_* `}{Slab outline line type}
#'   \item{`scale_slab_shape_* `}{Slab dot shape (for [geom_dotsinterval()])}
#' }
#'
#' See the corresponding scale documentation in ggplot for more information; e.g.
#' [scale_color_discrete()], [scale_color_continuous()], etc.
#'
#' Other scale functions can be used with the aesthetics/scales defined here by using the `aesthetics`
#' argument to that scale function. For example, to use color brewer scales with the `point_color` aesthetic:
#'
#' `scale_color_brewer(..., aesthetics = "point_color")`
#'
#' With continuous color scales, you may also need to provide a guide as the default guide does not work properly;
#' this is what `guide_colorbar2` is for:
#'
#' `scale_color_distiller(..., guide = "colorbar2", aesthetics = "point_color")`
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_shape
#' @param ... Arguments passed to underlying scale or guide functions. E.g. `scale_point_color_discrete`
#' passes arguments to [scale_color_discrete()]. See those functions for more details.
#' @param aesthetics Names of aesthetics to set scales for.
#' @param guide Guide to use for legends for an aesthetic.
#' @param range a numeric vector of length 2 that specifies the minimum and maximum size of the plotting symbol
#' after transformation.
#' @param na.translate In discrete scales, should we show missing values?
#' @param na.value When `na.translate` is true, what value should be shown?
#' @name scales
#' @author Matthew Kay
#' @seealso [scale_color_discrete()], [scale_color_continuous()], etc.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' # This plot shows how to set multiple specific aesthetics
#' # NB it is very ugly and is only for demo purposes.
#' data.frame(distribution = "Normal(1,2)") %>%
#'   parse_dist(distribution) %>%
#'   ggplot(aes(y = distribution, dist = .dist, args = .args)) +
#'   stat_dist_halfeyeh(
#'     shape = 21,  # this point shape has a fill and outline
#'     point_color = "red",
#'     point_fill = "black",
#'     point_alpha = .1,
#'     point_size = 6,
#'     stroke = 2,
#'     interval_color = "blue",
#'     # interval sizes are scaled from [1, 6] onto [0.6, 1.4] by default
#'     # see the interval_size_range option in help("geom_slabinterval")
#'     interval_size = 8,
#'     interval_linetype = "dashed",
#'     interval_alpha = .25,
#'     # fill sets the fill color of the slab (here the density)
#'     slab_color = "green",
#'     slab_fill = "purple",
#'     slab_size = 3,
#'     slab_linetype = "dotted",
#'     slab_alpha = .5
#'   )
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
scale_point_fill_discrete =
  function(..., aesthetics = "point_fill") scale_colour_discrete(..., aesthetics = aesthetics)
#' @rdname scales
#' @export
scale_point_fill_continuous = function(..., aesthetics = "point_fill", guide = "colourbar2") {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}


#' @rdname scales
#' @importFrom scales rescale_pal
#' @export
scale_point_alpha_continuous = function(..., range = c(0.1, 1)) {
  continuous_scale("point_alpha", "point_alpha_c", rescale_pal(range), ...)
}
#' @rdname scales
#' @export
scale_point_alpha_discrete = function(..., range = c(0.1, 1)) {
  discrete_scale("point_alpha", "point_alpha_d", function(n) seq(range[1], range[2], length.out = n), ...)
}


#' @rdname scales
#' @importFrom scales area_pal
#' @export
scale_point_size_continuous =
  function(..., range = c(1, 6)) continuous_scale("point_size", "point_size_c", area_pal(range), ...)
#' @rdname scales
#' @export
scale_point_size_discrete = function(..., range = c(1, 6), na.translate = FALSE) {
  force(range)
  discrete_scale("point_size", "point_size_d", function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...)
}


# scale_interval_... ------------------------------------------------------

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


#' @rdname scales
#' @importFrom scales rescale_pal
#' @export
scale_interval_alpha_continuous = function(..., range = c(0.1, 1)) {
  continuous_scale("interval_alpha", "interval_alpha_c", rescale_pal(range), ...)
}
#' @rdname scales
#' @export
scale_interval_alpha_discrete = function(..., range = c(0.1, 1)) {
  discrete_scale("interval_alpha", "interval_alpha_d", function(n) seq(range[1], range[2], length.out = n), ...)
}


#' @rdname scales
#' @importFrom scales area_pal
#' @export
scale_interval_size_continuous =
  function(..., range = c(1, 6)) continuous_scale("interval_size", "interval_size_c", area_pal(range), ...)
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
scale_interval_linetype_discrete = function(..., na.value = "blank") {
  discrete_scale("interval_linetype", "interval_linetype_d", linetype_pal(), na.value = na.value, ...)
}
#' @rdname scales
#' @export
scale_interval_linetype_continuous = function(...) {
  stop("A continuous variable cannot be mapped to linetype", call. = FALSE)
}


# scale_slab_... ----------------------------------------------------------

#' @export
#' @rdname scales
scale_slab_colour_discrete =
  function(..., aesthetics = "slab_colour") scale_colour_discrete(..., aesthetics = aesthetics)
#' @rdname scales
#' @export
scale_slab_color_discrete = scale_slab_colour_discrete
#' @rdname scales
#' @export
scale_slab_colour_continuous = function(..., aesthetics = "slab_colour", guide = "colourbar2") {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}
#' @rdname scales
#' @export
scale_slab_color_continuous = scale_slab_colour_continuous


#' @rdname scales
#' @export
scale_slab_fill_discrete =
  function(..., aesthetics = "slab_fill") scale_colour_discrete(..., aesthetics = aesthetics)
#' @rdname scales
#' @export
scale_slab_fill_continuous = function(..., aesthetics = "slab_fill", guide = "colourbar2") {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}


#' @rdname scales
#' @importFrom scales rescale_pal
#' @export
scale_slab_alpha_continuous = function(..., limits = function(l) c(min(0, l[[1]]), l[[2]]), range = c(0, 1)) {
  continuous_scale("slab_alpha", "slab_alpha_c", rescale_pal(range), limits = limits, ...)
}
#' @rdname scales
#' @export
scale_slab_alpha_discrete = function(..., range = c(0.1, 1)) {
  discrete_scale("slab_alpha", "slab_alpha_d", function(n) seq(range[1], range[2], length.out = n), ...)
}


#' @rdname scales
#' @importFrom scales area_pal
#' @export
scale_slab_size_continuous =
  function(..., range = c(1, 6)) continuous_scale("slab_size", "slab_size_c", area_pal(range), ...)
#' @rdname scales
#' @export
scale_slab_size_discrete = function(..., range = c(1, 6), na.translate = FALSE) {
  force(range)
  discrete_scale("slab_size", "slab_size_d", function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...)
}


#' @rdname scales
#' @importFrom scales linetype_pal
#' @export
scale_slab_linetype_discrete = function(..., na.value = "blank") {
  discrete_scale("slab_linetype", "slab_linetype_d", linetype_pal(), na.value = na.value, ...)
}
#' @rdname scales
#' @export
scale_slab_linetype_continuous = function(...) {
  stop("A continuous variable cannot be mapped to linetype", call. = FALSE)
}


#' @rdname scales
#' @importFrom scales shape_pal
#' @export
scale_slab_shape_discrete = function(..., solid = TRUE) {
  discrete_scale("slab_shape", "slab_shape_d", shape_pal(solid), ...)
}
#' @rdname scales
#' @export
scale_slab_shape_continuous = function (...) {
  stop("A continuous variable cannot be mapped to shape", call. = FALSE)
}


# guide_colorbar2 ---------------------------------------------------------

#' @rdname scales
#' @export
guide_colourbar2 = function(...) {
  # the default colourbar throws an error when asked to draw color bars for these aesthetics
  # even though it seems perfectly capable of doing so. This fixes that...
  guide_colourbar(available_aes = union(guide_colourbar()$available_aes, c(
    "point_colour",
    "point_fill",
    "interval_colour",
    "slab_colour",
    "slab_fill"
  )))
}

#' @rdname scales
#' @export
guide_colorbar2 = guide_colourbar2
