# Custom scales, largely for geom_slabinterval and its derivatives
#
# Author: mjskay
###############################################################################


# scale_point_... ---------------------------------------------------------

#' Sub-geometry scales for geom_slabinterval (ggplot2 scales)
#'
#' These scales allow more specific aesthetic mappings to be made when using [geom_slabinterval()]
#' and stats/geoms based on it (like eye plots).
#'
#' The following additional scales / aesthetics are defined for use with [geom_slabinterval()] and
#' related geoms:
#'
#' \describe{
#'   \item{`scale_point_color_* `}{Point color}
#'   \item{`scale_point_fill_* `}{Point fill color}
#'   \item{`scale_point_alpha_* `}{Point alpha level / opacity}
#'   \item{`scale_point_size_* `}{Point size}
#'   \item{`scale_interval_color_* `}{Interval line color}
#'   \item{`scale_interval_alpha_* `}{Interval alpha level / opacity}
#'   \item{`scale_interval_linetype_* `}{Interval line type}
#'   \item{`scale_slab_color_* `}{Slab outline color}
#'   \item{`scale_slab_fill_* `}{Slab fill color}
#'   \item{`scale_slab_alpha_* `}{Slab alpha level / opacity. The default settings of
#'   `scale_slab_alpha_continuous` differ from [`scale_alpha_continuous()`][ggplot2::scale_alpha_continuous]
#'   and are designed for gradient plots (e.g. [stat_gradientinterval()]) by ensuring that
#'   densities of 0 get mapped to 0 in the output.}
#'   \item{`scale_slab_linewidth_* `}{Slab outline line width}
#'   \item{`scale_slab_linetype_* `}{Slab outline line type}
#'   \item{`scale_slab_shape_* `}{Slab dot shape (for [geom_dotsinterval()])}
#' }
#'
#' See the corresponding scale documentation in ggplot for more information; e.g.
#' [`scale_color_discrete()`][ggplot2::scale_color_discrete],
#' [`scale_color_continuous()`][ggplot2::scale_color_continuous], etc.
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
#' These scales have been deprecated:
#'
#' \describe{
#'   \item{`scale_interval_size_* `}{Use `scale_linewidth_*`}
#'   \item{`scale_slab_size_* `}{Slab `scale_size_linewidth_*`}
#' }
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams ggplot2::scale_shape
#' @param ... Arguments passed to underlying scale or guide functions. E.g. `scale_point_color_discrete`
#' passes arguments to [`scale_color_discrete()`][ggplot2::scale_color_discrete]. See those functions for more details.
#' @param aesthetics <[character]> Names of aesthetics to set scales for.
#' @param guide <[Guide][ggplot2::Guide] | [string][character]> Guide to use for legends for an aesthetic.
#' @param range <length-2 [numeric]> The minimum and maximum size of the plotting symbol
#' after transformation.
#' @param na.translate <scalar [logical]> In discrete scales, should we show missing values?
#' @param na.value <[linetype][ggplot2::linetype]> When `na.translate` is `TRUE`, what value should be shown?
#' @return
#' A [ggplot2::Scale] representing one of the aesthetics used to target the appearance of specific parts of composite
#' `ggdist` geoms. Can be added to a [`ggplot()`][ggplot2::ggplot] object.
#' @name sub-geometry-scales
#' @aliases scales
#' @author Matthew Kay
#' @family ggdist scales
#' @seealso Other ggplot2 scales: [`scale_color_discrete()`][ggplot2::scale_color_discrete],
#' [`scale_color_continuous()`][ggplot2::scale_color_continuous], etc.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' # This plot shows how to set multiple specific aesthetics
#' # NB it is very ugly and is only for demo purposes.
#' data.frame(distribution = "Normal(1,2)") %>%
#'   parse_dist(distribution) %>%
#'   ggplot(aes(y = distribution, xdist = .dist, args = .args)) +
#'   stat_halfeye(
#'     shape = 21,  # this point shape has a fill and outline
#'     point_color = "red",
#'     point_fill = "black",
#'     point_alpha = .1,
#'     point_size = 6,
#'     stroke = 2,
#'     interval_color = "blue",
#'     # interval line widths are scaled from [1, 6] onto [0.6, 1.4] by default
#'     # see the interval_size_range parameter in help("geom_slabinterval")
#'     linewidth = 8,
#'     interval_linetype = "dashed",
#'     interval_alpha = .25,
#'     # fill sets the fill color of the slab (here the density)
#'     slab_color = "green",
#'     slab_fill = "purple",
#'     slab_linewidth = 3,
#'     slab_linetype = "dotted",
#'     slab_alpha = .5
#'   )
#'
#' @export
scale_point_colour_discrete =
  function(..., aesthetics = "point_colour") scale_colour_hue(..., aesthetics = aesthetics)
#' @rdname sub-geometry-scales
#' @export
scale_point_color_discrete = scale_point_colour_discrete
#' @rdname sub-geometry-scales
#' @export
scale_point_colour_continuous = function(..., aesthetics = "point_colour", guide = guide_colourbar2()) {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}
#' @rdname sub-geometry-scales
#' @export
scale_point_color_continuous = scale_point_colour_continuous


#' @rdname sub-geometry-scales
#' @export
scale_point_fill_discrete =
  function(..., aesthetics = "point_fill") scale_colour_hue(..., aesthetics = aesthetics)
#' @rdname sub-geometry-scales
#' @export
scale_point_fill_continuous = function(..., aesthetics = "point_fill", guide = guide_colourbar2()) {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}


#' @rdname sub-geometry-scales
#' @importFrom scales rescale_pal
#' @export
scale_point_alpha_continuous = function(..., range = c(0.1, 1)) {
  continuous_scale("point_alpha", palette = rescale_pal(range), ...)
}
#' @rdname sub-geometry-scales
#' @export
scale_point_alpha_discrete = function(..., range = c(0.1, 1)) {
  discrete_scale("point_alpha", palette = function(n) seq(range[1], range[2], length.out = n), ...)
}


#' @rdname sub-geometry-scales
#' @importFrom scales area_pal
#' @export
scale_point_size_continuous =
  function(..., range = c(1, 6)) continuous_scale("point_size", palette = area_pal(range), ...)
#' @rdname sub-geometry-scales
#' @export
scale_point_size_discrete = function(..., range = c(1, 6), na.translate = FALSE) {
  force(range)
  discrete_scale(
    "point_size", palette = function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...
  )
}


# scale_interval_... ------------------------------------------------------

#' @export
#' @rdname sub-geometry-scales
scale_interval_colour_discrete =
  function(..., aesthetics = "interval_colour") scale_colour_hue(..., aesthetics = aesthetics)
#' @rdname sub-geometry-scales
#' @export
scale_interval_color_discrete = scale_interval_colour_discrete
#' @rdname sub-geometry-scales
#' @export
scale_interval_colour_continuous = function(..., aesthetics = "interval_colour", guide = guide_colourbar2()) {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}
#' @rdname sub-geometry-scales
#' @export
scale_interval_color_continuous = scale_interval_colour_continuous


#' @rdname sub-geometry-scales
#' @importFrom scales rescale_pal
#' @export
scale_interval_alpha_continuous = function(..., range = c(0.1, 1)) {
  continuous_scale("interval_alpha", palette = rescale_pal(range), ...)
}
#' @rdname sub-geometry-scales
#' @export
scale_interval_alpha_discrete = function(..., range = c(0.1, 1)) {
  discrete_scale("interval_alpha", palette = function(n) seq(range[1], range[2], length.out = n), ...)
}


#' @rdname sub-geometry-scales
#' @importFrom scales area_pal
#' @export
scale_interval_size_continuous =
  function(..., range = c(1, 6)) continuous_scale("interval_size", palette = area_pal(range), ...)
#' @rdname sub-geometry-scales
#' @export
scale_interval_size_discrete = function(..., range = c(1, 6), na.translate = FALSE) {
  force(range)
  discrete_scale(
    "interval_size", palette = function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...
  )
}


#' @rdname sub-geometry-scales
#' @importFrom scales linetype_pal
#' @export
scale_interval_linetype_discrete = function(..., na.value = "blank") {
  discrete_scale("interval_linetype", palette = linetype_pal(), na.value = na.value, ...)
}
#' @rdname sub-geometry-scales
#' @export
scale_interval_linetype_continuous = function(...) {
  stop0("A continuous variable cannot be mapped to linetype")
}


# scale_slab_... ----------------------------------------------------------

#' @export
#' @rdname sub-geometry-scales
scale_slab_colour_discrete =
  function(..., aesthetics = "slab_colour") scale_colour_hue(..., aesthetics = aesthetics)
#' @rdname sub-geometry-scales
#' @export
scale_slab_color_discrete = scale_slab_colour_discrete
#' @rdname sub-geometry-scales
#' @export
scale_slab_colour_continuous = function(..., aesthetics = "slab_colour", guide = guide_colourbar2()) {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}
#' @rdname sub-geometry-scales
#' @export
scale_slab_color_continuous = scale_slab_colour_continuous


#' @rdname sub-geometry-scales
#' @export
scale_slab_fill_discrete =
  function(..., aesthetics = "slab_fill") scale_colour_hue(..., aesthetics = aesthetics)
#' @rdname sub-geometry-scales
#' @export
scale_slab_fill_continuous = function(..., aesthetics = "slab_fill", guide = guide_colourbar2()) {
  scale_colour_continuous(..., aesthetics = aesthetics, guide = guide)
}


#' @rdname sub-geometry-scales
#' @importFrom scales rescale_pal
#' @export
scale_slab_alpha_continuous = function(..., limits = function(l) c(min(0, l[[1]]), l[[2]]), range = c(0, 1)) {
  continuous_scale("slab_alpha", palette = rescale_pal(range), limits = limits, ...)
}
#' @rdname sub-geometry-scales
#' @export
scale_slab_alpha_discrete = function(..., range = c(0.1, 1)) {
  discrete_scale("slab_alpha", palette = function(n) seq(range[1], range[2], length.out = n), ...)
}


#' @rdname sub-geometry-scales
#' @importFrom scales rescale_pal
#' @export
scale_slab_size_continuous =
  function(..., range = c(1, 6)) continuous_scale("slab_size", palette = rescale_pal(range), ...)
#' @rdname sub-geometry-scales
#' @export
scale_slab_size_discrete = function(..., range = c(1, 6), na.translate = FALSE) {
  force(range)
  discrete_scale(
    "slab_size", palette = function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...
  )
}


#' @rdname sub-geometry-scales
#' @importFrom scales rescale_pal
#' @export
scale_slab_linewidth_continuous =
  function(..., range = c(1, 6)) continuous_scale("slab_linewidth", palette = rescale_pal(range), ...)
#' @rdname sub-geometry-scales
#' @export
scale_slab_linewidth_discrete = function(..., range = c(1, 6), na.translate = FALSE) {
  force(range)
  discrete_scale(
    "slab_linewidth", palette = function(n) seq(range[1], range[2], length.out = n),
    na.translate = na.translate, ...
  )
}


#' @rdname sub-geometry-scales
#' @importFrom scales linetype_pal
#' @export
scale_slab_linetype_discrete = function(..., na.value = "blank") {
  discrete_scale("slab_linetype", palette = linetype_pal(), na.value = na.value, ...)
}
#' @rdname sub-geometry-scales
#' @export
scale_slab_linetype_continuous = function(...) {
  stop0("A continuous variable cannot be mapped to linetype")
}


#' @rdname sub-geometry-scales
#' @importFrom scales shape_pal
#' @export
scale_slab_shape_discrete = function(..., solid = TRUE) {
  discrete_scale("slab_shape", palette = shape_pal(solid), ...)
}
#' @rdname sub-geometry-scales
#' @export
scale_slab_shape_continuous = function(...) {
  stop0("A continuous variable cannot be mapped to shape")
}


# guide_colorbar2 ---------------------------------------------------------

#' @rdname sub-geometry-scales
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

#' @rdname sub-geometry-scales
#' @export
guide_colorbar2 = guide_colourbar2
