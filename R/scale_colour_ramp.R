# colour_ramp scale for applying a secondary ramp to the fill color scale
#
# Author: mjskay
###############################################################################


# scale_colour_ramp -------------------------------------------------------

#' Secondary color scale that ramps from another color (ggplot2 scale)
#'
#' This scale creates a secondary scale that modifies the `fill` or `color` scale of
#' geoms that support it ([geom_lineribbon()] and [geom_slabinterval()]) to "ramp"
#' from a secondary color (by default white) to the primary fill color (determined
#' by the standard `color` or `fill` aesthetics). It uses the
#' [partial_colour_ramp()] data type.
#'
#' @inheritParams ggplot2::continuous_scale
#' @param from The color to ramp from. Corresponds to `0` on the scale.
#' @param ... Arguments passed to underlying scale or guide functions. E.g.
#' [scale_colour_ramp_discrete()] passes arguments to [`discrete_scale()`][ggplot2::discrete_scale],
#' [scale_colour_ramp_continuous()] passes arguments to [`continuous_scale()`][ggplot2::continuous_scale].
#' See those functions for more details.
#' @param guide A function used to create a guide or its name. For
#' [scale_colour_ramp_continuous()] and [scale_fill_ramp_continuous()],
#' [guide_rampbar()] can be used to create gradient color bars. See
#' [`guides()`][ggplot2::guides] for information on other guides.
#' @param aesthetics Names of aesthetics to set scales for.
#' @param range a numeric vector of length 2 that specifies the minimum and maximum
#' values after the scale transformation. These values should be between `0`
#' (the `from` color) and `1` (the color determined by the `fill` aesthetic).
#' @details
#' These scales transform data into [`partial_colour_ramp`]s. Each [`partial_colour_ramp`]
#' is a pair of two values: a `from` colour and a numeric `amount` between `0`
#' and `1` representing a distance between `from` and the target color (where `0`
#' indicates the `from` color and `1` the target color).
#'
#' The target color is determined by the corresponding aesthetic: for example,
#' the `colour_ramp` aesthetic creates ramps between `from` and whatever the
#' value of the `colour` aesthetic is; the `fill_ramp` aesthetic creates ramps
#' between `from` and whatever the value of the `fill` aesthetic is. When the
#' `colour_ramp` aesthetic is set, \pkg{ggdist} geometries will modify their
#' `colour` by applying the colour ramp between `from` and `colour` (and
#' similarly for `fill_ramp` and `fill`).
#'
#' Colour ramps can be applied (i.e. translated into colours) using
#' [ramp_colours()], which can be used with [partial_colour_ramp()]
#' to implement geoms that make use of `colour_ramp` or `fill_ramp` scales.
#' @return
#' A [ggplot2::Scale] representing a scale for the `colour_ramp` and/or `fill_ramp`
#' aesthetics for `ggdist` geoms. Can be added to a [`ggplot()`][ggplot2::ggplot] object.
#' @name scale_colour_ramp
#' @aliases scale_color_ramp scale_fill_ramp
#' @author Matthew Kay
#' @family ggdist scales
#' @family colour ramp functions
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' library(distributional)
#'
#' tibble(d = dist_uniform(0, 1)) %>%
#'   ggplot(aes(y = 0, xdist = d)) +
#'   stat_slab(aes(fill_ramp = after_stat(x)))
#'
#' tibble(d = dist_uniform(0, 1)) %>%
#'   ggplot(aes(y = 0, xdist = d)) +
#'   stat_slab(aes(fill_ramp = after_stat(x)), fill = "blue") +
#'   scale_fill_ramp_continuous(from = "red")
#'
#' # you can invert the order of `range` to change the order of the blend
#' tibble(d = dist_normal(0, 1)) %>%
#'   ggplot(aes(y = 0, xdist = d)) +
#'   stat_slab(aes(fill_ramp = after_stat(cut_cdf_qi(cdf))), fill = "blue") +
#'   scale_fill_ramp_discrete(from = "red", range = c(1, 0))
#'
#' @importFrom scales rescale_pal
#' @export
scale_colour_ramp_continuous = function(
  from = "white", ..., limits = function(l) c(min(0, l[[1]]), l[[2]]), range = c(0, 1),
  guide = "legend", aesthetics = "colour_ramp"
) {
  scale = ggproto(NULL, ScaleColourRampContinuous, from = from)
  continuous_scale(
    aesthetics, palette = rescale_pal(range), limits = limits, guide = guide, ..., super = scale
  )
}

#' @rdname scale_colour_ramp
#' @export
scale_color_ramp_continuous = scale_colour_ramp_continuous

#' @rdname scale_colour_ramp
#' @export
scale_colour_ramp_discrete = function(
  from = "white", ..., range = c(0.2, 1),
  aesthetics = "colour_ramp"
) {
  scale = ggproto(NULL, ScaleColourRampDiscrete, from = from)
  discrete_scale(
    aesthetics, palette = function(n) seq(range[1], range[2], length.out = n), ..., super = scale
  )
}

#' @rdname scale_colour_ramp
#' @export
scale_color_ramp_discrete = scale_colour_ramp_discrete


# scale_fill_ramp ---------------------------------------------------------

#' @rdname scale_colour_ramp
#' @export
scale_fill_ramp_continuous = function(..., aesthetics = "fill_ramp") {
  scale_colour_ramp_continuous(..., aesthetics = aesthetics)
}
#' @rdname scale_colour_ramp
#' @export
scale_fill_ramp_discrete = function(..., aesthetics = "fill_ramp") {
  scale_colour_ramp_discrete(..., aesthetics = aesthetics)
}


# continuous scale --------------------------------------------------------

ScaleColourRampContinuous = ggproto("ScaleColourRampContinuous", ScaleContinuous,
  from = "white",

  map = function(self, x, limits = self$get_limits()) {
    out = ggproto_parent(ScaleContinuous, self)$map(x, limits)
    partial_colour_ramp(out, self$from)
  }
)


# discrete scale --------------------------------------------------------

ScaleColourRampDiscrete = ggproto("ScaleColourRampDiscrete", ScaleDiscrete,
  from = "white",

  map = function(self, x, limits = self$get_limits()) {
    out = ggproto_parent(ScaleDiscrete, self)$map(x, limits)
    partial_colour_ramp(out, self$from)
  }
)
