# colour_ramp scale for applying a secondary ramp to the fill color scale
#
# Author: mjskay
###############################################################################


# scale_colour_ramp -------------------------------------------------------

#' Secondary ggplot color scale that ramps from another color
#'
#' This scale creates a secondary scale that modifies the `fill` or `color` scale of
#' geoms that support it ([geom_lineribbon()] and [geom_slabinterval()]) to "ramp"
#' from a secondary color (by default white) to the primary fill color (determined
#' by the standard `color` or `fill` aesthetics).
#'
#' @inheritParams ggplot2::continuous_scale
#' @param from The color to ramp from. Corresponds to `0` on the scale.
#' @param ... Arguments passed to underlying scale or guide functions. E.g. [scale_colour_ramp_discrete()],
#' passes arguments to [discrete_scale()], [scale_colour_ramp_continuous()] passes arguments
#' to [continuous_scale()]. See those functions for more details.
#' @param guide A function used to create a guide or its name. For `scale_colour_ramp_continuous()`
#' and `scale_fill_ramp_continuous()`, `guide_rampbar()` can be used to create gradient
#' color bars. See `guides()` for information on other guides.
#' @param aesthetics Names of aesthetics to set scales for.
#' @param range a numeric vector of length 2 that specifies the minimum and maximum
#' values after the scale transformation. These values should be between `0`
#' (the `from` color) and `1` (the color determined by the `fill` aesthetic).
#' @details
#' These scales transform data into values between `0` and `1` that represent a
#' color between `from` and the target color. The target color is determined by
#' the aesthetic: for example, the `colour_ramp` aesthetic creates ramps between
#' `from` and whatever the value of the `colour` aesthetic is. Then, if the
#' `colour_ramp` aesthetic is set, \pkg{ggdist} geometries will modify their
#' `colour` by applying the colour ramp between `from` and `colour`.
#'
#' [partial_colour_ramp()] is used by these scales to create `numeric()`-like
#' objects marked as being a partial colour ramp from the colour `from`.
#' @return
#' A [ggplot2::Scale] representing a scale for the `colour_ramp` and/or `fill_ramp`
#' aesthetics for `ggdist` geoms. Can be added to a [ggplot()] object.
#' @name scale_colour_ramp
#' @aliases scale_color_ramp scale_fill_ramp
#' @author Matthew Kay
#' @family ggdist scales
#' @seealso [guide_rampbar()]
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
    aesthetics, "colour_ramp_c", rescale_pal(range), limits = limits, guide = guide, ..., super = scale
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
    aesthetics, "colour_ramp_d", function(n) seq(range[1], range[2], length.out = n), ..., super = scale
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


# partial_colour_ramp datatype --------------------------------------------

new_partial_colour_ramp = function(amount = double(), from = "white") {
  if (length(amount) < 1) x = double()
  stopifnot(is.double(amount))
  if (length(from) <= 1) from = rep(from, length(amount))
  stopifnot(is.character(from))
  vctrs::new_rcrd(list(amount = amount, from = from), class = "ggdist_partial_colour_ramp")
}

#' @rdname scale_colour_ramp
#' @param amount double in `[0, 1]` giving the amount to ramp from
#' the `from` colour (`0` == `from` and `1` == the other endpoint).
partial_colour_ramp = function(amount = double(), from = "white") {
  amount = vctrs::vec_cast(amount, numeric())
  from = vctrs::vec_cast(from, character())
  new_partial_colour_ramp(amount, from)
}


# formatting --------------------------------------------------------------

#' @export
vec_ptype_full.ggdist_partial_colour_ramp = function(x, ...) "partial_colour_ramp"
#' @export
vec_ptype_abbr.ggdist_partial_colour_ramp = function(x, ...) "rmp"

#' @export
format.ggdist_partial_colour_ramp = function(x, ...) {
  sprintf("[%s from %s]", field(x, "amount"), field(x, "from"))
}


# predicates --------------------------------------------------------------

#' @export
is.na.ggdist_partial_colour_ramp = function(x) {
  is.na(field(x, "amount")) | is.na(field(x, "from"))
}


# casting -------------------------------------------------------

as_partial_colour_ramp = function(x) {
  vec_cast(x, new_partial_colour_ramp())
}

#' @export
vec_ptype2.ggdist_partial_colour_ramp.ggdist_partial_colour_ramp = function(x, y, ...) new_partial_colour_ramp()

#' @export
vec_ptype2.ggdist_partial_colour_ramp.double = function(x, y, ...) new_partial_colour_ramp()
#' @export
vec_ptype2.double.ggdist_partial_colour_ramp = function(x, y, ...) new_partial_colour_ramp()
#' @export
vec_ptype2.ggdist_partial_colour_ramp.integer = function(x, y, ...) new_partial_colour_ramp()
#' @export
vec_ptype2.integer.ggdist_partial_colour_ramp = function(x, y, ...) new_partial_colour_ramp()

#' @export
vec_cast.ggdist_partial_colour_ramp.double = function(x, to, ...) partial_colour_ramp(x)
#' @export
vec_cast.ggdist_partial_colour_ramp.integer = function(x, to, ...) partial_colour_ramp(x)
#' @export
vec_cast.double.ggdist_partial_colour_ramp = function(x, to, ...) field(x, "amount")
#' @export
vec_cast.integer.ggdist_partial_colour_ramp = function(x, to, ...) as.integer(field(x, "amount"))


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


# applying color ramps --------------------------------------------------------

#' Assuming equal-length vectors `colors` and `ramps`, where `colors` are
#' colors and `ramps` is a partial colour ramp aesthetic column, returns
#' a vector of same length as input giving the transformed (ramped) colors.
#'
#' E.g. inside a `draw_group()` or `draw_panel()` method of a geom,
#' usage might be:
#'
#'  data$fill = apply_colour_ramp(data$fill, data$fill_ramp)
#'
#' to apply the effects of the fill_ramp aesthetic to the fill aesthetic.
#' @param colors character vector of colours
#' @param ramps `partial_colour_ramp` vector
#' @noRd
apply_colour_ramp = function(colors, ramps) {
  if (is.null(colors) || is.null(ramps)) return(colors)
  ramps = vec_cast(ramps, new_partial_colour_ramp())

  map2_chr_(colors, ramps, function(color, ramp) {
    scales::seq_gradient_pal(field(ramp, "from"), color)(field(ramp, "amount"))
  })
}
