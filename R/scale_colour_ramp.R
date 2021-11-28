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
#' @param aesthetics Names of aesthetics to set scales for.
#' @param range a numeric vector of length 2 that specifies the minimum and maximum
#' values after the scale transformation. These values should be between `0`
#' (the `from` color) and `1` (the color determined by the `fill` aesthetic).
#' @return
#' A [ggplot2::Scale] representing a scale for the `colour_ramp` and/or `fill_ramp`
#' aesthetics for `ggdist` geoms. Can be added to a [ggplot()] object.
#' @name scale_colour_ramp
#' @aliases scale_color_ramp scale_fill_ramp
#' @author Matthew Kay
#' @family ggdist scales
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' library(distributional)
#'
#' tibble(d = dist_uniform(0, 1)) %>%
#'   ggplot(aes(y = 0, xdist = d)) +
#'   stat_slab(aes(fill_ramp = stat(x)))
#'
#' tibble(d = dist_uniform(0, 1)) %>%
#'   ggplot(aes(y = 0, xdist = d)) +
#'   stat_slab(aes(fill_ramp = stat(x)), fill = "blue") +
#'   scale_fill_ramp_continuous(from = "red")
#'
#' # you can invert the order of `range` to change the order of the blend
#' tibble(d = dist_normal(0, 1)) %>%
#'   ggplot(aes(y = 0, xdist = d)) +
#'   stat_slab(aes(fill_ramp = stat(cut_cdf_qi(cdf))), fill = "blue") +
#'   scale_fill_ramp_discrete(from = "red", range = c(1, 0))
#'
#' @importFrom scales rescale_pal
#' @export
scale_colour_ramp_continuous = function(
  from = "white", ..., limits = function(l) c(min(0, l[[1]]), l[[2]]), range = c(0, 1),
  aesthetics = "colour_ramp"
) {
  continuous_scale(
    aesthetics, "colour_ramp_c", colour_ramp_pal(range, from), limits = limits, ...
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
  scale = discrete_scale(
    aesthetics, "colour_ramp_d", colour_ramp_pal_discrete(range, from), ...
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


# helpers -----------------------------------------------------------------

#' @importFrom scales rescale
colour_ramp_pal = function(range, from) {
  force(range)
  force(from)
  function(x) {
    # this is a stupid hack so we can pass the color through
    # surely there is a better way?
    lapply(rescale(x, range, c(0, 1)), function(y) {
      attr(y, "from") = from
      y
    })
  }
}

colour_ramp_pal_discrete = function(range, from) {
  force(range)
  force(from)
  function(n) {
    # this is a stupid hack so we can pass the color through
    # surely there is a better way?
    lapply(seq(range[1], range[2], length.out = n), function(y) {
      attr(y, "from") = from
      y
    })
  }
}

# assuming equal-length vectors `colors` and `amounts`, where `colors` are
# colors and `amounts` is a scaled ramp aesthetic column, returns a vector
# of same length as input giving the transformed (ramped) colors. E.g.
# inside a draw_group() or draw_panel() method of a geom, usage might be:
# data$fill = apply_colour_ramp(data$fill, data$fill_ramp)
# to apply the effects of the fill_ramp aesthetic to the fill aesthetic.
apply_colour_ramp = function(colors, amounts) {
  if (is.null(colors) || is.null(amounts)) return(colors)

  map2_chr_(colors, amounts, function(color, amount) {
    # null amounts come from missing values
    amount = amount %||% NA
    from = attr(amount, "from") %||% "white"
    scales::seq_gradient_pal(from, color)(amount)
  })
}
