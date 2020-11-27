# fill_ramp scale for applying a secondary ramp to the fill color scale
#
# Author: mjskay
###############################################################################


#' Secondary ggplot fill scale that ramps to another color
#'
#' This scale modifies the fill color of geoms that support it ([geom_lineribbon()]
#' and [geom_slabinterval()]) to "ramp" from a secondary color (by default white)
#' to the primary fill color (determined by the standard `fill` aesthetic).
#'
#' @inheritParams ggplot2::continuous_scale
#' @param from The color to ramp from. Corresponds to `0` on the scale.
#' @param ... Arguments passed to underlying scale or guide functions. E.g. `scale_fill_ramp_discrete()`,
#' passes arguments to [discrete_scale()], `scale_fill_ramp_continous()` passes arguments
#' to [continous_scale()]. See those functions for more details.
#' @param aesthetics Names of aesthetics to set scales for.
#' @param range a numeric vector of length 2 that specifies the minimum and maximum
#' values after the scale transformation. These values should be between `0`
#' (the `from` color) and `1` (the color determined by the `fill` aesthetic).
#' @return
#' A [ggplot2::Scale] representing a scale for the `fill_ramp` aesthetic for
#' `ggdist` geoms. Can be added to a [ggplot()] object.
#' @name scale_fill_ramp
#' @author Matthew Kay
#' @seealso [discrete_scale()], [continous_scale()].
#' @examples
#'
#' TODO
#'
#' @importFrom scales rescale_pal
#' @export
scale_fill_ramp_continuous = function(from = "white", ..., limits = function(l) c(min(0, l[[1]]), l[[2]]), range = c(0, 1)) {
  continuous_scale(
    "fill_ramp", "fill_ramp_c", fill_ramp_pal(range, from), limits = limits, ...
  )
}
#' @rdname scale_fill_ramp
#' @export
scale_fill_ramp_discrete = function(from = "white", ..., range = c(0.1, 1)) {
  scale = discrete_scale(
    "fill_ramp", "fill_ramp_d", fill_ramp_pal_discrete(range, from), ...
  )
}

#' @importFrom scales rescale
fill_ramp_pal = function(range, from) {
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

fill_ramp_pal_discrete = function(range, from) {
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
# data$fill = apply_color_ramp(data$fill, data$fill_ramp)
# to apply the effects of the fill_ramp aesthetic to the fill aesthetic.
#' @importFrom scales seq_gradient_pal
#' @importFrom purrr map2_chr
apply_color_ramp = function(colors, amounts) {
  if (is.null(colors) || is.null(amounts)) return(colors)

  map2(colors, amounts, function(color, amount) {
    from = attr(amount, "from") %||% "white"
    seq_gradient_pal(from, color)(amount)
  })
}
