# modified colorbar guide for color/fill ramp scales
#
# Author: mjskay
###############################################################################


# guide_rampbar -------------------------------------------------------

#' Continuous colour ramp guide
#'
#' A colour ramp bar guide that shows continuous colour ramp scales mapped onto
#' values as a smooth gradient. Designed for use with [scale_fill_ramp_continuous()]
#' and [scale_colour_ramp_continuous()]. Based on [guide_colourbar()].
#'
#' @inheritDotParams ggplot2::guide_colourbar
#' @param to The color to ramp to in the guide. Corresponds to `1` on the scale.
#' @param available_aes A vector of character strings listing the aesthetics for which a `guide_rampbar()` can be drawn.
#' @details
#' This guide creates smooth gradient color bars for use with [scale_fill_ramp_continuous()]
#' and [scale_colour_ramp_continuous()]. The color to ramp from is determined by the `from`
#' argument of the `scale_*` function, and the color to ramp to is determined by the `to` argument
#' to [guide_rampbar()].
#'
#' Guides can be specified in each `scale_*` function or in `guides()`.
#' `guide = "rampbar"` in `scale_*` is syntactic sugar for `guide = guide_rampbar()`;
#' e.g. `scale_colour_ramp_continuous(guide = "rampbar")`. For how to specify
#' the guide for each scale in more detail, see `guides()`.
#' @return
#' A guide object.
#' @author Matthew Kay
#' @seealso [scale_fill_ramp_continuous()], [scale_colour_ramp_continuous()].
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#' library(distributional)
#'
#' # The default guide for ramp scales is guide_legend(), which creates a
#' # discrete style scale:
#' tibble(d = dist_uniform(0, 1)) %>%
#'   ggplot(aes(y = 0, xdist = d)) +
#'   stat_slab(aes(fill_ramp = after_stat(x)), fill = "blue") +
#'   scale_fill_ramp_continuous(from = "red")
#'
#' # We can guide_rampbar() to instead create a continuous guide, but
#' # it does not know what ccolor to ramp to (defaults to "gray65"):
#' tibble(d = dist_uniform(0, 1)) %>%
#'   ggplot(aes(y = 0, xdist = d)) +
#'   stat_slab(aes(fill_ramp = after_stat(x)), fill = "blue") +
#'   scale_fill_ramp_continuous(from = "red", guide = guide_rampbar())
#'
#' # We can tell the guide what color to ramp to using the `to` argument:
#' tibble(d = dist_uniform(0, 1)) %>%
#'   ggplot(aes(y = 0, xdist = d)) +
#'   stat_slab(aes(fill_ramp = after_stat(x)), fill = "blue") +
#'   scale_fill_ramp_continuous(from = "red", guide = guide_rampbar(to = "blue"))
#'
#' @export
guide_rampbar = function(..., to = "gray65", available_aes = c("fill_ramp", "colour_ramp")) {
  guide = guide_colourbar(..., available_aes = available_aes)
  if (inherits(guide, "GuideColourbar")) {
    # If ggplot2 >3.4.2, guides are written ggproto, so here we inherit from
    # the colourbar guide
    new_guide = ggproto(
      "GuideRampbar", guide,
      params = c(list(to = to), guide$params),
      extract_decor = function(scale, aesthetic, nbin = 300,
                               reverse = FALSE, to = "gray65", ...) {
        bar = guide$extract_decor(scale, aesthetic, nbin, reverse, ...)
        bar$colour = apply_colour_ramp(to, bar$colour)
        bar
      }
    )
    return(new_guide)
  }
  guide$to = to
  class(guide) = c("guide", "rampbar", "colorbar")
  guide
}

#' @export
guide_train.rampbar = function(guide, scale, aesthetic = NULL) {
  guide = NextMethod()
  guide$bar$colour = apply_colour_ramp(guide$to, guide$bar$colour)
  guide
}
