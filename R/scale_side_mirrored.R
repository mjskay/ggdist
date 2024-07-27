# side scale for mirrored slabs
#
# Author: mjskay
###############################################################################


# scale_side_mirrored -------------------------------------------------------

#' Side scale for mirrored slabs (ggplot2 scale)
#'
#' This scale creates mirrored slabs for the `side` aesthetic of the [geom_slabinterval()]
#' and [geom_dotsinterval()] family of geoms and stats. It works on discrete variables
#' of two or three levels.
#'
#' @inheritParams ggplot2::discrete_scale
#' @param start <[string][character]> The side to start from. Can be any valid value of the `side` aesthetic
#' except `"both"`.
#' @inheritDotParams ggplot2::discrete_scale
#' @param aesthetics <[character]> Names of aesthetics to set scales for.
#' @return
#' A [ggplot2::Scale] representing a scale for the `side`
#' aesthetic for \pkg{ggdist} geoms. Can be added to a [`ggplot()`][ggplot2::ggplot] object.
#' @author Matthew Kay
#' @family ggdist scales
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' set.seed(1234)
#' data.frame(
#'   x = rnorm(400, c(1,4)),
#'   g = c("a","b")
#' ) %>%
#'   ggplot(aes(x, fill = g, side = g)) +
#'   geom_weave(linewidth = 0, scale = 0.5) +
#'   scale_side_mirrored()
#'
#' @export
scale_side_mirrored = function(
  start = "topright", ..., aesthetics = "side"
) {
  discrete_scale(
    aesthetics, palette = side_mirrored_pal(start), ...
  )
}


# helpers -----------------------------------------------------------------

side_mirrored_pal = function(start) {
  force(start)
  function(x) {
    switch(x,
      start,
      c(start, opposite_side(start)),
      c(start, "both", opposite_side(start)),
      stop0("scale_side_mirrored() cannot be used with more than 3 levels; got: ", x)
    )
  }
}

opposite_sides = c(
  "top" = "bottom",
  "bottom" = "top",
  "left" = "right",
  "right" = "left",
  "topright" = "bottomleft",
  "topleft" = "bottomright",
  "bottomright" = "topleft",
  "bottomleft" = "topright"
)

opposite_side = function(side) {
  if (!side %in% names(opposite_sides)) {
    stop0("`", deparse0(side), "` is not a valid side for use with scale_side_mirrored()")
  }
  unname(opposite_sides[side])
}
