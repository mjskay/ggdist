# Geom for weighted dotplots
#
# Author: mjskay
###############################################################################


# grob construction -------------------------------------------------------

## make_weighted_points_grob -------------------------------------------------
make_weighted_points_grob = auto_partial(name = "make_weighted_points_grob", function(
  x,
  y,
  weight = NULL,
  ...,  # ignored
  pch = 21,
  col = "gray65",
  fill = "gray65",
  fontsize = 11,
  lwd = 1,
  lty = "solid",
  axis = "x"
) {
  size = unit(fontsize / font_size_ratio, "points")
  weight = weight %||% rep(1, length(x))

  grobs = pmap_(list(x, y, weight, fill, lwd, lty, pch), function(x, y, weight, fill, lwd, lty, pch) {
    # TODO: do something with shape
    shape = translate_weighted_shape(pch)

    h = size * weight
    w = size
    switch(shape,
      circle = roundrectGrob(
        r = min(unit(0.5, "snpc"), unit(3, "pt")),
        gp = gpar(col = col, fill = fill, lwd = lwd, lty = lty),
        vp = viewport(
          x = x, y = y,
          height = if (axis == "x") h else w,
          width = if (axis == "x") w else h
        )
      ),
      square = rectGrob(
        x = x, y = y,
        height = if (axis == "x") h else w,
        width = if (axis == "x") w else h,
        gp = gpar(col = col, fill = fill, lwd = lwd, lty = lty)
      )
    )
  })

  do.call(grobTree, grobs)
})

#' Translate a pch into a shape for use with a weighted dotplot
#' @param shape a `pch`-style shape (number or single letter)
#' @returns `"square"` or `"circle"`
#' @noRd
translate_weighted_shape = function(shape) {
  if (shape %in% c(0, 15, 22)) {
    "square"
  } else if (shape %in% c(1, 16, 19, 20, 21)) {
    "circle"
  } else {
    cli_abort(
      "Only circle (1, 16, 19, 20, 21) and square (0, 15, 22)
      shapes are supported by {.help ggdist::geom_weighted_dots}.",
      class = "ggdist_invalid_weighted_dot_shape"
    )
  }
}

# geom_weighted_dots ----------------------------------------------------------
#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomWeightedDots = ggproto("GeomWeightedDots", GeomDots,

  ## aesthetics --------------------------------------------------------------

  aes_docs = {
    aes_docs = GeomDots$aes_docs
    dots_aes_i = which(startsWith(names(aes_docs), "Dots-specific"))
    aes_docs[[dots_aes_i]] = defaults(list(
      weight = 'The weight associated with each dot, where `1` is a normal-sized dot.'
    ), aes_docs[[dots_aes_i]])
    aes_docs
  },

  hidden_aes = union("family", GeomDots$hidden_aes),

  default_aes = defaults(aes(
    weight = 1
  ), GeomDots$default_aes),

  ## params ------------------------------------------------------------------

  hidden_params = union("layout", GeomDots$hidden_params),

  setup_params = function(self, data, params) {
    params = ggproto_parent(GeomDots, self)$setup_params(data, params)

    stopifnot(params$layout == "bin")

    params
  },

  ## other methods -----------------------------------------------------------

  points_grob_factory = function(...) make_weighted_points_grob(...)
)

#' @title Weighted dot plot (geom)
#' @description
#' Variant of [geom_dots()] for creating weighted dotplots. Accepts the `weight`
#' aesthetic that gives the relative size of each dot (where `1` is a normal-size
#' dot). Unlike [geom_dots()], this geom only supports circular and square
#' `shape`s, and can only use a Wilkinson binning layout (`layout = "bin"`).
#' @eval rd_dotsinterval_shortcut_geom(
#'   "weighted_dots", "weighted dot", title = FALSE, describe = FALSE, examples = FALSE
#' )
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' theme_set(theme_ggdist())
#'
#' set.seed(1234)
#' x = rnorm(1000)
#'
#' # TODO
#' @export
geom_weighted_dots = make_geom(GeomWeightedDots)
