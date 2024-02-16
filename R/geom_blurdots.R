# Geom for blurry dotplots
#
# Author: mjskay
###############################################################################


# blur functions ---------------------------------------------------------------

blur_type_gaussian = function(x, r, sd) {
  pnorm(x + r, 0, sd) - pnorm(x - r, 0, sd)
}

blur_type_interval = function(x, r, sd) {
  ifelse(
    x < r, 1,
    ifelse(
      x < 2 * sd, 0.5,
      0
    )
  )
}


# grob construction -------------------------------------------------------

make_blurry_points_grob = function(
  x = 0.5,
  y = 0.5,
  ...,  # ignored
  col = "gray65",
  fill = "gray65",
  fontsize = 11,
  lwd = 1,
  lty = "solid",
  axis = "x",
  sd = 0,
  n = 100,
  blur_type = blur_type_gaussian
) {
  # ensure r and sd are in the same units -- that way when we apply the blur function
  # (which only takes numerics) everything will line up correctly
  r = unit(fontsize / font_size_ratio / 2, "points")
  sd = convertUnit(unit(sd %||% 0, "native"), unitTo = "points", axisFrom = axis, typeFrom = "dimension")

  grobs = .mapply(list(x, y, fill, sd, lwd, lty), NULL, FUN = function(x, y, fill, sd, lwd, lty) {
    blur_width = 2 * sd + r
    blur_x = seq(0, as.numeric(blur_width), length.out = n)
    grad_colors = alpha(fill, c(blur_type(blur_x, as.numeric(r), as.numeric(sd)), 0))
    grad = radialGradient(grad_colors, r2 = blur_width)

    h = 2 * r
    w = 2 * blur_width
    blurry_fill = rectGrob(
      x = x, y = y,
      height = if (axis == "x") h else w,
      width = if (axis == "x") w else h,
      gp = gpar(fill = grad, col = NA)
    )
    outline = circleGrob(
      x = x, y = y, r = r,
      gp = gpar(col = col, fill = NA, lwd = lwd, lty = lty)
    )

    grobTree(blurry_fill, outline)
  })

  do.call(grobTree, grobs)
}

# geom_blur_dots ----------------------------------------------------------
#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomBlurDots = ggproto("GeomBlurDots", GeomDots,

  ## aesthetics --------------------------------------------------------------

  aes_docs = {
    aes_docs = GeomDots$aes_docs
    dots_aes_i = which(startsWith(names(aes_docs), "Dots-specific"))
    aes_docs[[dots_aes_i]] = defaults(list(
      blur = 'The blur associated with each dot, expressed as a standard deviation in data units.'
    ), aes_docs[[dots_aes_i]])
    aes_docs
  },

  hidden_aes = union("shape", GeomDots$hidden_aes),

  default_aes = defaults(aes(
    blur = 0
  ), GeomDots$default_aes),

  default_key_aes = defaults(aes(
    colour = NA
  ), GeomDots$default_key_aes),

  ## other methods -----------------------------------------------------------

  setup_data = function(self, data, params) {
    define_orientation_variables(params$orientation)

    data = ggproto_parent(GeomDots, self)$setup_data(data, params)

    # add an xmin/xmax to dots based on blur sd so that the full extent of
    # blurred dots is drawn
    data[["blur"]] = data[["blur"]] %||% params$blur
    if (!is.null(data[["blur"]])) {
      slab_i = which(data$datatype == "slab")
      data[slab_i, xmin] = data[slab_i, x] - 2 * data[slab_i, "blur"]
      data[slab_i, xmax] = data[slab_i, x] + 2 * data[slab_i, "blur"]
    }

    data
  },

  draw_slabs = function(self, s_data, panel_params, coord, orientation, ...) {
    define_orientation_variables(orientation)

    if (!is.null(s_data[["blur"]])) {
      # blur is expressed in terms of data coordinates, need to translate
      # into standardized space
      xscale = max(panel_params[[x.range]]) - min(panel_params[[x.range]])
      s_data$blur = s_data$blur / xscale
      s_data$blur[is.na(s_data$blur)] = 0
    }

    ggproto_parent(GeomDots, self)$draw_slabs(s_data, panel_params, coord, orientation, ...)
  },

  make_points_grob = make_blurry_points_grob
)

#' @title Blurry dot plot (geom)
#' @description
#' Variant of [geom_dots()] for creating blurry dotplots. Accepts a `blur`
#' aesthetic that gives the standard deviation of the blur applied to the dots.
#' Requires a graphics engine supporting radial gradients. Unlike [geom_dots()],
#' all dots must be circular, so this geom does not support the `shape` aesthetic.
#' @eval rd_dotsinterval_shortcut_geom("blur_dots", "blurry dot", title = FALSE, describe = FALSE)
#' @export
geom_blur_dots = make_geom(GeomBlurDots)
