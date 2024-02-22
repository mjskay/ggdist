# Geom for blurry dotplots
#
# Author: mjskay
###############################################################################


# grob construction -------------------------------------------------------

# avoid NOTE on R < 4.1 for the use of radialGradient below
if (getRversion() < "4.1") globalVariables("radialGradient")

make_blurry_points_grob = auto_partial(name = "make_blurry_points_grob", function(
  x,
  y,
  ...,  # ignored
  col = "gray65",
  fill = "gray65",
  fontsize = 11,
  lwd = 1,
  lty = "solid",
  axis = "x",
  sd = 0,
  n = 100,
  blur = blur_gaussian
) {
  # ensure r and sd are in the same units -- that way when we apply the blur function
  # (which only takes numerics) everything will line up correctly
  r = unit(fontsize / font_size_ratio / 2, "points")
  sd = convertUnit(unit(sd %||% 0, "native"), unitTo = "points", axisFrom = axis, typeFrom = "dimension")

  grobs = .mapply(list(x, y, fill, sd, lwd, lty), NULL, FUN = function(x, y, fill, sd, lwd, lty) {
    blur_width = 2 * sd + r
    blur_x = seq(0, as.numeric(blur_width), length.out = n)
    grad_colors = alpha(fill, c(blur(blur_x, as.numeric(r), as.numeric(sd)), 0))
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
})

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
      sd = 'The standard deviation (in data units) of the blur associated with each dot.'
    ), aes_docs[[dots_aes_i]])
    aes_docs
  },

  hidden_aes = union(c(
    "shape",
    "family"
  ), GeomDots$hidden_aes),

  default_aes = defaults(aes(
    sd = 0
  ), GeomDots$default_aes),

  default_key_aes = defaults(aes(
    colour = NA
  ), GeomDots$default_key_aes),

  ## params ------------------------------------------------------------------

  default_params = defaults(list(
    blur = "gaussian"
  ), GeomDots$default_params),

  param_docs = defaults(list(
    blur = glue_doc('Blur function to apply to dots.
      One of: \\itemize{
        \\item A function that takes a numeric vector of distances from the dot
          center, the dot radius, and the standard deviation of the blur and returns
          a vector of opacities in \\eqn{[0, 1]}, such as [blur_gaussian()]
          or [blur_interval()].
        \\item A string indicating what blur function to use, as the suffix to a
          function name starting with `blur_`; e.g. `"gaussian"` (the default)
          applies [blur_gaussian()].
      }')
  ), GeomDots$param_docs),

  setup_params = function(self, data, params) {
    params = ggproto_parent(GeomDots, self)$setup_params(data, params)

    params$blur = match_function(params$blur, prefix = "blur_")

    params
  },

  ## other methods -----------------------------------------------------------

  setup_data = function(self, data, params) {
    define_orientation_variables(params$orientation)

    data = ggproto_parent(GeomDots, self)$setup_data(data, params)

    # add an xmin/xmax to dots based on blur sd so that the full extent of
    # blurred dots is drawn
    data[["sd"]] = data[["sd"]] %||% params$sd
    if (!is.null(data[["sd"]])) {
      slab_i = which(data$datatype == "slab")
      data[slab_i, xmin] = data[slab_i, x] - 2 * data[slab_i, "sd"]
      data[slab_i, xmax] = data[slab_i, x] + 2 * data[slab_i, "sd"]
    }

    data
  },

  draw_slabs = function(self, s_data, panel_params, coord, orientation, ...) {
    define_orientation_variables(orientation)

    if (!is.null(s_data[["sd"]])) {
      # blur sd is expressed in terms of data coordinates, need to translate
      # into standardized space
      xscale = max(panel_params[[x.range]]) - min(panel_params[[x.range]])
      s_data$sd = s_data$sd / xscale
      s_data$sd[is.na(s_data$sd)] = 0
    }

    ggproto_parent(GeomDots, self)$draw_slabs(s_data, panel_params, coord, orientation, ...)
  },

  points_grob_factory = function(...) make_blurry_points_grob(...)
)

#' @title Blurry dot plot (geom)
#' @description
#' Variant of [geom_dots()] for creating blurry dotplots. Accepts an `sd`
#' aesthetic that gives the standard deviation of the blur applied to the dots.
#' Requires a graphics engine supporting radial gradients. Unlike [geom_dots()],
#' all dots must be circular, so this geom does not support the `shape` aesthetic.
#' @eval rd_dotsinterval_shortcut_geom(
#'   "blur_dots", "blurry dot", title = FALSE, describe = FALSE, examples = FALSE
#' )
#' @examplesIf getRversion() >= "4.1" && requireNamespace("posterior", quietly = TRUE)
#' library(dplyr)
#' library(ggplot2)
#'
#' theme_set(theme_ggdist())
#'
#' set.seed(1234)
#' x = rnorm(1000)
#'
#' # manually calculate quantiles and their MCSE
#' # this could also be done more succinctly with stat_mcse_dots()
#' p = ppoints(100)
#' df = data.frame(
#'   q = quantile(x, p),
#'   se = posterior::mcse_quantile(x, p)
#' )
#'
#' df %>%
#'   ggplot(aes(x = q, sd = se)) +
#'   geom_blur_dots()
#'
#' df %>%
#'   ggplot(aes(x = q, sd = se)) +
#'   # or blur = blur_interval(.width = .95) to set the interval width
#'   geom_blur_dots(blur = "interval")
#' @export
geom_blur_dots = make_geom(GeomBlurDots)


# blur functions ---------------------------------------------------------------

#' Blur functions for blurry dot plots
#' @description
#' Methods for constructing blurs, as used in the `blur` argument to
#' [geom_blur_dots()] or [stat_mcse_dots()].
#' @template description-auto-partial
#' @param x numeric vector of positive distances from the center of the dot
#' (assumed to be 0) to evaluate blur function at.
#' @param r radius of the dot that is being blurred.
#' @param sd standard deviation of the dot that is being blurred.
#' @param .width for `blur_interval()`, a probability giving the width of
#' the interval.
#' @name blur
#' @details
#' These functions are passed `x`, `r`, and `sd` when [geom_blur_dots()]
#' draws in order to create a radial gradient representing each dot in the
#' dotplot. They return values between `0` and `1` giving the opacity of the
#' dot at each value of `x`.
#'
#' `blur_gaussian()` creates a dot with radius `r` that has a Gaussian blur with
#' standard deviation `sd` applied to it. It does this by calculating
#' \eqn{\alpha(x; r, \sigma)}, the opacity at distance \eqn{x} from the center
#' of a dot with radius \eqn{r} that has had a Gaussian blur with standard
#' deviation \eqn{\sigma} = `sd` applied to it:
#'
#' \deqn{
#' \alpha(x; r, \sigma) = \Phi \left(\frac{x + r}{\sigma} \right) -
#'   \Phi \left(\frac{x - r}{\sigma} \right)
#' }
#'
#' `blur_interval()` creates an interval-type representation around the
#' dot at 50% opacity, where the interval is a Gaussian quantile interval with
#' mass equal to `.width` and standard deviation `sd`.
#' @returns
#' A vector of length `x` giving the opacity of the radial gradient representing
#' the dot at each `x` value.
#' @seealso
#' [geom_blur_dots()] and [stat_mcse_dots()] for geometries making use of
#' `blur`s.
#' @examples
#' # see examples in geom_blur_dots()
#' @importFrom stats pnorm
#' @export
blur_gaussian = auto_partial(name = "blur_gaussian", function(x, r, sd) {
  pnorm(x + r, 0, sd) - pnorm(x - r, 0, sd)
})

#' @rdname blur
#' @importFrom stats qnorm
#' @export
blur_interval = auto_partial(name = "blur_interval", function(x, r, sd, .width = 0.95) {
  z = qnorm((1 + .width)/2)
  ifelse(
    x < r, 1,
    ifelse(
      x < z * sd, 0.5,
      0
    )
  )
})
