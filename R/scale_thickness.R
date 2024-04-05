# thickness scale
#
# Author: mjskay
###############################################################################


# shared scale ------------------------------------------------------------

#' Slab thickness scale (ggplot2 scale)
#'
#' This \pkg{ggplot2} scale linearly scales all `thickness` values of geoms
#' that support the `thickness` aesthetic (such as [geom_slabinterval()]). It
#' can be used to align the `thickness` scales across multiple geoms (by default,
#' `thickness` is normalized on a per-geom level instead of as a global scale).
#' For a comprehensive discussion and examples of slab scaling and normalization,
#' see the [`thickness` scale article](https://mjskay.github.io/ggdist/articles/thickness.html).
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritDotParams ggplot2::continuous_scale
#' @param renormalize When mapping values to the `thickness` scale, should those
#' values be allowed to be renormalized by geoms (e.g. via the `normalize` parameter
#' to [geom_slabinterval()])? The default is `FALSE`: if `scale_thickness_shared()`
#' is in use, the geom-specific `normalize` parameter is ignored (this is achieved
#' by flagging values as already normalized by wrapping them in `thickness()`).
#' Set this to `TRUE` to allow geoms to also apply their own normalization.
#' Note that if you set renormalize to `TRUE`, subguides created via the
#' `subguide` parameter to [geom_slabinterval()] will display the scaled values
#' output by this scale, not the original data values.
#' @param expand A numeric vector of limit expansion constants of length
#' 2 or 4, following the same format used by the `expand` argument of
#' [continuous_scale()]. The default is not to expand the limits.
#' You can use the convenience function [expansion()] to generate the
#' expansion values; expanding the lower limit is usually not recommended
#' (because with most `thickness` scales the lower limit is the baseline
#' and represents `0`), so a typical usage might be something like
#' `expand = expansion(c(0, 0.05))` to expand the top end of the scale
#' by 5%.
#' @details
#' By default, normalization/scaling of slab thicknesses is controlled by geometries,
#' not by a \pkg{ggplot2} scale function. This allows various functionality not
#' otherwise possible, such as (1) allowing different geometries to have different
#' thickness scales and (2) allowing the user to control at what level of aggregation
#' (panels, groups, the entire plot, etc) thickness scaling is done via the `normalize`
#' parameter to [geom_slabinterval()].
#'
#' However, this default approach has one drawback: two different geoms will always
#' have their own scaling of `thickness`. [scale_thickness_shared()] offers an
#' alternative approach: when added to a chart, all geoms will use the same
#' `thickness` scale, and geom-level normalization (via their `normalize` parameters)
#' is ignored. This is achieved by "marking" thickness values as already
#' normalized by wrapping them in the [thickness()] data type (this can be
#' disabled by setting `renormalize = TRUE`).
#'
#' Note: while a slightly more typical name for `scale_thickness_shared()` might
#' be `scale_thickness_continuous()`, the latter name would cause this scale
#' to be applied to all `thickness` aesthetics by default according to the rules
#' \pkg{ggplot2} uses to find default scales. Thus, to retain the usual behavior
#' of [stat_slabinterval()] (per-geom normalization of `thickness`), this scale
#' is called `scale_thickness_shared()`.
#'
#' @return
#' A [ggplot2::Scale] representing a scale for the `thickness`
#' aesthetic for `ggdist` geoms. Can be added to a [ggplot()] object.
#' @name scale_thickness
#' @author Matthew Kay
#' @family ggdist scales
#' @seealso The [thickness] datatype.
#' @seealso The `thickness` aesthetic of [geom_slabinterval()].
#' @seealso [subscale_thickness()], for setting a `thickness` sub-scale within
#' a single [geom_slabinterval()].
#' @examples
#' library(distributional)
#' library(ggplot2)
#' library(dplyr)
#'
#' prior_post = data.frame(
#'   prior = dist_normal(0, 1),
#'   posterior = dist_normal(0.1, 0.5)
#' )
#'
#' # By default, separate geoms have their own thickness scales, which means
#' # distributions plotted using two separate geoms will not have their slab
#' # functions drawn on the same scale (thus here, the two distributions have
#' # different areas under their density curves):
#' prior_post %>%
#'   ggplot() +
#'   stat_halfeye(aes(xdist = posterior)) +
#'   stat_slab(aes(xdist = prior), fill = NA, color = "red")
#'
#' # For this kind of prior/posterior chart, it makes more sense to have the
#' # densities on the same scale; thus, the areas under both would be the same.
#' # We can do that using scale_thickness_shared():
#' prior_post %>%
#'   ggplot() +
#'   stat_halfeye(aes(xdist = posterior)) +
#'   stat_slab(aes(xdist = prior), fill = NA, color = "#e41a1c") +
#'   scale_thickness_shared()
#'
#' @importFrom scales identity_pal
#' @import vctrs
#' @export
scale_thickness_shared = function(
  name = waiver(), breaks = waiver(), labels = waiver(),
  limits = function(l) c(min(0, l[[1]]), l[[2]]),
  renormalize = FALSE,
  oob = scales::oob_keep,
  guide = "none",
  expand = c(0, 0),
  ...
) {
  sc = continuous_scale(
    "thickness", palette = identity_pal(),
    name = name, breaks = breaks, labels = labels,
    limits = limits,
    oob = oob,
    guide = guide,
    expand = expand,
    ...,
    super = ScaleThicknessShared
  )
  sc$renormalize = renormalize
  sc
}

ScaleThicknessShared = ggproto("ScaleThicknessShared", ScaleContinuous,
  renormalize = FALSE,

  map = function(self, x, limits = self$dimension(expand = self$expand)) {
    out = if (is_thickness(x)) {
      x
    } else {
      ggproto_parent(ScaleContinuous, self)$map(x, limits)
    }

    if (self$renormalize) {
      out
    } else {
      thickness(out, limits[[1]], limits[[2]])
    }
  },

  train = function(self, x) {
    if (!is_thickness(x)) {
      ggproto_parent(ScaleContinuous, self)$train(x)
    }
  }
)


# identity scale ----------------------------------------------------------

#' @rdname scale_thickness
#' @importFrom scales identity_pal
#' @export
scale_thickness_identity = function(..., guide = "none") {
  continuous_scale(
    "thickness", palette = identity_pal(),
    ...,
    guide = guide, super = ScaleContinuousIdentity
  )
}


# scale_type --------------------------------------------------------------

#' @export
scale_type.ggdist_thickness = function(x) {
  "continuous"
}
