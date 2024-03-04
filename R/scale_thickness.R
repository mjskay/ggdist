# thickness scale and data type
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
#' [thickness()] is used by [scale_thickness_shared()] to create `numeric()`-like
#' objects marked as being in units of slab "thickness". Unlike regular `numeric()`s,
#' `thickness()` values mapped onto the `thickness` aesthetic are not rescaled by
#' [scale_thickness_shared()] or [geom_slabinterval()]. In most cases `thickness()`
#' is not useful directly; though it can be used to mark values that should not be
#' rescaled---see the definitions of [stat_ccdfinterval()] and [stat_gradientinterval()]
#' for some usages.
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
#' @seealso The `thickness` aesthetic of [geom_slabinterval()].
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
  ...
) {
  sc = continuous_scale(
    "thickness", palette = identity_pal(),
    name = name, breaks = breaks, labels = labels,
    limits = limits,
    oob = oob,
    guide = guide,
    expand = expansion(0, 0),
    ...,
    super = ScaleThicknessShared
  )
  sc$renormalize = renormalize
  sc
}

ScaleThicknessShared = ggproto("ScaleThicknessShared", ScaleContinuous,
  renormalize = FALSE,

  map = function(self, x, limits = self$get_limits()) {
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


# data type for thickness ---------------------------------------------------------------

new_thickness = function(x = double(), lower = NA_real_, upper = NA_real_) {
  if (length(x) < 1) x = double()
  stopifnot(is.double(x))
  if (length(lower) <= 1) lower = rep(lower, length(x))
  if (length(upper) <= 1) upper = rep(upper, length(x))
  new_rcrd(list(x = x, lower = lower, upper = upper), class = "ggdist_thickness")
}

#' @rdname scale_thickness
#' @param x An object (typically a `numeric()`) to be converted to a `thickness()`
#' object.
#' @param lower The original lower bounds of thickness values before scaling.
#' @param upper The original upper bounds of thickness values before scaling.
#' @export
thickness = function(x = double(), lower = NA_real_, upper = NA_real_) {
  x = vec_cast(x, double())
  lower = vec_cast(lower, double())
  upper = vec_cast(upper, double())
  new_thickness(x, lower, upper)
}


# bounds ------------------------------------------------------------------

thickness_lower = function(x) {
  if (is_thickness(x)) field(x, "lower") else NA
}

thickness_upper = function(x) {
  if (is_thickness(x)) field(x, "upper") else NA
}


# predicates --------------------------------------------------------------

is_thickness = function(x) {
  inherits(x, "ggdist_thickness")
}

#' @export
is.na.ggdist_thickness = function(x) {
  is.na(field(x, "x"))
}


# formatting ------------------------------------------------------

#' @export
vec_ptype_full.ggdist_thickness = function(x, ...) "thickness"
#' @export
vec_ptype_abbr.ggdist_thickness = function(x, ...) "thk"

#' @export
format.ggdist_thickness = function(x, ...) {
  sprintf("%sthk [%s,%s]", field(x, "x"), field(x, "lower"), field(x, "upper"))
}


# casting -------------------------------------------------------

as_thickness = function(x) {
  vec_cast(x, new_thickness())
}

#' @export
vec_ptype2.ggdist_thickness.ggdist_thickness = function(x, y, ...) new_thickness()

#' @export
vec_ptype2.ggdist_thickness.double = function(x, y, ...) new_thickness()
#' @export
vec_ptype2.double.ggdist_thickness = function(x, y, ...) new_thickness()
#' @export
vec_ptype2.ggdist_thickness.integer = function(x, y, ...) new_thickness()
#' @export
vec_ptype2.integer.ggdist_thickness = function(x, y, ...) new_thickness()

#' @export
vec_cast.ggdist_thickness.double = function(x, to, ...) thickness(x)
#' @export
vec_cast.ggdist_thickness.integer = function(x, to, ...) thickness(x)
#' @export
vec_cast.double.ggdist_thickness = function(x, to, ...) field(x, "x")
#' @export
vec_cast.integer.ggdist_thickness = function(x, to, ...) as.integer(field(x, "x"))


# arithmetic --------------------------------------------------------------

#' @export
#' @method vec_arith ggdist_thickness
vec_arith.ggdist_thickness = function(op, x, y, ...) {
  UseMethod("vec_arith.ggdist_thickness", y)
}
#' @export
#' @method vec_arith.ggdist_thickness default
vec_arith.ggdist_thickness.default = function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.ggdist_thickness ggdist_thickness
vec_arith.ggdist_thickness.ggdist_thickness = function(op, x, y, ...) {
  x_value = field(x, "x")
  y_value = field(y, "x")

  bounds = lapply(c(lower = "lower", upper = "upper"), function(bound) {
    x_bound = field(x, bound)
    y_bound = field(y, bound)
    use_x = is.na(y_bound) | ((x_bound == y_bound) %in% TRUE)
    use_y = is.na(x_bound)

    incompatible = !(use_x | use_y)
    if (any(incompatible)) {
      cli_abort(
        "incompatible <thickness> bounds at locations {which(incompatible)}",
        class = "ggdist_incompatible_thickness_bounds"
      )
    }

    x_bound[use_y] = y_bound[use_y]
    x_bound
  })

  switch(
    op,
    "+" = ,
    "-" = new_thickness(vec_arith_base(op, x_value, y_value), bounds$lower, bounds$upper),
    "/" = vec_arith_base(op, x_value, y_value),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.ggdist_thickness numeric
vec_arith.ggdist_thickness.numeric = function(op, x, y, ...) {
  x_value = field(x, "x")
  switch(
    op,
    "/" = ,
    "*" = new_thickness(vec_arith_base(op, x_value, y), field(x, "lower"), field(x, "upper")),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric ggdist_thickness
vec_arith.numeric.ggdist_thickness = function(op, x, y, ...) {
  y_value = field(y, "x")
  switch(
    op,
    "*" = new_thickness(vec_arith_base(op, x, y_value), field(y, "lower"), field(y, "upper")),
    stop_incompatible_op(op, x, y)
  )
}
