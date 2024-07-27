#' Sub-scale for thickness aesthetic
#'
#' This is a sub-scale intended for adjusting the scaling of the `thickness`
#' aesthetic at a geometry (or sub-geometry) level in \pkg{ggdist}. It can be
#' used with the `subscale` parameter of [geom_slabinterval()].
#' @template description-auto-partial-waivable
#'
#' @inheritParams scale_thickness
#' @param x <[numeric]> Vector to be rescaled.
#'    Typically provided automatically by [geom_slabinterval()].
#' @param limits <length-2 [numeric] | [function] | [NULL]> One of:
#'  - A [numeric] vector of length two providing the limits of the scale. Use
#'    `NA` to use the default minimum or maximum.
#'  - A function that accepts a length-2 [numeric] vector of the automatic
#'    limits and returns new limits. Unlike positional scales, these limits
#'    will not remove data.
#'  - `NULL` to use the range of the data
#'
#' @details
#' You can overwrite `subscale_thickness` in the global environment to set
#' the default properties of the thickness subscale. For example:
#'
#' ```r
#' subscale_thickness = ggdist::subscale_thickness(expand = expansion(c(0, 0.05)))
#' ```
#'
#' This will cause [geom_slabinterval()]s to default to a thickness subscale
#' that expands by 5% at the top of the scale. **Always** prefix such a
#' definition with `ggdist::` to avoid infinite loops caused by recursion.
#' @returns A [thickness] vector of the same length as `x` scaled to be between
#' `0` and `1`.
#' @family sub-scales
#' @seealso The [thickness] datatype.
#' @seealso The `thickness` aesthetic of [geom_slabinterval()].
#' @seealso [scale_thickness_shared()], for setting a `thickness` scale across
#' all geometries using the `thickness` aesthetic.
#' @examples
#' library(ggplot2)
#' library(distributional)
#'
#' df = data.frame(d = dist_normal(2:3, 1), g = c("a", "b"))
#'
#' # breaks on thickness subguides are always limited to the bounds of the
#' # subscale, which may leave labels off near the edge of the subscale
#' # (e.g. here `0.4` is omitted because the max value is approx `0.39`)
#' ggplot(df, aes(xdist = d, y = g)) +
#'   stat_slabinterval(
#'     subguide = "inside"
#'   )
#'
#' # We can use the subscale to expand the upper limit of the thickness scale
#' # by 5% (similar to the default for positional scales), allowing bounds near
#' # (but just less than) the limit, like `0.4`, to be shown.
#' ggplot(df, aes(xdist = d, y = g)) +
#'   stat_slabinterval(
#'     subguide = "inside",
#'     subscale = subscale_thickness(expand = expansion(c(0, 0.5)))
#'   )
#' @importFrom scales rescale expand_range
#' @export
subscale_thickness = auto_partial(name = "subscale_thickness", function(
  x,
  limits = function(l) c(min(0, l[1]), l[2]),
  expand = c(0, 0)
) {
  expand = validate_expand(expand)
  limits_fun = validate_limits(limits)

  finite_x = x[is.finite(x)]
  if (length(finite_x) > 0) {
    limits = limits_fun(range(finite_x))
    stopifnot(is.finite(limits))
    limits = c(
      expand_range(limits, expand[1], expand[2])[1],
      expand_range(limits, expand[3], expand[4])[2]
    )
    if (limits[1] == limits[2]) {
      limits = c(NA_real_, NA_real_)
    } else {
      x = rescale(x, from = limits)
    }
  } else {
    limits = c(NA_real_, NA_real_)
  }

  thickness(x, limits[1], limits[2])
})

#' Identity sub-scale for thickness aesthetic
#'
#' This is an identity sub-scale for the `thickness` aesthetic
#' in \pkg{ggdist}. It returns its input as a [thickness] vector without
#' rescaling. It can be used with the `subscale` parameter of
#' [geom_slabinterval()].
#'
#' @inheritParams subscale_thickness
#' @returns A [thickness] vector of the same length as `x`, with infinite
#' values in `x` squished into the data range.
#' @family sub-scales
#' @export
subscale_identity = function(x) {
  thickness(x)
}


# apply a thickness subscale ----------------------------------------------

#' Squish infinite values in a [thickness] vector
#' @importFrom scales oob_squish_infinite
#' @noRd
squish_infinite_thickness = function(x) {
  limits = range(
    0, 1, field(x, "x"), thickness_lower(x), thickness_upper(x),
    na.rm = TRUE, finite = TRUE
  )
  field(x, "x") = oob_squish_infinite(field(x, "x"), range = limits)
  x
}

#' apply a thickness subscale to an object
#' @noRd
apply_subscale = function(x, subscale) UseMethod("apply_subscale")

#' @export
apply_subscale.NULL = function(x, subscale) {
  NULL
}

#' @export
apply_subscale.default = function(x, subscale) {
  squish_infinite_thickness(subscale(x))
}

#' @export
apply_subscale.ggdist_thickness = function(x, subscale) {
  # thickness values passed directly into the geom (e.g. by
  # scale_thickness_shared()) are not scaled again.
  squish_infinite_thickness(x)
}

#' @export
apply_subscale.data.frame = function(x, subscale) {
  x$thickness = apply_subscale(x$thickness, subscale = subscale)
  x
}


# helpers -----------------------------------------------------------------

#' Validate an `expand` argument and return a canonical version
#' (length-4 numeric).
#' @noRd
validate_expand = function(expand) {
  stopifnot(is.numeric(expand), length(expand) %in% c(2, 4))
  rep(expand, length.out = 4)
}

#' Validate a `limits` argument and return a canonical version
#' (a function that takes limits and returns new ones)
#' @noRd
validate_limits = function(limits) {
  if (is.null(limits)) {
    identity
  } else if (is.function(limits)) {
    limits
  } else {
    stopifnot(is.numeric(limits), length(limits) == 2L)
    function(l) {
      limits[is.na(limits)] = l
      limits
    }
  }
}
