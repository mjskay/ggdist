#' Sub-scale for thickness aesthetic
#'
#' This is a sub-scale intended for adjusting the scaling of the `thickness`
#' aesthetic at a geometry (or sub-geometry) level in \pkg{ggdist}. It can be
#' used with the `subscale` parameter of [geom_slabinterval()].
#' @template description-auto-partial
#'
#' @param x a [numeric] or [thickness] vector to be rescaled.
#'    Typically provided automatically by [geom_slabinterval()].
#' @param expand A numeric vector of limit expansion constants of length
#'    2 or 4, following the same format used by the `expand` argument of
#'    [continuous_scale()]. The default is not to expand the limits.
#'    You can use the convenience function [expansion()] to generate the
#'    expansion values; expanding the lower limit is usually not recommended
#'    (because with most `thickness` scales the lower limit is the baseline
#'    and represents `0`), so a typical usage might be something like
#'    `expand = expansion(c(0, 0.05))` to expand the top end of the subscale
#'    by 5%.
#' @returns A [thickness] vector of the same length as `x` scaled to be between
#' `0` and `1`.
#' @family sub-scales
#' @seealso The [thickness] datatype.
#' @seealso The `thickness` aesthetic of [geom_slabinterval()].
#' @seealso [scale_thickness_shared()], for setting a `thickness` scale across
#' all geometries using the `thickness` aesthetic.
#' @examples
#' # TODO
#' @importFrom scales rescale oob_squish_infinite expand_range
#' @export
subscale_thickness = auto_partial(name = "subscale_thickness", function(
  x,
  limits = function(l) c(min(0, l[1]), l[2]),
  expand = c(0, 0)
) {
  expand = validate_expand(expand)
  limits = validate_limits(limits)

  finite_x = x[is.finite(x)]
  if (length(finite_x) > 0) {
    limits = limits(range(finite_x))
    stopifnot(is.finite(limits))
    limits[1] = expand_range(limits, expand[1], expand[2])[1]
    limits[2] = expand_range(limits, expand[3], expand[4])[2]
    if (limits[1] == limits[2]) {
      limits = c(NA_real_, NA_real_)
    } else {
      x = rescale(x, from = limits)
    }
  } else {
    limits = c(NA_real_, NA_real_)
  }

  # infinite values get plotted at the max height (e.g. for point masses)
  x = oob_squish_infinite(x)

  thickness(x, limits[1], limits[2])
})

#' Identity sub-scale for thickness aesthetic
#'
#' This is an identity sub-scale for the `thickness` aesthetic
#' in \pkg{ggdist}. It returns its input as a [thickness] vector without
#' rescaling (though it does squish infinite values). It can be used with the
#' `subscale` parameter of [geom_slabinterval()].
#'
#' @inheritParams subscale_thickness
#' @returns A [thickness] vector of the same length as `x`, with infinite
#' values in `x` squished into the data range.
#' @family sub-scales
#' @export
subscale_identity = function(x) {
  x = as_thickness(x)
  limits = range(0, 1, field(x, "x"), na.rm = TRUE, finite = TRUE)
  field(x, "x") = oob_squish_infinite(field(x, "x"), range = limits)
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
