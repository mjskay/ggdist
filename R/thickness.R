# thickness data type
#
# Author: mjskay
###############################################################################


# constructors ---------------------------------------------------------------

#' Thickness (datatype)
#'
#' A representation of the thickness of a slab: a scaled value (`x`) where
#' `0` is the base of the slab and `1` is its maximum extent, and the lower
#' (`lower`) and upper (`upper`) limits of the slab values in their original
#' data units.
#'
#' @param x <coercible-to-[numeric]> A [numeric] vector or an object
#' coercible to a [numeric] (via [vctrs::vec_cast()]) representing scaled values
#' to be converted to a `thickness()` object.
#' @param lower <[numeric]> The original lower bounds of thickness values before scaling.
#' May be `NA` to indicate that this bound is not known.
#' @param upper <[numeric]> The original upper bounds of thickness values before scaling.
#' May be `NA` to indicate that this bound is not known.
#'
#' @details
#' This datatype is used by [scale_thickness_shared()] and [subscale_thickness()]
#' to represent `numeric()`-like objects marked as being in units of slab "thickness".
#'
#' Unlike regular `numeric()`s, `thickness()` values mapped onto the `thickness`
#' aesthetic are not rescaled by [scale_thickness_shared()] or [geom_slabinterval()].
#' In most cases `thickness()` is not useful directly; though it can be used to
#' mark values that should not be rescaled---see the definitions of
#' [stat_ccdfinterval()] and [stat_gradientinterval()] for some example usages.
#'
#' [thickness] objects with unequal lower or upper limits may not be combined.
#' However, [thickness] objects with `NA` limits may be combined with
#' [thickness] objects with non-`NA` limits. This allows (e.g.) specifying
#' locations on the [thickness] scale that are independent of data limits.
#' @return
#' A [vctrs::rcrd] of class `"ggdist_thickness"` with fields
#' `"x"`, `"lower"`, and `"upper"`.
#' @author Matthew Kay
#' @seealso The `thickness` aesthetic of [geom_slabinterval()].
#' @seealso [scale_thickness_shared()], for setting a `thickness` scale across
#' all geometries using the `thickness` aesthetic.
#' @seealso [subscale_thickness()], for setting a `thickness` sub-scale within
#' a single [geom_slabinterval()].
#' @examples
#' thickness(0:1)
#' thickness(0:1, 0, 10)
#' @name thickness
NULL

new_thickness = function(x = double(), lower = NA_real_, upper = NA_real_) {
  if (length(x) < 1) x = double()
  stopifnot(is.double(x))
  if (length(lower) <= 1) lower = rep(lower, length(x))
  if (length(upper) <= 1) upper = rep(upper, length(x))
  new_rcrd(list(x = x, lower = lower, upper = upper), class = "ggdist_thickness")
}

#' @rdname thickness
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
