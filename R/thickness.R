# thickness data type
#
# Author: mjskay
###############################################################################


# constructors ---------------------------------------------------------------

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
