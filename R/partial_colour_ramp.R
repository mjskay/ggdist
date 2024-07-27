# partial_colour_ramp datatype used by scale_colour_ramp
#
# Author: mjskay
###############################################################################


# partial_colour_ramp -------------------------------------------------------

#' Partial colour ramp (datatype)
#'
#' A representation of a partial ramp between two colours: the origin colour
#' (`from`) and the distance from the origin colour to the target colour
#' (`amount`, a value between `0` and `1`). The target colour of the ramp
#' can be filled in later using `ramp_colours()`, producing a colour.
#'
#' @param amount <[numeric]> Vector of values between `0` and `1` giving amounts
#' to ramp the colour. `0` corresponds to the colour `from`.
#' @param from <[character]> Vector giving colours to ramp from.
#'
#' @details
#' This datatype is used by [scale_colour_ramp] to create ramped colours in
#' \pkg{ggdist} geoms. It is a [vctrs::rcrd] datatype with two fields:
#' `"amount"`, the amount to ramp, and `"from"`, the colour to ramp from.
#'
#' Colour ramps can be applied (i.e. translated into colours) using
#' [ramp_colours()], which can be used with [partial_colour_ramp()]
#' to implement geoms that make use of `colour_ramp` or `fill_ramp` scales.
#' @return
#' A [vctrs::rcrd] of class `"ggdist_partial_colour_ramp"` with fields
#' `"amount"` and `"from"`.
#' @author Matthew Kay
#' @family colour ramp functions
#' @examples
#' pcr = partial_colour_ramp(c(0, 0.25, 0.75, 1), "red")
#' pcr
#'
#' ramp_colours("blue", pcr)
#' @name partial_colour_ramp
NULL


# partial_colour_ramp datatype --------------------------------------------

new_partial_colour_ramp = function(amount = double(), from = "white") {
  if (length(amount) < 1) x = double()
  stopifnot(is.double(amount))
  if (length(from) <= 1) from = rep(from, length(amount))
  stopifnot(is.character(from))
  vctrs::new_rcrd(list(amount = amount, from = from), class = "ggdist_partial_colour_ramp")
}

#' @rdname partial_colour_ramp
#' @export
partial_colour_ramp = function(amount = double(), from = "white") {
  amount = vctrs::vec_cast(amount, numeric())
  from = vctrs::vec_cast(from, character())
  new_partial_colour_ramp(amount, from)
}


# formatting --------------------------------------------------------------

#' @export
vec_ptype_full.ggdist_partial_colour_ramp = function(x, ...) "partial_colour_ramp"
#' @export
vec_ptype_abbr.ggdist_partial_colour_ramp = function(x, ...) "rmp"

#' @export
format.ggdist_partial_colour_ramp = function(x, ...) {
  sprintf("[%s from %s]", field(x, "amount"), field(x, "from"))
}


# predicates --------------------------------------------------------------

#' @export
is.na.ggdist_partial_colour_ramp = function(x) {
  is.na(field(x, "amount")) | is.na(field(x, "from"))
}


# casting -------------------------------------------------------

as_partial_colour_ramp = function(x) {
  vec_cast(x, new_partial_colour_ramp())
}

#' @export
vec_ptype2.ggdist_partial_colour_ramp.ggdist_partial_colour_ramp = function(x, y, ...) new_partial_colour_ramp()

#' @export
vec_ptype2.ggdist_partial_colour_ramp.double = function(x, y, ...) new_partial_colour_ramp()
#' @export
vec_ptype2.double.ggdist_partial_colour_ramp = function(x, y, ...) new_partial_colour_ramp()
#' @export
vec_ptype2.ggdist_partial_colour_ramp.integer = function(x, y, ...) new_partial_colour_ramp()
#' @export
vec_ptype2.integer.ggdist_partial_colour_ramp = function(x, y, ...) new_partial_colour_ramp()

#' @export
vec_cast.ggdist_partial_colour_ramp.double = function(x, to, ...) partial_colour_ramp(x)
#' @export
vec_cast.ggdist_partial_colour_ramp.integer = function(x, to, ...) partial_colour_ramp(x)
#' @export
vec_cast.double.ggdist_partial_colour_ramp = function(x, to, ...) field(x, "amount")
#' @export
vec_cast.integer.ggdist_partial_colour_ramp = function(x, to, ...) as.integer(field(x, "amount"))


# applying colour ramps --------------------------------------------------------

#' Apply partial colour ramps
#'
#' Given vectors of colours and [`partial_colour_ramp`]s, ramps the colours
#' according to the parameters of the partial colour ramps, returning
#' a vector of the same length as the inputs giving the transformed
#' (ramped) colours.
#'
#' @param colour <[character]> Vector of colours to ramp to.
#' @param ramp <[partial_colour_ramp]> Vector of colour ramps (same length as
#' `colour`) giving the colour to ramp from and the amount to ramp.
#' @details
#' Takes vectors of colours and [`partial_colour_ramp`]s and produces
#' colours by interpolating between each `from` colour and the target `colour`
#' the specified `amount` (where `amount` and `from` are the corresponding
#' fields of the `ramp`).
#'
#' For example, to add support for the `fill_ramp` aesthetic to a geometry,
#' this line could be used inside the `draw_group()` or `draw_panel()` method
#' of a geom:
#'
#' ```
#' data$fill = ramp_colours(data$fill, data$fill_ramp)
#' ```
#' @return
#' A character vector of colours.
#' @author Matthew Kay
#' @family colour ramp functions
#' @examples
#' pcr = partial_colour_ramp(c(0, 0.25, 0.75, 1), "red")
#' pcr
#'
#' ramp_colours("blue", pcr)
#' @export
ramp_colours = function(colour, ramp) {
  if (is.null(colour) || is.null(ramp)) return(colour)
  ramp = vec_cast(ramp, new_partial_colour_ramp())
  c(colour, ramp) %<-% vec_recycle_common(colour, ramp)

  map2_chr_(colour, ramp, function(colour_i, ramp_i) {
    scales::seq_gradient_pal(field(ramp_i, "from"), colour_i)(field(ramp_i, "amount"))
  })
}
