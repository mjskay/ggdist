# probability expressions within after_stat() for stat_slabinterval
#
# Author: mjskay
###############################################################################

#' Probability expressions in ggdist aesthetics
#'
#' **Experimental** probability-like expressions that can be used in place of
#' some `after_stat()` expressions in aesthetic assignments in \pkg{ggdist} stats.
#'
#' @param x <bare [language]> Expressions. See *Probability expressions*, below.
#'
#' @details
#' [Pr_()] and [p_()] are an **experimental** mini-language for specifying aesthetic values
#' based on probabilities and probability densities derived from distributions
#' supplied to \pkg{ggdist} stats (e.g., in [stat_slabinterval()],
#' [stat_dotsinterval()], etc.). They generate expressions that use [`after_stat()`][ggplot2::after_stat]
#' and the computed variables of the stat (such as `cdf` and `pdf`; see e.g.
#' the **Computed Variables** section of [stat_slabinterval()]) to compute
#' the desired probabilities or densities.
#'
#' For example, one way to map the density of a distribution onto the `alpha`
#' aesthetic of a slab is to use `after_stat(pdf)`:
#'
#' ```
#' ggplot() +
#'   stat_slab(aes(xdist = distributional::dist_normal(), alpha = after_stat(pdf)))
#' ```
#'
#' \pkg{ggdist} probability expressions offer an alternative, equivalent syntax:
#'
#' ```
#' ggplot() +
#'   stat_slab(aes(xdist = distributional::dist_normal(), alpha = !!p_(x)))
#' ```
#'
#' Where `p_(x)` is the probability density function. The use of `!!` is
#' necessary to splice the generated expression into the `aes()` call; for
#' more information, see [quasiquotation][rlang::quasiquotation].
#'
#' @section Probability expressions:
#'
#' Probability expressions consist of a call to `Pr_()` or `p_()` containing
#' a small number of valid combinations of operators and variable names.
#'
#' Valid variables in probability expressions include:
#'
#' - `x`, `y`, or `value`: values along the `x` or `y` axis. `value` is the
#'   orientation-neutral form.
#' - `xdist`, `ydist`, or `dist`: distributions mapped along the `x` or `y`
#'   axis. `dist` is the orientation-neutral form. `X` and `Y` can also be
#'   used as synonyms for `xdist` and `ydist`.
#' - `interval`: the smallest interval containing the current `x`/`y` value.
#'
#' `Pr_()` generates expressions for probabilities, e.g. cumulative distribution
#' functions (CDFs). Valid operators inside `Pr_()` are:
#'
#' - `<`, `<=`, `>`, `>=`: generates values of the cumulative distribution
#'   function (CDF) or complementary CDF by comparing one of \{`x`, `y`, `value`\}
#'   to one of \{`xdist`, `ydist`, `dist`, `X`, `Y`\}. For example, `Pr_(xdist <= x)`
#'   gives the CDF and `Pr_(xdist > x)` gives the CCDF.
#' - `%in%`: currently can only be used with `interval` on the right-hand side:
#'   gives the probability of \{`x`, `y`, `value`\} (left-hand side) being in the
#'   smallest interval the stat generated that contains the value; e.g.
#'   `Pr_(x %in% interval)`.
#'
#' `p_()` generates expressions for probability density functions or probability mass
#'   functions (depending on if the underlying distribution is continuous or
#'   discrete). It currently does not allow any operators in the expression, and
#'   must be passed one of `x`, `y`, or `value`.
#' @seealso The *Computed Variables* section of [stat_slabinterval()] (especially
#' `cdf` and `pdf`) and the [`after_stat()`][ggplot2::after_stat] function.
#' @examples
#' library(ggplot2)
#' library(distributional)
#'
#' df = data.frame(
#'   d = c(dist_normal(2.7, 1), dist_lognormal(1, 1/3)),
#'   name = c("normal", "lognormal")
#' )
#'
#' # map density onto alpha of the fill
#' ggplot(df, aes(y = name, xdist = d)) +
#'   stat_slabinterval(aes(alpha = !!p_(x)))
#'
#' # map CCDF onto thickness (like stat_ccdfinterval())
#' ggplot(df, aes(y = name, xdist = d)) +
#'   stat_slabinterval(aes(thickness = !!Pr_(xdist > x)))
#'
#' # map containing interval onto fill
#' ggplot(df, aes(y = name, xdist = d)) +
#'   stat_slabinterval(aes(fill = !!Pr_(x %in% interval)))
#'
#' # the color scale in the previous example is not great, so turn the
#' # probability into an ordered factor and adjust the fill scale.
#' # Though, see also the `level` computed variable in `stat_slabinterval()`,
#' # which is probably easier to use to create this style of chart.
#' ggplot(df, aes(y = name, xdist = d)) +
#'   stat_slabinterval(aes(fill = ordered(!!Pr_(x %in% interval)))) +
#'   scale_fill_brewer(direction = -1)
#'
#' @export
#' @importFrom rlang as_label as_name
Pr_ = function(x) {
  expr = substitute(x)
  label = paste0("Pr_(", as_label(expr), ")")

  if (is.call(expr)) {
    f = as_name(expr[[1]])
    e1 = parse_Pr_element(expr[[2]], label)
    e2 = parse_Pr_element(expr[[3]], label)
    switch(f,
      ">" = , ">=" = , "<" = , "<=" = {
        check_Pr_cdf_element_combination(e1, e2, label)
        e1 = standardize_Pr_element(e1)
        e2 = standardize_Pr_element(e2)
        if (f %in% c(">", ">=")) e1 = e2
        if (e1 == "dist") {
          return(quote(after_stat(cdf)))
        } else {
          return(quote(after_stat(1 - cdf)))
        }
      },
      "%in%" = {
        e1 = standardize_Pr_element(e1)
        e2 = standardize_Pr_element(e2)
        if (e1 %in% c("value", "dist") && e2 == "interval") {
          return(quote(after_stat(.width)))
        }
      }
    )
  }

  stop0("Unrecognized probability expression: `", label, "`")
}

#' @rdname Pr_
#' @export
p_ = function(x) {
  expr = substitute(x)
  label = paste0("p_(", as_label(expr), ")")

  if (is.name(expr)) {
    e = standardize_Pr_element(parse_Pr_element(expr, label))
    if (e == "value") {
      return(quote(after_stat(pdf)))
    }
  }

  stop0("Unrecognized probability expression: `", label, "`")
}


# helpers -----------------------------------------------------------------

parse_Pr_element = function(e, label = NULL) {
  e = deparse0(e)
  switch(e,
    X = , xdist = "xdist",
    Y = , ydist = "ydist",
    x = "x",
    y = "y",
    value = "value",
    dist = "dist",
    interval = "interval",
    stop0("Unrecognized probability expression element in `", label, "`: ", e)
  )
}

standardize_Pr_element = function(e) {
  switch(e,
    x = , y = , value = "value",
    xdist = , ydist = , dist = "dist",
    e
  )
}

check_Pr_cdf_element_combination = function(e1, e2, label = NULL) {
  if (
    !list(sort(c(e1, e2))) %in% list(
      c("x", "xdist"),
      c("y", "ydist"),
      c("dist", "value")
    )
  ) {
    stop0("Invalid combination of probability expression elements in `", label, "`: ", e1, " and ", e2)
  }
}
