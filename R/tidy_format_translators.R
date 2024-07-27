# to_broom_names and from_broom_names
#
# Author: mjskay
###############################################################################


# to_broom_names / from_broom_names -----------------------------

#' Translate between different tidy data frame formats for draws from distributions
#'
#' These functions translate \pkg{ggdist}/\pkg{tidybayes}-style data frames to/from different data frame
#' formats (each format using a different naming scheme for its columns).
#'
#' Function prefixed with `to_` translate from the \pkg{ggdist}/\pkg{tidybayes} format to another format, functions
#' prefixed with `from_` translate from that format back to the \pkg{ggdist}/\pkg{tidybayes} format. Formats include:
#'
#' [to_broom_names()] / [from_broom_names()]:
#'
#' \itemize{
#'    \item `.variable` <-> `term`
#'    \item `.value` <-> `estimate`
#'    \item `.prediction` <-> `.fitted`
#'    \item `.lower` <-> `conf.low`
#'    \item `.upper` <-> `conf.high`
#' }
#'
#' [to_ggmcmc_names()] / [from_ggmcmc_names()]:
#'
#' \itemize{
#'    \item `.chain` <-> `Chain`
#'    \item `.iteration` <-> `Iteration`
#'    \item `.variable` <-> `Parameter`
#'    \item `.value` <-> `value`
#' }
#'
#' @param data <[data.frame]> A data frame to translate.
#'
#' @return A data frame with (possibly) new names in some columns, according to the
#' translation scheme described in **Details**.
#'
#' @author Matthew Kay
#' @keywords manip
#' @examples
#'
#' library(dplyr)
#'
#' data(RankCorr_u_tau, package = "ggdist")
#'
#' df = RankCorr_u_tau %>%
#'   dplyr::rename(.variable = i, .value = u_tau) %>%
#'   group_by(.variable) %>%
#'   median_qi(.value)
#'
#' df
#'
#' df %>%
#'   to_broom_names()
#'
#' @name tidy-format-translators
#' @export
to_broom_names = function(data) {
  new_names = c(
    .variable = "term",
    .value = "estimate",
    .prediction = ".fitted",
    .lower = "conf.low",
    .upper = "conf.high"
  )

  rename_cols(data, new_names)
}


#' @rdname tidy-format-translators
#' @export
from_broom_names = function(data) {
  new_names = c(
    term = ".variable",
    estimate = ".value",
    .fitted = ".prediction",
    conf.low = ".lower",
    conf.high = ".upper"
  )

  rename_cols(data, new_names)
}



# to_ggmcmc_names / from_ggmcmc_names -------------------------------------

#' @rdname tidy-format-translators
#' @export
to_ggmcmc_names = function(data) {
  new_names = c(
    .chain = "Chain",
    .iteration = "Iteration",
    .variable = "Parameter",
    .value = "value"
  )

  rename_cols(data, new_names)
}

#' @rdname tidy-format-translators
#' @export
from_ggmcmc_names = function(data) {
  new_names = c(
    Chain = ".chain",
    Iteration = ".iteration",
    Parameter = ".variable",
    value = ".value"
  )

  rename_cols(data, new_names)
}
