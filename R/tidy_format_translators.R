# to_broom_names / from_broom_names
#
# Author: mjskay
###############################################################################


# to_broom_names / from_broom_names -----------------------------

#' Translate between different tidy data frame formats for draws from distributions
#'
#' These functions translate tidybayes-style tidy data frames of draws to/from different tidy data frame
#' formats (each format using a different naming scheme).
#'
#' Function prefixed with \code{to_} translate from the tidybayes format to another format, functions
#' prefixed with \code{from_} translate from that format back to the tidybayes format. Formats include:
#'
#' \code{to_broom_names()} / \code{from_broom_names()}:
#'
#' \itemize{
#'    \item \code{.variable} <-> \code{term}
#'    \item \code{.value} <-> \code{estimate}
#'    \item \code{.prediction} <-> \code{.fitted}
#'    \item \code{.lower} <-> \code{conf.low}
#'    \item \code{.upper} <-> \code{conf.high}
#' }
#'
#' \code{to_ggmcmc_names()} / \code{from_ggmcmc_names()}:
#'
#' \itemize{
#'    \item \code{.chain} <-> \code{Chain}
#'    \item \code{.iteration} <-> \code{Iteration}
#'    \item \code{.variable} <-> \code{Parameter}
#'    \item \code{.value} <-> \code{value}
#' }
#'
#' @param data A data frame to translate.
#'
#' @return A data frame with (possibly) new names in some columns, according to the
#' translation scheme above.
#'
#' @author Matthew Kay
#' @keywords manip
#' @examples
#'
#' library(magrittr)
#'
#' data(line, package = "coda")
#'
#' line %>%
#'   gather_draws(alpha, beta, sigma) %>%
#'   median_qi() %>%
#'   to_broom_names()
#'
#' @name tidy-format-translators
#' @importFrom dplyr select_all
#' @importFrom rlang %||%
#' @export
to_broom_names = function(data) {
  lookup = c(
    .variable = "term",
    .value = "estimate",
    .prediction = ".fitted",
    .lower = "conf.low",
    .upper = "conf.high"
  )

  # this approach, while possibly a little odd seeming, ensures that
  # group names in grouped data frams are also changed
  select_all(data, ~ lookup[.] %||% .)
}


#' @rdname tidy-format-translators
#' @export
from_broom_names = function(data) {
  lookup = c(
    term = ".variable",
    estimate = ".value",
    .fitted = ".prediction",
    conf.low = ".lower",
    conf.high = ".upper"
  )

  select_all(data, ~ lookup[.] %||% .)
}



# to_ggmcmc_names / from_ggmcmc_names -------------------------------------

#' @rdname tidy-format-translators
#' @export
to_ggmcmc_names = function(data) {
  lookup = c(
    .chain = "Chain",
    .iteration = "Iteration",
    .variable = "Parameter",
    .value = "value"
  )

  select_all(data, ~ lookup[.] %||% .)
}

#' @rdname tidy-format-translators
#' @export
from_ggmcmc_names = function(data) {
  lookup = c(
    Chain = ".chain",
    Iteration = ".iteration",
    Parameter = ".variable",
    value = ".value"
  )

  select_all(data, ~ lookup[.] %||% .)
}

