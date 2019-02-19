# add_draws
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(".draw")


#' Add draws to a data frame in tidy format
#'
#' Add draws from a matrix of draws (usually draws from a predictive distribution) to a data frame in tidy format. This is
#' a generic version of \code{\link{add_fitted_draws}}/\code{\link{add_predicted_draws}} that can be used with model types
#' that have their own prediction functions that are not yet supported by tidybayes.
#'
#' Given a data frame with M rows and an N by M matrix of N draws, adds a \code{.row}, \code{.draw}, and \code{.value}
#' column (or another name if \code{value} is set) to \code{data}, and expands \code{data} into a long-format dataframe of draws.
#'
#' \code{\link{add_fitted_draws}(df, m)} is roughly equivalent to \code{\link{add_draws}(df, posterior_linpred(m))}, except
#' that \code{add_fitted_draws} standardizes argument names and values across packages.
#'
#' \code{\link{add_predicted_draws}(df, m)} is roughly equivalent to \code{\link{add_draws}(df, posterior_predict(m))}, except
#' that \code{add_predicted_draws} standardizes argument names and values across packages.
#'
#' @param data Data frame to add draws to, with M rows.
#' @param draws N by M matrix of draws, with M columns corresponding to the M rows in \code{data}, and N draws in each column.
#' @param value The name of the output column; default \code{".value"}.
#' @return A data frame (actually, a \code{\link[tibble]{tibble}}) with a \code{.row} column (a
#' factor grouping rows from the input \code{data}), a \code{.draw} column (a unique index corresponding to each draw
#' from the distribution), and a column with its name specified by the \code{value} argument (default is \code{.value})
#' containing the values of draws from \code{draws}. The data frame is grouped by all rows in \code{data} plus the \code{.row} column.
#' @author Matthew Kay
#' @seealso \code{\link{add_fitted_draws}}, \code{\link{add_predicted_draws}}
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' if (
#'   require("rstanarm", quietly = TRUE) &&
#'   require("modelr", quietly = TRUE)
#' ) {
#'
#'   theme_set(theme_light())
#'
#'   m_mpg = stan_glm(mpg ~ hp * cyl, data = mtcars,
#'     # 1 chain / few iterations just so example runs quickly
#'     # do not use in practice
#'     chains = 1, iter = 500)
#'
#'   # plot posterior predictive intervals
#'   mtcars %>%
#'     group_by(cyl) %>%
#'     data_grid(hp = seq_range(hp, n = 101)) %>%
#'     # the line below is equivalent to add_fitted_draws(m_mpg), except that it does not
#'     # standardize arguments across model types
#'     add_draws(posterior_linpred(m_mpg, newdata = .)) %>%
#'     ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
#'     stat_lineribbon(aes(y = .value), alpha = 0.25) +
#'     geom_point(data = mtcars) +
#'     scale_fill_brewer(palette = "Greys")
#' }
#' }
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest
#' @importFrom dplyr tibble bind_cols
#' @importFrom tibble is_tibble
#' @export
add_draws = function(data, draws, value = ".value") {
  .value = as.name(value)

  groups = union(colnames(data), ".row")

  add_draws_list(data, draws, value = value) %>%
    unnest(.draw, !!.value, .drop = FALSE) %>%
    group_by_at(groups)
}

add_draws_list = function(data, draws, value = ".value") {
  if (!is_tibble(data)) {
    data = as_tibble(data)
  }

  bind_cols(data, draws_list(draws, value = value))
}

draws_list = function(draws, value = ".value") {
  .value = as.name(value)

  if (length(dim(draws)) != 2) {
    stop("`draws` must have exactly two dimensions. It has", length(dim(draws)))
  }

  tibble(
    .row = seq_len(ncol(draws)),
    .draw = lapply(1:ncol(draws), function(i) seq_len(nrow(draws))),
    !!.value := lapply(1:ncol(draws), function(i) draws[,i])
  )
}
