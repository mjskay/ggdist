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
#' a generic version of [add_fitted_draws()]/[add_predicted_draws()] that can be used with model types
#' that have their own prediction functions that are not yet supported by tidybayes.
#'
#' Given a data frame with M rows and an N by M matrix of N draws, adds a `.row`, `.draw`, and `.value`
#' column (or another name if `value` is set) to `data`, and expands `data` into a long-format dataframe of draws.
#'
#' `add_fitted_draws(df, m)` is roughly equivalent to `add_draws(df, posterior_linpred(m, newdata = df, summary = FALSE))`, except
#' that `add_fitted_draws` standardizes argument names and values across packages and has additional features for some
#' model types (like handling ordinal responses and distributional parameters in brms).
#'
#' `add_predicted_draws(df, m)` is roughly equivalent to `add_draws(df, posterior_predict(m, newdata = df, summary = FALSE))`, except
#' that `add_predicted_draws` standardizes argument names and values across packages.
#'
#' @param data Data frame to add draws to, with M rows.
#' @param draws N by M matrix of draws, with M columns corresponding to the M rows in `data`, and N draws in each column.
#' @param value The name of the output column; default `".value"`.
#' @return A data frame (actually, a [tibble][tibble::tibble]) with a `.row` column (a
#' factor grouping rows from the input `data`), a `.draw` column (a unique index corresponding to each draw
#' from the distribution), and a column with its name specified by the `value` argument (default is `.value`)
#' containing the values of draws from `draws`. The data frame is grouped by all rows in `data` plus the `.row` column.
#' @author Matthew Kay
#' @seealso [add_fitted_draws()], [add_predicted_draws()], [add_draws()]
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' if (
#'   require("brms", quietly = TRUE) &&
#'   require("modelr", quietly = TRUE)
#' ) {
#'
#'   theme_set(theme_light())
#'
#'   m_mpg = brm(mpg ~ hp * cyl, data = mtcars,
#'     # 1 chain / few iterations just so example runs quickly
#'     # do not use in practice
#'     chains = 1, iter = 500)
#'
#'   # plot posterior predictive intervals
#'   mtcars %>%
#'     group_by(cyl) %>%
#'     data_grid(hp = seq_range(hp, n = 101)) %>%
#'     # the line below is equivalent to add_fitted_draws(m_mpg), except that it does not
#'     # standardize arguments across model types. `summary = FALSE` is not strictly necessary
#'     # with posterior_linpred(), but because it is necessary on some functions (otherwise
#'     # those functions return summaries instead of a matrix of draws) it is
#'     # included in this example.
#'     add_draws(posterior_linpred(m_mpg, newdata = ., summary = FALSE)) %>%
#'     ggplot(aes(x = hp, y = mpg, color = ordered(cyl))) +
#'     stat_lineribbon(aes(y = .value), alpha = 0.25) +
#'     geom_point(data = mtcars) +
#'     scale_fill_brewer(palette = "Greys")
#' }
#' }
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest_legacy
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble is_tibble
#' @importFrom rlang sym
#' @export
add_draws = function(data, draws, value = ".value") {
  groups = union(colnames(data), ".row")

  add_draws_list(data, draws, value = value) %>%
    unnest_legacy(.draw, !!sym(value)) %>%
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
