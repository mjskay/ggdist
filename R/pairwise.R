# pairwise: generate pairwise combinations of values key/value columns in a long-format data frame
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".chain", ".iteration"))


#' Pairwise combinations of values from key/value columns in a tidy data frame
#'
#' Fast method for producing combinations of values in a value column for different levels of a key column,
#' assuming long-format (tidy) data with an equal number of values per key. Among other things, this is
#' useful for producing scatter-plot matrices.
#'
#' @param data Tidy data frame.
#' @param key Bare name of column in \code{data} containing the key .
#' @param value Bare name of column in \code{data} containg the value.
#' @param triangle Should the upper or lower triangle of the matrix of all possible combinations be returned?
#' The default, \code{"lower only"}, returns the lower triangle without the diagonal; \code{"lower"} returns
#' the lower triangle with the diagonal (\code{"upper"} and \code{"upper only"} operate analogously), and
#' \code{"both"} returns the full set of possible combinations.
#'
#' This method is particularly useful for constructing scatterplot matrices. See examples below.
#'
#' @return A tidy data frame of combinations of values in \code{key} and \code{value}, with columns \code{.row}
#' and \code{.col} containing values from \code{key}, and columns \code{.y} and \code{.x} containing values
#' from \code{value}.
#'
#' @author Matthew Kay
#' @seealso \code{\link[emmeans]{emmeans}}
#' @keywords manip
#' @examples
#'
#' library(ggplot2)
#' library(dplyr)
#'
#' t_a = rnorm(100)
#' t_b = rnorm(100, t_a * 2)
#' t_c = rnorm(100)
#'
#' df = rbind(
#'   data.frame(g = "a", t = t_a),
#'   data.frame(g = "b", t = t_b),
#'   data.frame(g = "c", t = t_c)
#' )
#'
#' df %>%
#'   pairwise(g, t) %>%
#'   ggplot(aes(.x, .y)) +
#'   geom_point() +
#'   facet_grid(vars(.row), vars(.col))
#'
#' df %>%
#'   pairwise(g, t, triangle = "upper") %>%
#'   ggplot(aes(.x, .y)) +
#'   geom_point() +
#'   facet_grid(vars(.row), vars(.col))
#'
#' df %>%
#'   pairwise(g, t, triangle = "both") %>%
#'   ggplot(aes(.x, .y)) +
#'   geom_point() +
#'   facet_grid(vars(.row), vars(.col))
#'
#' data(line, package = "coda")
#'
#' line %>%
#'   tidy_draws() %>%
#'   gather_variables() %>%
#'   pairwise(.variable, .value) %>%
#'   ggplot(aes(.x, .y)) +
#'   geom_point(alpha = .25) +
#'   facet_grid(vars(.row), vars(.col))
#'
#' line %>%
#'   tidy_draws() %>%
#'   gather_variables() %>%
#'   pairwise(.variable, .value) %>%
#'   ggplot(aes(.x, .y, color = factor(.chain))) +
#'   geom_density_2d(alpha = .5) +
#'   facet_grid(vars(.row), vars(.col))
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom purrr imap_dfr map
#' @importFrom plyr dlply
#' @importFrom dplyr rename group_by_at ungroup group_vars
#' @export
pairwise = function(data, key, value, triangle = c("lower only", "upper only", "lower", "upper", "both")) {
  key = enquo(key)
  value = enquo(value)

  data %<>%
    rename(.row = !!key, .y = !!value)

  if (!is.factor(data[[".row"]])) {
    data[[".row"]] = factor(data[[".row"]])
  }

  # the approach to looking up levels ensures that the ordering is based
  # on the order of level names in the original variable, not on string order
  levels_ = 1:nlevels(data[[".row"]])
  names(levels_) = levels(data[[".row"]])
  triangle_test = switch(match.arg(triangle),
    `lower only` = function(row, col) levels_[[row]] > levels_[[col]],
    `upper only` = function(row, col) levels_[[row]] < levels_[[col]],
    lower = function(row, col) levels_[[row]] >= levels_[[col]],
    upper = function(row, col) levels_[[row]] <= levels_[[col]],
    both = function(row, col) TRUE
  )

  groups_ = group_vars(data) %>%
    setdiff(".row")

  data %>%
    ungroup() %>%
    pairwise_(triangle_test) %>%
    group_by_at(c(groups_, ".row", ".col"))
}

pairwise_ = function(data, triangle_test) {
  row_data = dlply(data, ".row", function(d) d)
  col_data = map(row_data, . %>% select(.col = .row, .x = .y))
  imap_dfr(row_data, function(row_df, row) {
    imap_dfr(col_data, function(col_df, col) {
      if (triangle_test(row, col)) {
        cbind(row_df, col_df)
      }
    })
  })
}
