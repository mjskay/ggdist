# unspread_draws
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("..dimension_values"))



# deprecated names for unspread_draws --------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
unspread_samples = function(..., indices = c(".chain", ".iteration", ".draw")) {
  .Deprecated("unspread_draws", package = "tidybayes") # nocov
  unspread_draws(..., draw_indices = indices)               # nocov
}



#' Turn tidy data frames of variables from a Bayesian model back into untidy data
#'
#' Inverse operations of \code{\link{spread_draws}} and \code{\link{gather_draws}}, giving
#' results that look like \code{\link{tidy_draws}}.
#'
#' These functions take symbolic specifications of variable names and dimensions in the same format as
#' \code{\link{spread_draws}} and \code{\link{gather_draws}} and invert the tidy data frame back into
#' a data frame whose column names are variables with dimensions in them.
#'
#' @param data A tidy data frame of draws, such as one output by \code{spread_draws} or \code{gather_draws}.
#' @param ... Expressions in the form of
#' \code{variable_name[dimension_1, dimension_2, ...]}. See \code{\link{spread_draws}}.
#' @param draw_indices Character vector of column names in \code{data} that
#' should be treated as indices of draws. The default is \code{c(".chain",".iteration",".draw")},
#' which are the same names used for chain, iteration, and draw indices returned by
#' \code{\link{spread_draws}} or \code{\link{gather_draws}}.
#' @param drop_indices Drop the columns specified by \code{draw_indices} from the resulting data frame. Default \code{FALSE}.
#' @param variable The name of the column in \code{data} that contains the names of variables from the model.
#' @param value The name of the column in \code{data} that contains draws from the variables.
#' @return A data frame.
#' @author Matthew Kay
#' @seealso \code{\link{spread_draws}}, \code{\link{gather_draws}}, \code{\link{tidy_draws}}.
#' @keywords manip
#' @examples
#'
#' library(dplyr)
#'
#' data(RankCorr, package = "tidybayes")
#'
#' # We can use unspread_draws to allow us to manipulate draws with tidybayes
#' # and then transform the draws into a form we can use with packages like bayesplot.
#' # Here we subset b[i,j] to just values of i in 1:2 and j == 1, then plot with bayesplot
#' RankCorr %>%
#'   spread_draws(b[i,j]) %>%
#'   filter(i %in% 1:2, j == 1) %>%
#'   unspread_draws(b[i,j], drop_indices = TRUE) %>%
#'   bayesplot::mcmc_areas()
#'
#' # As another example, we could use compare_levels to plot all pairwise comparisons
#' # of b[1,j] for j in 1:3
#' RankCorr %>%
#'   spread_draws(b[i,j]) %>%
#'   filter(i == 1, j %in% 1:3) %>%
#'   compare_levels(b, by = j) %>%
#'   unspread_draws(b[j], drop_indices = TRUE) %>%
#'   bayesplot::mcmc_areas()
#'
#' @importFrom rlang enquos
#' @importFrom purrr map reduce
#' @importFrom dplyr inner_join ungroup select distinct mutate
#' @importFrom tidyr spread_ unite
#' @importFrom magrittr %<>% %>%
#' @rdname unspread_draws
#' @export
unspread_draws = function(data, ..., draw_indices = c(".chain", ".iteration", ".draw"), drop_indices = FALSE) {
  result =
    map(enquos(...), function(variable_spec) {
      unspread_draws_(data, variable_spec, draw_indices = draw_indices)
    }) %>%
    reduce(inner_join, by = draw_indices) %>%
    as_tibble()

  if (drop_indices) {
    result %>%
      select(-one_of(draw_indices))
  } else {
    result
  }
}

unspread_draws_ = function(data, variable_spec, draw_indices = c(".chain", ".iteration", ".draw")) {
  #parse a variable spec in the form variable_name[dimension_name_1, dimension_name_2, ..] | wide_dimension
  spec = parse_variable_spec(variable_spec)
  variable_names = spec[[1]]
  dimension_names = spec[[2]]
  wide_dimension_name = spec[[3]]

  if (!is.null(wide_dimension_name)) {
    stop("unspread_draws does not support the wide dimension syntax (`|`).")
  }

  # generate the subset of the data that has just the variable names and indices in question
  # we also have to ungroup() here because otherwise grouping columns that are not involved in this variable
  # will be automatically retained even when we try to select() them out.
  data_subset = data %>%
    ungroup() %>%
    select(!!c(draw_indices, variable_names, dimension_names))

  if (is.null(dimension_names)) {
    return(distinct(data_subset))
  }

  # we have to do distinct() here in case a variable had duplicates created for dimensions of
  # other variables that it does not share; e.g. in the case of spread_draws(a, b[i]) %>% unspread_draws(a)
  data_distinct = data_subset %>%
    unite(..dimension_values, !!!dimension_names, sep = ",") %>%
    distinct()

  map(variable_names, function(variable_name) {
    data_distinct %>%
      select(!!c(draw_indices, variable_name, "..dimension_values")) %>%
      mutate(..variable = paste0(variable_name, "[", ..dimension_values, "]")) %>%
      select(-..dimension_values) %>%
      spread_("..variable", variable_name)
  }) %>%
    reduce(inner_join, by = draw_indices)
}
