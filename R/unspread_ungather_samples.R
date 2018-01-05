# unspread_samples and ungather_samples
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("..index_values"))


#' Turn tidy data frames of parameters from a Bayesian model back into untidy data
#'
#' Inverse operations of \code{\link{spread_samples}} and \code{\link{gather_samples}}, giving
#' results that look like \code{\link{as_sample_tibble}}.
#'
#' These functions take symbolic specifications of parameter names and indices in the same format as
#' \code{\link{spread_samples}} and \code{\link{gather_samples}} and invert the tidy data frame back into
#' a data frame whose column names are parameters with indices in them.
#'
#' @param data A tidy data frame of samples, such as one output by \code{spread_samples} or \code{gather_samples}.
#' @param ... Expressions in the form of
#' \code{variable_name[index_1, index_2, ...]}. See \code{\link{spread_samples}}.
#' @param indices Character vector of column names in \code{data} that
#' should be treated as indices of chain/iteration. The default is \code{c(".chain",".iteration")},
#' which are the same names used for chain/iteration indices returned by
#' \code{\link{spread_samples}} or \code{\link{gather_samples}}.
#' @param drop_indices Drop the columns specified by \code{indices} from the resulting data frame. Default \code{FALSE}.
#' @param term The name of the column in \code{data} that contains the names of parameters from the model.
#' @param estimate The name of the column in \code{data} that contains the samples of the parameters.
#' @return A data frame.
#' @author Matthew Kay
#' @seealso \code{\link{spread_samples}}, \code{\link{gather_samples}}, \code{\link{as_sample_tibble}}.
#' @keywords manip
#' @examples
#'
#' library(magrittr)
#'
#' data(RankCorr, package = "tidybayes")
#'
#' # We can use unspread_samples to allow us to manipulate samples with tidybayes
#' # and then transform the samples into a form we can use with packages like bayesplot.
#' # Here we subset b[i,j] to just values of i in 1:3 and j == 1, then plot with bayesplot
#' RankCorr %>%
#'   spread_samples(b[i,j]) %>%
#'   filter(i %in% 1:3, j == 1) %>%
#'   unspread_samples(b[i,j], drop_indices = TRUE) %>%
#'   bayesplot::mcmc_areas()
#'
#' # As another example, we could use compare_levels to plot all pairwise comparisons
#' # of b[i,1] for i in 1:3
#' RankCorr %>%
#'   spread_samples(b[i,j]) %>%
#'   filter(i %in% 1:3, j == 1) %>%
#'   compare_levels(b, by = i) %>%
#'   unspread_samples(b[i], drop_indices = TRUE) %>%
#'   bayesplot::mcmc_areas()
#'
#' @importFrom lazyeval lazy_dots
#' @importFrom purrr map reduce
#' @importFrom dplyr inner_join ungroup select distinct mutate
#' @importFrom tidyr spread_ unite
#' @importFrom magrittr %<>% %>%
#' @rdname unspread_samples
#' @export
unspread_samples = function(data, ..., indices = c(".chain", ".iteration"), drop_indices = FALSE) {
  result =
    map(lazy_dots(...), function(variable_spec) {
      unspread_samples_(data, variable_spec, indices = indices)
    }) %>%
    reduce(inner_join, by = indices) %>%
    as_tibble()

  if (drop_indices) {
    result %>%
      select(-one_of(indices))
  } else {
    result
  }
}

unspread_samples_ = function(data, variable_spec, indices = c(".chain", ".iteration")) {
  #parse a variable spec in the form variable_name[index_name_1, index_name_2, ..] | wide_index
  spec = parse_variable_spec(variable_spec)
  variable_names = spec[[1]]
  index_names = spec[[2]]
  wide_index_name = spec[[3]]

  if (!is.null(wide_index_name)) {
    stop("unspread_samples does not support the wide indexing syntax (`|`).")
  }

  # generate the subset of the data that has just the variable names and indices in question
  # we also have to ungroup() here because otherwise grouping columns that are not involved in this variable
  # will be automatically retained even when we try to select() them out.
  data_subset = data %>%
    ungroup() %>%
    select(!!c(indices, variable_names, index_names))

  if (is.null(index_names)) {
    return(distinct(data_subset))
  }

  # we have to do distinct() here in case a variable had duplicates created for indices of
  # other variables that it does not share; e.g. in the case of spread_samples(a, b[i]) %>% unspread_samples(a)
  data_distinct = data_subset %>%
    unite(..index_values, !!!index_names, sep = ",") %>%
    distinct()

  map(variable_names, function(variable_name) {
    data_distinct %>%
      select(!!c(indices, variable_name, "..index_values")) %>%
      mutate(..term = paste0(variable_name, "[", ..index_values, "]")) %>%
      select(-..index_values) %>%
      spread_("..term", variable_name)
  }) %>%
    reduce(inner_join, by = indices)
}


#' @rdname unspread_samples
#' @export
ungather_samples = function(
  data, ..., term = "term", estimate = "estimate", indices = c(".chain", ".iteration"), drop_indices = FALSE
) {

  result =
    map(lazy_dots(...), function(variable_spec) {
      ungather_samples_(data, variable_spec, term = term, estimate = estimate, indices = indices)
    }) %>%
    reduce(inner_join, by = indices) %>%
    as_tibble()

  if (drop_indices) {
    result %>%
      select(-one_of(indices))
  } else {
    result
  }
}

ungather_samples_ = function(
  data, variable_spec, term = "term", estimate = "estimate", indices = c(".chain", ".iteration")
) {

  #parse a variable spec in the form variable_name[index_name_1, index_name_2, ..] | wide_index
  spec = parse_variable_spec(variable_spec)
  variable_names = spec[[1]]
  index_names = spec[[2]]
  wide_index_name = spec[[3]]

  if (!is.null(wide_index_name)) {
    stop("ungather_samples does not support the wide indexing syntax (`|`).")
  }

  # filter to desired rows and columns, removing duplicates (which may have been
  # introduced if `data` was the result of a call to `gather_terms`)
  data %<>%
    filter(.data[[!!term]] %in% !!variable_names) %>%
    ungroup() %>%
    select(!!c(indices, index_names, term, estimate)) %>%
    distinct()

  if (is.null(index_names)) {
    return(spread_(data, term, estimate))
  }

  data %<>%
    unite(..index_values, !!!index_names, sep = ",")

  data[[term]] = paste0(data[[term]], "[", data[["..index_values"]], "]")

  data %>%
    select(-..index_values) %>%
    spread_(term, estimate)
}
