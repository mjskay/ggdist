# ungather_draws
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("..dimension_values"))



# deprecated names for ungather_draws --------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
ungather_samples = function(...) {
  .Deprecated("ungather_draws")     # nocov
  ungather_draws(...)               # nocov
}



#' @rdname unspread_draws
#' @export
ungather_draws = function(
  data, ..., term = "term", estimate = "estimate", indices = c(".chain", ".iteration", ".draw"), drop_indices = FALSE
) {

  result =
    map(lazy_dots(...), function(variable_spec) {
      ungather_draws_(data, variable_spec, term = term, estimate = estimate, indices = indices)
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

ungather_draws_ = function(
  data, variable_spec, term = "term", estimate = "estimate", indices = c(".chain", ".iteration", ".draw")
) {

  #parse a variable spec in the form variable_name[dimension_name_1, dimension_name_2, ..] | wide_dimension
  spec = parse_variable_spec(variable_spec)
  variable_names = spec[[1]]
  dimension_names = spec[[2]]
  wide_dimension_name = spec[[3]]

  if (!is.null(wide_dimension_name)) {
    stop("ungather_draws does not support the wide dimension syntax (`|`).")
  }

  # filter to desired rows and columns, removing duplicates (which may have been
  # introduced if `data` was the result of a call to `gather_terms`)
  data %<>%
    filter(.data[[!!term]] %in% !!variable_names) %>%
    ungroup() %>%
    select(!!c(indices, dimension_names, term, estimate)) %>%
    distinct()

  if (is.null(dimension_names)) {
    return(spread_(data, term, estimate))
  }

  data %<>%
    unite(..dimension_values, !!!dimension_names, sep = ",")

  data[[term]] = paste0(data[[term]], "[", data[["..dimension_values"]], "]")

  data %>%
    select(-..dimension_values) %>%
    spread_(term, estimate)
}
