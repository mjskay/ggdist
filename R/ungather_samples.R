# ungather_samples
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("..index_values"))


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
