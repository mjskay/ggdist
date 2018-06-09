# gather_samples
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".."))


#' @rdname spread_samples
#' @importFrom dplyr bind_rows
#' @export
gather_samples = function(model, ..., regex = FALSE, sep = "[, ]") {
  tidysamples = lapply(lazy_dots(...), function(variable_spec) {
    model %>%
      spread_samples_(variable_spec, regex = regex, sep = sep) %>%
      gather_terms()
  })

  #get the groups from all the samples --- when we bind them together,
  #the grouping information is not always retained, so we'll have to recreate
  #the full set of groups from all the data frames after we bind them
  groups_ = tidysamples %>%
    map(groups) %>%
    reduce(union)

  bind_rows(tidysamples) %>%
    group_by_(.dots = groups_)
}
