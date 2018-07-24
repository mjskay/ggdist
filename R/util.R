# Utility functions for tidybayes
#
# Author: mjskay
###############################################################################


# deparse that is guaranteed to return a single string (instead of
# a list of strings if the expression goes to multiple lines)
deparse0 = function(expr, width.cutoff = 500, ...) {
  paste0(deparse(expr, width.cutoff = width.cutoff, ...), collapse = "")
}

# Based on https://stackoverflow.com/a/14838753
# Escapes a string for inclusion in a regex
#' @importFrom stringr str_replace_all
escape_regex <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\1")
}

combine_chains_for_deprecated_ = function(x) {
  x$.chain = as.integer(NA)
  x$.iteration = x$.draw
  x$.draw = NULL
  x
}
