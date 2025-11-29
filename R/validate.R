# S7 validation methods
#
# Author: mjskay
###############################################################################

validate_positive_scalar = function(value) {
  if (!is.numeric(value) || length(value) != 1 || value <= 0) {
    "must be a positive scalar."
  }
}

validate_positive_scalar_integerish = function(value) {
  validate_positive_scalar(value) %||% if (as.integer(value) != value) {
    "must be an integer."
  }
}

validate_in = function(allowed) function(value) {
  if (!value %in% allowed) {
    paste0("must be one of: ", paste(allowed, collapse = ", "), ".")
  }
}
