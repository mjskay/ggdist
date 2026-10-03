# S7 validation methods
#
# Author: mjskay
###############################################################################

validate_nonnegative_scalar = function(value) {
  if (!isTRUE(value >= 0)) {
    "must be a non-negative scalar."
  }
}

validate_positive_scalar = function(value) {
  if (!isTRUE(value > 0)) {
    "must be a positive scalar."
  }
}

validate_unit_scalar = function(value) {
  if (!isTRUE(0 <= value && value <= 1)) {
    "must be a scalar between 0 and 1."
  }
}

validate_not_na = function(value) {
  if (anyNA(value)) {
    "must not contain NA."
  }
}

validate_positive_scalar_integerish = function(value) {
  validate_positive_scalar(value) %||% if (!is.infinite(value) && as.integer(value) != value) {
    "must be an integer or integer-like."
  }
}

validate_in = function(allowed) function(value) {
  if (!value %in% allowed) {
    paste0("must be one of: ", paste(allowed, collapse = ", "), ".")
  }
}
