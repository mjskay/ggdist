# Compatibility functions
# These functions allow compatibliity with specific packages if they are not
# installed, allowing them to be moved into Suggests.
#
# Author: mjskay
###############################################################################


# dplyr -------------------------------------------------------------------
#' Wrapper around dplyr::group_vars()
#' @noRd
group_vars_ = function(x) {
  if (requireNamespace("dplyr", quietly = TRUE)) {
    dplyr::group_vars(x)
  } else {
    # can't have grouped data frames without dplyr, so if dplyr isn't installed
    # assume we aren't getting grouped dfs
    character()
  }
}


# tidyselect --------------------------------------------------------------
#' Wrapper around tidyselect::eval_select() that works for a single bare symbol
#' and otherwise falls back to tidyselect::eval_select. This handles the majority
#' of cases where tidyselect is used in ggdist, allowing us to move tidyselect
#' to Suggests.
#' @importFrom rlang eval_tidy get_expr caller_env as_name
#' @noRd
eval_select_ = function(expr, data, env = caller_env(), ..., error_call = caller_env()) {
  raw_expr = get_expr(expr)
  if (is.symbol(raw_expr) || is.character(raw_expr)) {
    i = which(names(data) == as_name(expr))
    if (length(i) != 1) {
      cli_abort(
        c(
          "Column names must select exactly 1 column.",
          "x" = "Found {length(i)} columns named {.val {as_name(expr)}}."
        ),
        call = error_call,
        class = "ggdist_invalid_column_selection"
      )
    }
    i
  } else {
    stop_if_not_installed("tidyselect", "{.topic [tidyselect syntax](tidyselect::language)}", call = error_call)
    tidyselect::eval_select(expr, data, ..., error_call = error_call)
  }
}
