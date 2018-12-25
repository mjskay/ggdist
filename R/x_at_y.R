# x_at_y: generates a nested index for values of x given a value of y
#
# Author: mjskay
###############################################################################


#' Generate lookup vectors for composing nested indices
#'
#' Generates a lookup vector such that \code{x_at_y(x, y)[y] == x}. Particularly useful
#' for generating lookup tables for nested indices in conjunction with \code{\link{compose_data}}.
#'
#' \code{x_at_y(x, y)} returns a vector \code{k} such that \code{k[y] == x}. It also
#' fills in missing values in \code{y}: if \code{y} is an integer, \code{k} will contain
#' entries for all values from \code{1} to \code{max(y)}; if \code{y} is a factor,
#' \code{k} will contain entries for all values from \code{1} to \code{nlevels(y)}.
#' Missing values are replaced with \code{missing} (default \code{NA}).
#'
#' @param x Values in the resulting lookup vector. There should be only
#' one unique value of \code{x} for every corresponding value of \code{y}.
#' @param y Keys in the resulting lookup vector. Should be factors or integers.
#' @param missing Missing levels from \code{y} will be filled in with this value
#' in the resulting lookup vector. Default \code{NA}.
#' @author Matthew Kay
#' @seealso \code{\link{compose_data}}.
#' @examples
#'
#' library(magrittr)
#'
#' df = data.frame(
#'   plot = factor(paste0("p", rep(1:8, times = 2))),
#'   site = factor(paste0("s", rep(1:4, each = 2, times = 2)))
#' )
#'
#' # turns site into a nested index: site[p] gives the site for plot p
#' df %>%
#'   compose_data(site = x_at_y(site, plot))
#'
#' @importFrom rlang enquo quo_label is_integerish
#' @importFrom dplyr tibble group_by slice distinct left_join
#' @importFrom magrittr %$%
#' @export
x_at_y = function(x, y, missing = NA) {
  x_label = quo_label(enquo(x))
  y_label = quo_label(enquo(y))

  data = tibble(x = x, y = y)

  # make the index --- this will not include missing values
  index =
    data %>%
    group_by(y) %>%
    slice(1)

  if (nrow(distinct(data)) != nrow(index)) {
    stop(paste0(
      y_label, " does not appear to be nested in ", x_label,
      ": there are multiple values of ", x_label, " for at least one value of ", y_label
    ))
  }

  # fill in all values of y: if an integer, we want missing indices;
  # if a factor, we want levels that weren't present in the data.
  all_y =
    if (is.factor(y)) {
      factor(levels(y), levels = levels(y))
    } else if (is_integerish(y)) {
      if (min(y) < 1) {
        stop(paste0("All values of ", y_label, " must be >= 1. Got min(", y_label, ") == ", min(y)))
      }
      seq_len(max(y))
    } else {
      stop(paste0(
        "Cannot generate a lookup table for non-numeric / non-factor variable: ",
        y_label, " is of type ", deparse0(class(y))
      ))
    }

  #fill in any NAs
  index = tibble(y = all_y) %>%
    left_join(index, by = "y") %$%
    x

  index[is.na(index)] = missing

  index
}
