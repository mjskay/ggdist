# gather_variables
#
# Author: mjskay
###############################################################################



# deprecated names for gather_variables -----------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
gather_terms = function(...) {
  .Deprecated("gather_variables")       # nocov
  to_broom_names(gather_variables(...)) # nocov
}



# gather_variables --------------------------------------------------------

#' Gather variables from a tidy data frame of draws from variables into a single column
#'
#' Given a data frame such as might be returned by as_sample_tibble or spread_draws,
#' gather variables and their values from that data frame into a \code{".variable"} and \code{".value"} column.
#'
#' This function gathers every column except grouping columns and those matching the regular expression
#' \code{ignore_columns} into key/value columns \code{".variable"} and \code{".value"}.
#'
#' Imagine a data frame \code{data} as returned by \code{spread_draws(fit, a[i], b[i,v])}, like this:
#' \itemize{
#'      \item column \code{".chain"}: the chain number
#'      \item column \code{".iteration"}: the interation number
#'      \item column \code{".draw"}: the draw number
#'      \item column \code{"i"}: value in \code{1:5}
#'      \item column \code{"v"}: value in \code{1:10}
#'      \item column \code{"a"}: value of \code{"a[i]"} for draw number \code{".draw"}
#'      \item column \code{"b"}: value of \code{"b[i,v]"} for draw number \code{".draw"}
#'  }
#'
#' \code{gather_variables(data)} on that data frame would return a grouped
#' data frame (grouped by \code{i} and \code{v}), with:
#' \itemize{
#'      \item column \code{".chain"}: the chain number
#'      \item column \code{".iteration"}: the interation number
#'      \item column \code{".draw"}: the draw number
#'      \item column \code{"i"}: value in \code{1:5}
#'      \item column \code{"v"}: value in \code{1:10}
#'      \item column \code{".variable"}: value in \code{c("a", "b")}.
#'      \item column \code{".value"}: value of \code{"a[i]"} (when \code{".variable"} is \code{"a"};
#'          repeated for every value of \code{"v"}) or \code{"b[i,v]"} (when \code{".variable"} is
#'          \code{"b"}) for draw number \code{".draw"}
#'  }
#'
#' In this example, this call:
#'
#' \preformatted{gather_variables(data)}
#'
#' Is roughly equivalent to:
#'
#' \preformatted{data \%>\%
#'   gather(.variable, .value, -c(.chain, .iteration, .draw, i, v)) \%>\%
#'   group_by(.variable, add = TRUE)
#' }
#'
#' @param data A data frame with variable names spread across columns, such as one returned by
#' \code{\link{as_sample_tibble}} or \code{\link{spread_draws}}.
#' @param ignore_columns A regular expression that matches column names to ignore in the gather. The
#' default ignores columns that start with \code{"."}.
#' @return A data frame.
#' @author Matthew Kay
#' @seealso \code{\link{spread_draws}}, \code{\link{as_sample_tibble}}.
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(dplyr)
#'
#' data(RankCorr, package = "tidybayes")
#'
#' RankCorr %>%
#'   spread_draws(b[i,v], tau[i]) %>%
#'   gather_variables() %>%
#'   median_qi()
#'
#' # the first three lines below are roughly equivalent to ggmcmc::ggs(RankCorr)
#' RankCorr %>%
#'   as_sample_tibble() %>%
#'   gather_variables() %>%
#'   median_qi()
#'
#' }
#' @importFrom stringi stri_detect_regex
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom tidyr gather
#' @export
gather_variables = function(data, ignore_columns = "^\\..*") {
  # get a list of the names of columns that either start with "." or
  # which are grouping columns (these are indices from the spec)
  #  -> e.g. c(".chain", ".iteration", ".draw", "i")
  special_columns = names(data) %>%
    {.[stri_detect_regex(., ignore_columns) | . %in% groups(data)]}

  # translate that list into quoted negations of those column names
  # so we can exclude them from the gather()
  #  -> e.g. list(~ -.chain, ~ -.iteration, ~ -.draw, ~ -i)
  columns_excluded_from_gather = special_columns %>%
    map(~ quo(-!!as.name(.)))

  data %>%
    gather(".variable", ".value", !!!columns_excluded_from_gather) %>%
    group_by_at(".variable", add = TRUE)
}
