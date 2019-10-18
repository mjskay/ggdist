# combine_chains: combine the chains and iterations columns of tidy draws
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".chain", ".iteration"))


#' Combine the chain and iteration columns of tidy data frames of draws
#'
#' Combines the chain and iteration columns of a tidy data frame of draws from a Bayesian model fit into a new column
#' that can uniquely identify each draw. Generally speaking **not needed for pure tidybayes code**, as tidybayes
#' functions now automatically include a `.draw` column, but can be useful when interacting with packages that
#' do not provide such a column.
#'
#' @param data Tidy data frame of draws with columns representing the chain and iteration of each draw.
#' @param chain Bare name of column in `data` indicating the chain of each row. The default (`.chain`) is
#' the same as used by other functions in `tidybayes`.
#' @param iteration Bare name of column in `data` indicating the iteration of each row. The default
#' (`.iteration`) is the same as used by other functions in `tidybayes`.
#' @param into Name (as a character vector) of the column to combine chains into. The default, `NULL`, replaces the
#' `chain` column with `NA`s and writes the combined chain iteration numbers into `iteration`. If
#' provided, `chain` and `iteration` will not be modified, and the combined iteration number will be written
#' into a new column named `into`.
#'
#' @return A data frame of tidy draws with a combined iteration column
#'
#' @author Matthew Kay
#' @seealso [emmeans::emmeans()]
#' @keywords manip
#' @examples
#'
#' library(magrittr)
#' library(coda)
#'
#' data(line, package = "coda")
#'
#' # The `line` posterior has two chains with 200 iterations each:
#' line %>%
#'   tidy_draws() %>%
#'   summary()
#'
#' # combine_chains combines the chain and iteration column into the .draw column.
#' line %>%
#'   tidy_draws() %>%
#'   combine_chains() %>%
#'   summary()
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr mutate pull
#' @importFrom rlang enquo :=
#' @export
combine_chains = function(data, chain = .chain, iteration = .iteration, into = ".draw") {
  chain = enquo(chain)
  iteration = enquo(iteration)

  max_iteration = data %>%
    pull(!!iteration) %>%
    max()

  data %<>% mutate(
    !!into := as.integer(ifelse(is.na(!!chain), 0, (!!chain) - 1) * (!!max_iteration) + !!iteration)
  )

  data
}
