# combine_chains: combine the chains and iterations columns of tidy samples
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".chain"))


#' Combine the chain and iteration columns of tidy samples
#'
#' Combines the chain and iteration columns of a tidy data frame of samples from a Bayesian model fit by renumbering
#' iterations to ensure that the iteration column alone can uniquely identify each draw. Can be useful for making plots
#' showing multiple iterations where you do not care about differences among chains.
#'
#' @param samples Tidy data frame of samples with columns representing the chain and iteration of each sample, such as
#' returned by \code{\link{as_sample_tibble}}, \code{\link{spread_samples}}, or \code{\link{gather_samples}}.
#' @param chain Bare name of column in \code{samples} indicating the chain of each row. The default (\code{.chain}) is
#' the same as returned by other functions in \code{tidybayes}.
#' @param iteration Bare name of column in \code{samples} indicating the iteration of each row. The default
#' (\code{.iteration}) is the same as returned by other functions in \code{tidybayes}.
#' @param into Name (as a character vector) of the column to combine chains into. The default, \code{NULL}, replaces the
#' \code{chain} column with \code{NA}s and writes the combined chain iteration numbers into \code{iteration}. If
#' provided, \code{chain} and \code{iteration} will not be modified, and the combined iteration number will be written
#' into a new column named \code{into}.
#'
#' @return A data frame of tidy samples with a combined iteration column
#'
#' @author Matthew Kay
#' @seealso \code{\link[emmeans]{emmeans}}
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
#'   as_sample_tibble() %>%
#'   summary()
#'
#' # combine_chains combines the chains, giving 400 iterations with
#' # `.chain = NA` (to indicate that the chains were combined, as opposed to
#' # this being 400 iterations from one chain).
#' line %>%
#'   as_sample_tibble() %>%
#'   combine_chains() %>%
#'   summary()
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr mutate pull
#' @importFrom rlang enquo :=
#' @export
combine_chains = function(samples, chain = .chain, iteration = .iteration, into = NULL) {
  chain = enquo(chain)
  iteration = enquo(iteration)

  into_col = if (is.null(into)) {
    iteration[[2]]
  } else {
    into
  }

  max_iteration = samples %>%
    pull(!!iteration) %>%
    max()
  samples %<>% mutate(
    !!into_col := as.integer(ifelse(is.na(!!chain), 0, (!!chain) - 1) * (!!max_iteration) + !!iteration)
  )

  if (is.null(into)) {
    samples %<>% mutate(
      !!chain[[2]] := as.integer(NA)
    )
  }

  samples
}
