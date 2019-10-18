# Deprecated functions
#
# Author: mjskay
###############################################################################



#' Deprecated functions, arguments, and column names in tidybayes
#'
#' Deprecated functions, arguments, and column names and their alternatives are listed below.
#' Many of the deprecations are due to a naming scheme overhaul in tidybayes version 1.0.
#'
#' @section Deprecated Functions:
#'
#' Several deprecated versions of functions use slightly different output
#' formats (e.g., they use names like `term` and `estimate` where new
#' functions use `.variable` and `.value`; or they set `.iteration` even
#' when iteration information is not available --- new functions always set `.draw`
#' but may not set `.iteration`), so be careful when upgrading to new function names.
#' See *Deprecated Arguments and Column Names*, below, for more information.
#'
#' Deprecated functions are:
#'
#' \itemize{
#'
#'   \item `spread_samples`, `extract_samples`, and `tidy_samples` are
#'   deprecated names for [spread_draws()]. The spread/gather terminology
#'   better distinguishes the resulting data frame format, and *draws* is more
#'   correct terminology than *samples* for describing multiple realizations from
#'   a posterior distribution.
#'
#'   \item `gather_samples` is a deprecated name for [gather_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `unspread_samples` is a deprecated name for [unspread_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `ungather_samples` is a deprecated name for [ungather_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `fitted_samples` / `add_fitted_samples` are deprecated names for
#'   [fitted_draws()] / [add_fitted_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `predicted_samples` / `add_predicted_samples` are deprecated names for
#'   [predicted_draws()] / [add_predicted_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `gather_lsmeans_samples` and `gather_emmeans_samples` are deprecated aliases
#'   for [gather_emmeans_draws()]. The new name (estimated marginal means) is more
#'   appropriate for Bayesian models than the old name (least-squares means), and reflects the
#'   naming of the newer `emmeans` package. It also reflects
#'   a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `as_sample_tibble` and `as_sample_data_frame` are deprecated aliases
#'   for [tidy_draws()]. The original intent of `as_sample_tibble` was to be
#'   used primarily internally (hence its less user-friendly name); however, increasingly
#'   I have come across use cases of `tidy_draws` that warrant a more user-friendly name.
#'   It also reflects a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `ggeye` is deprecated: for a package whose goal is flexible and customizable
#'   visualization, monolithic functions are inflexible and do not sufficiently capitalize on users'
#'   existing knowledge of ggplot; instead, I think it is more flexible to design geoms and stats
#'   that can used within a complete ggplot workflow. [geom_eyeh()] offers a horizontal
#'   eye plot geom that can be used instead of `ggeye`.
#'
#' }
#'
#' @section Deprecated Arguments and Column Names:
#'
#' Versions of tidybayes before version 1.0 used a different naming scheme for several
#' arguments and output columns.
#'
#' Deprecated arguments and column names are:
#'
#' \itemize{
#'   \item `term` is now `.variable`
#'   \item `estimate` is now `.value`
#'   \item `pred` is now `.prediction`
#'   \item `conf.low` is now `.lower`
#'   \item `conf.high` is now `.upper`
#'   \item `.prob` is now `.width`
#'   \item The `.draw` column was added, and should be used instead of `.chain`
#'     and `.iteration` to uniquely identify draws when you do not care about chains. (`.chain` and
#'     `.iteration` are still provided for identifying draws *within* chains, if desired).
#' }
#'
#' To translate to/from the old naming scheme in output, use [to_broom_names()]
#' and [from_broom_names()].
#'
#' Many of these names were updated in version 1.0 in order to
#' make terminology more consistent and in order to satisfy these criteria:
#'
#' \itemize{
#'   \item Ignore compatibility with broom names on the assumption an adapter function can be created.
#'   \item Use names that could be compatible with frequentist approaches (hence `.width` instead of `.prob`).
#'   \item Always precede with "." to avoid collisions with variable names in models.
#'   \item No abbreviations (remembering if something is abbreviated or not can be a pain).
#'   \item No two-word names (multi-word names can always be standardized on and used in documentation, but I think data frame output should be succinct).
#'   \item Names should be nouns (I made an exception for lower/upper because they are common).
#' }
#'
#' @format NULL
#' @usage NULL
#' @author Matthew Kay
#' @name tidybayes-deprecated
#' @import ggplot2
#' @export
ggeye = function(data = NULL, mapping = NULL, ...) {
  .Deprecated("geom_eyeh", package = "tidybayes")
  ggplot(data = data, mapping = mapping) + geom_eye(...) + coord_flip()
}
