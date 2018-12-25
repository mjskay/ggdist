# compare_levels
#
# Author: mjskay
###############################################################################


# comparison types --------------------------------------------------------

comparison_types = within(list(), {
  ordered = function(x) {
    l = levels(x)
    lapply(2:length(l), function(i) c(l[[i]], l[[i - 1]]))
  }

  control = function(x) {
    l = levels(x)
    lapply(l[-1], function(j) c(j, l[[1]]))
  }

  pairwise = function(x) {
    #reverse combn so that the control level (first level) is second for
    #consistency with control() and ordered()
    lapply(combn(levels(x), 2, simplify = FALSE), rev)
  }

  default = function(x) {
    if (is.ordered(x)) ordered(x)
    else pairwise(x)
  }
})



# compare_levels ----------------------------------------------------------

#' Compare the value of draws of some variable from a Bayesian model for
#' different levels of a factor
#'
#' Given posterior draws from a Bayesian model in long format (e.g. as
#' returned by \code{\link{spread_draws}}), compare the value of a variable in those draws
#' across different paired combinations of levels of a factor.
#'
#' This function simplifies conducting comparisons across levels of some
#' variable in a tidy data frame of draws. It applies \code{fun} to all
#' values of \code{variable} for each pair of levels of \code{by} as selected
#' by \code{comparison}. By default, all pairwise comparisons are generated if
#' \code{by} is an unordered \code{factor} and ordered comparisons are made if
#' \code{by} is \code{ordered}.
#'
#' The included \code{comparison} types are: \itemize{ \item \code{ordered}:
#' compare each level \code{i} with level \code{i - 1}; e.g. \code{fun(i, i -
#' 1)} \item \code{pairwise}: compare each level of \code{by} with every other
#' level.  \item \code{control}: compare each level of \code{by} with the first
#' level of \code{by}.  If you wish to compare with a different level, you can
#' first apply \code{\link{relevel}} to \code{by} to set the control
#' (reference) level.  \item \code{default}: use \code{ordered} if
#' \code{is.ordered(by)} and \code{pairwise} otherwise.  }
#'
#' @param data Long-format \code{data.frame} of draws such as returned by
#' \code{\link{spread_draws}} or \code{\link{gather_draws}}. If \code{data}
#' is a grouped data frame, comparisons will be made within groups (if
#' one of the groups in the data frame is the \code{by} column, that specific
#' group will be ignored, as it is not possible to make comparisons both
#' within some variable and across it simultaneously).
#' @param variable Bare (unquoted) name of a column in data representing the
#' variable to compare across levels.
#' @param by Bare (unquoted) name of a column in data that is a
#' \code{factor} or \code{ordered}. The value of \code{variable} will be
#' compared across pairs of levels of this \code{factor}.
#' @param fun Binary function to use for comparison. For each pair of levels of
#' \code{by} we are comparing (as determined by \code{comparison}), compute the
#' result of this function.
#' @param comparison One of (a) the comparison types \code{ordered},
#' \code{control}, \code{pairwise}, or \code{default} (may also be given as
#' strings, e.g. \code{"ordered"}), see `Details`; (b) a user-specified
#' function that takes a \code{factor} and returns a list of pairs of names of
#' levels to compare (as strings) and/or unevaluated expressions containing
#' representing the comparisons to make; or (c) a list of pairs of names of
#' levels to compare (as strings) and/or unevaluated expressions representing
#' the comparisons to make, e.g.: \code{list(c("a", "b"), c("b", "c"))} or
#' \code{.(a - b, b - c)}, both of which would compare level \code{"a"} against
#' \code{"b"} and level \code{"b"} against \code{"c"}. Note that the
#' unevaluated expression syntax ignores the \code{fun} argument, can include
#' any other functions desired (e.g. variable transformations), and can even
#' include more than two levels or other columns in \code{data}.
#' @param draw_indices Character vector of column names in \code{data} that
#' should be treated as indices when making the comparison (i.e. values of
#' \code{variable} within each level of \code{by} will be compared at each
#' unique combination of levels of \code{draw_indices}). Columns in \code{draw_indices}
#' not found in \code{data} are ignored. The default is \code{c(".chain",".iteration",".draw")},
#' which are the same names used for chain/iteration/draw indices returned by
#' \code{\link{spread_draws}} or \code{\link{gather_draws}}; thus if you are using \code{compare_levels}
#' with \code{\link{spread_draws}} or \code{\link{gather_draws}} you generally should not need to change this
#' value.
#' @param ignore_groups character vector of names of groups to ignore by
#' default in the input grouping. This is primarily provided to make it
#' easier to pipe output of \code{\link{add_fitted_draws}} into this function,
#' as that function provides a \code{".row"} output column that is grouped,
#' but which is virtually never desired to group by when using \code{compare_levels}.
#' @return A \code{data.frame} with the same columns as \code{data}, except
#' that the \code{by} column contains a symbolic representation of the
#' comparison of pairs of levels of \code{by} in \code{data}, and
#' \code{variable} contains the result of that comparison.
#' @author Matthew Kay
#' @seealso \code{\link{spread_draws}} and \code{\link{gather_draws}}.
#' @keywords manip
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' data(RankCorr, package = "tidybayes")
#'
#' # Let's do all pairwise comparisons of b[i,1]:
#' RankCorr %>%
#'   spread_draws(b[i,j]) %>%
#'   filter(j == 1) %>%
#'   compare_levels(b, by = i) %>%
#'   median_qi()
#'
#' # Or let's plot all comparisons against the first level (control):
#' RankCorr %>%
#'   spread_draws(b[i,j]) %>%
#'   filter(j == 1) %>%
#'   compare_levels(b, by = i, comparison = control) %>%
#'   ggplot(aes(x = b, y = i)) +
#'   geom_halfeyeh()
#'
#' # Or let's plot comparisons of all levels of j within
#' # all levels of i
#' RankCorr %>%
#'   spread_draws(b[i,j]) %>%
#'   group_by(i) %>%
#'   compare_levels(b, by = j) %>%
#'   ggplot(aes(x = b, y = j)) +
#'   geom_halfeyeh() +
#'   facet_grid(cols = vars(i))
#'
#' @importFrom tidyselect vars_pull
#' @importFrom plyr ldply
#' @importFrom tidyr spread_
#' @importFrom dplyr one_of
#' @importFrom dplyr as_tibble
#' @importFrom rlang sym quo_name eval_tidy
#' @export
compare_levels = function(data, variable, by, fun=`-`, comparison = "default",
    draw_indices = c(".chain", ".iteration", ".draw"),
    ignore_groups = ".row"
  ) {
  variable = vars_pull(names(data), !!enquo(variable))
  by = vars_pull(names(data), !!enquo(by))
  fun = enquo(fun)
  comparison = enquo(comparison)
  groups_ = setdiff(group_vars(data), ignore_groups)

  data %>%
    group_by_at(setdiff(groups_, by)) %>%
    do(compare_levels_(., variable, by, fun, comparison, draw_indices)) %>%
    group_by_at(union(groups_, by))
}


compare_levels_ = function(data, variable, by, fun, comparison, draw_indices) {
  #drop unused levels from "by" column
  data[[by]] = factor(data[[by]])

  #drop all unused columns before changing to wide format
  draw_indices = intersect(draw_indices, names(data)) #don't include index columns that aren't in data
  data = data[, union(draw_indices, c(variable, by))]

  #get wide version of data that we can use to generate comparisons easily
  data_wide = spread_(data, by, variable)

  # determine a pretty function name
  fun_name = if (is.name(fun[[2]])) quo_name(fun) else ":"
  fun = eval_tidy(fun)

  #get a version of the data data frame without columns representing
  #the levels we are comparing by (these columns will be included
  #alongside the comparison results for reference)
  by_levels = levels(data[[by]])
  data_wide_no_levels = select(data_wide, -one_of(by_levels))

  #get list of pairs of levels to compare
  if (is.character(comparison[[2]])) comparison = as.name(comparison[[2]])
  comparison_function = eval_tidy(comparison, comparison_types)
  comparison_levels =
    if (is.list(comparison_function)) comparison_function
  else comparison_function(data[[by]])

  #make comparisons
  ldply(comparison_levels, function(levels.) {
    comparison = if (is.language(levels.)) {
      #user-supplied quoted expressions are evaluated within the data frame
      data.frame(
        by = quo_name(levels.),
        variable = eval_tidy(levels., data_wide)
      )
    }
    else {
      #otherwise, levels should be pairs of strings representing levels
      data.frame(
        by = paste(levels.[[1]], fun_name, levels.[[2]]),
        variable = fun(data_wide[[levels.[[1]]]], data_wide[[levels.[[2]]]])
      )
    }

    names(comparison) = c(by, variable)
    cbind(data_wide_no_levels, comparison)
  })
}
