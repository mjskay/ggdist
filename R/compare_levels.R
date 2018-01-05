# compare_levels
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("default"))


#COMPARISON TYPES
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


#' Compare the value of some variable extracted from a Bayesian posterior
#' sample for different levels of a factor
#'
#' Given a posterior sample from a Bayesian sampler in long format (e.g. as
#' returned by spread_samples), compare the value of a variable in that sample
#' across different paired combinations of levels of a factor.
#'
#' This function simplifies conducting comparisons across levels of some
#' variable returned from a Bayesian sample. It applies \code{fun} to all
#' samples of \code{variable} for each pair of levels of \code{by} as selected
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
#' @param samples Long-format \code{data.frame} of samples such as returned by
#' \code{\link{spread_samples}} or \code{\link{gather_samples}}.
#' @param variable Bare (unquoted) name of a column in samples representing the
#' variable to compare across levels.
#' @param by Bare (unquoted) name of a column in samples that is a
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
#' include more than two levels or other columns in \code{samples}.
#' @param indices Character vector of column names in \code{samples} that
#' should be treated as indices when making the comparison (i.e. values of
#' \code{variable} within each level of \code{by} will be compared at each
#' unique combination of levels of \code{indices}). Columns in \code{indices}
#' not found in \code{samples} are ignored. The default is \code{c(".chain",".iteration")},
#' which are the same names used for chain/iteration indices variables returned by
#' \code{\link{spread_samples}} or \code{\link{gather_samples}}; thus if you are using \code{compare_levels}
#' with \code{\link{spread_samples}} or \code{\link{gather_samples}} you generally should not need to change this
#' value.
#' @return A \code{data.frame} with the same columns as \code{samples}, except
#' that the \code{by} column contains a symbolic representation of the
#' comparison of pairs of levels of \code{by} in \code{samples}, and
#' \code{variable} contains the result of that comparison.
#' @author Matthew Kay
#' @seealso \code{\link{spread_samples}} and \code{\link{gather_samples}}.
#' @keywords manip
#' @examples
#'
#' library(magrittr)
#' library(ggplot2)
#'
#' data(RankCorr, package = "tidybayes")
#'
#' # Let's do all pairwise comparisons of b[i,1] for i in 1:3:
#' RankCorr %>%
#'   spread_samples(b[i,j]) %>%
#'   filter(i %in% 1:3, j == 1) %>%
#'   compare_levels(b, by = i) %>%
#'   mean_qi()
#'
#' # Or let's plot all comparisons against the first level (control):
#' RankCorr %>%
#'   spread_samples(b[i,j]) %>%
#'   filter(j == 1) %>%
#'   compare_levels(b, by = i, comparison = control) %>%
#'   ggplot(aes(x = b, y = i)) +
#'   geom_halfeyeh()
#'
#' @export
compare_levels = function(samples, variable, by, fun=`-`, comparison = default, indices = c(".chain", ".iteration")) {
  eval(bquote(compare_levels_(samples,
    .(deparse0(substitute(variable))),
    .(deparse0(substitute(by))),
    .(substitute(fun)),
    .(substitute(comparison),
      .(indices))
  )))
}

#' @importFrom plyr ldply
#' @importFrom tidyr spread_
#' @importFrom dplyr one_of
#' @importFrom tibble as_tibble
#' @importFrom rlang sym
compare_levels_ = function(samples, variable, by, fun=`-`, comparison = default, indices = c(".chain", ".iteration")) {
  #drop unused levels from "by" column
  samples[[by]] = factor(samples[[by]])

  #drop all unused columns before changing to wide format
  indices = intersect(indices, names(samples)) #don't include index columns that aren't in samples
  samples = samples[, union(indices, c(variable, by))]

  #get wide version of samples that we can use to generate comparisons easily
  samples_wide = spread_(samples, by, variable)

  # determine a pretty function name
  fun_language = substitute(fun)
  fun_name = if (is.name(fun_language)) deparse0(fun_language) else ":"

  #get a version of the samples data frame without columns representing
  #the levels we are comparing by (these columns will be included
  #alongside the comparison results for reference)
  by_levels = levels(samples[[by]])
  samples_wide_no_levels = select(samples_wide, -one_of(by_levels))

  #get list of pairs of levels to compare
  comparison = substitute(comparison)
  if (is.character(comparison)) comparison = as.name(comparison)
  comparison_function = eval(comparison, comparison_types)
  comparison_levels =
    if (is.list(comparison_function)) comparison_function
  else comparison_function(samples[[by]])

  #make comparisons
  ldply(comparison_levels, function(levels.) {
    comparison = if (is.language(levels.)) {
      #user-supplied quoted expressions are evaluated within the data frame
      data.frame(
        by = deparse0(levels.),
        variable = eval(levels., samples_wide)
      )
    }
    else {
      #otherwise, levels should be pairs of strings representing levels
      data.frame(
        by = paste(levels.[[1]], fun_name, levels.[[2]]),
        variable = fun(samples_wide[[levels.[[1]]]], samples_wide[[levels.[[2]]]])
      )
    }
    names(comparison) = c(by, variable)
    cbind(samples_wide_no_levels, comparison)
  }) %>%
    group_by(!!sym(by))
}
