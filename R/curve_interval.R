# curve_interval function for constructing curve boxplots
#
# Author: mjskay
###############################################################################

#' Curvewise point and interval summaries for tidy data frames of draws from distributions
#'
#' Translates draws from distributions in a grouped data frame into a set of point and
#' interval summaries using a curve boxplot-inspired approach.
#'
#' TODO: insert algorithm details
#'
#' @param .data Data frame (or grouped data frame as returned by [group_by()])
#' that contains draws to summarize.
#' @param ... Bare column names or expressions that, when evaluated in the context of
#' `.data`, represent draws to summarize. If this is empty, then by default all
#' columns that are not group columns and which are not in `.exclude` (by default
#' `".chain"`, `".iteration"`, `".draw"`, and `".row"`) will be summarized.
#' This can be list columns.
#' @param .along Which columns are the input values to the function describing the curve (e.g., the "x"
#' values). Supports tidyselect syntax, as in `dplyr::select()`. Intervals are calculated jointly with
#' respect to these variables, conditional on all other grouping variables in the data frame. The default
#' (`NULL`) causes `curve_interval()` to use all grouping variables in the input data frame, which will
#' generate the most conservative intervals. However, if you want to calculate intervals for some function
#' `y = f(x)` conditional on some other variable(s) (say, conditional on a factor `g`), you would group by
#' `g`, then use `.along = x` to calculate intervals jointly over `x` conditional on `g`.
#' @param .width vector of probabilities to use that determine the widths of the resulting intervals.
#' If multiple probabilities are provided, multiple rows per group are generated, each with
#' a different probability interval (and value of the corresponding `.width` column).
#' @param .simple_names When `TRUE` and only a single column / vector is to be summarized, use the
#' name `.lower` for the lower end of the interval and `.upper` for the
#' upper end. If `.data` is a vector and this is `TRUE`, this will also set the column name
#' of the point summary to `.value`. When `FALSE` and `.data` is a data frame,
#' names the lower and upper intervals for each column `x` `x.lower` and `x.upper`.
#' When `FALSE` and `.data` is a vector, uses the naming scheme `y`, `ymin`
#' and `ymax` (for use with ggplot).
#' @param .exclude A character vector of names of columns to be excluded from summarization
#' if no column names are specified to be summarized. Default ignores several meta-data column
#' names used in tidybayes.
#' @param na.rm logical value indicating whether `NA` values should be stripped before the computation proceeds.
#' If `FALSE` (the default), any vectors to be summarized that contain `NA` will result in
#' point and interval summaries equal to `NA`.
#' @param x vector to summarize (for interval functions: `qi` and `hdi`)
#' @return A data frame containing point summaries and intervals, with at least one column corresponding
#' to the point summary, one to the lower end of the interval, one to the upper end of the interval, the
#' width of the interval (`.width`), the type of point summary (`.point`), and the type of interval (`.interval`).
#' @author Matthew Kay
#' @examples
#'
#' @importFrom purrr map_dfr map map2 discard map_dbl map_lgl iwalk
#' @importFrom dplyr group_vars summarise_at %>% group_split across
#' @importFrom rlang quos quos_auto_name eval_tidy quo_get_expr
#' @importFrom tidyselect eval_select
#' @importFrom stats median
#' @export
curve_interval = function(.data, ..., .along = NULL, .width = .95, .simple_names = TRUE,
  na.rm = FALSE, .exclude = c(".chain", ".iteration", ".draw", ".row")
) {
  data = .data    # to avoid conflicts with tidy eval's `.data` pronoun
  col_exprs = quos(..., .named = TRUE)
  point_name = "halfspace_depth"
  interval_name = "halfspace_depth"
  # get the grouping variables we will jointly calculate intervals on
  .along = enquo(.along)
  if (is.null(quo_get_expr(.along))) {
    .along = group_vars(data)
  } else {
    .along = names(data)[eval_select(.along, data)]
    data = group_by_at(data, .along, .add = TRUE)
  }
  # get the groups we will condition before doing the joint intervals
  .conditional_groups = setdiff(group_vars(data), .along)

  if (length(col_exprs) == 0) {
    # no column expressions provided => summarise all columns that are not groups and which
    # are not in .exclude
    col_exprs = names(data) %>%
      #don't aggregate groups because we aggregate within these
      setdiff(group_vars(data)) %>%
      setdiff(.exclude) %>%
      # have to use quos here because lists of symbols don't work correctly with iwalk() for some reason
      # (the simpler version of this line would be `syms() %>%`)
      map(~ quo(!!sym(.))) %>%
      quos_auto_name()

    if (length(col_exprs) == 0) {
      #still nothing to aggregate? not sure what the user wants
      stop("No columns found to calculate point and interval summaries for.")
    }
  }

  result = if (length(col_exprs) == 1 && .simple_names) {
    # only one column provided => summarise that column and use ".lower" and ".upper" as
    # the generated column names for consistency with tidy() in broom
    col_expr = col_exprs[[1]]
    col_name = names(col_exprs)

    # evaluate the expression that will result in the draws we want to summarise
    data[[col_name]] = eval_tidy(col_expr, data)

    # if the value we are going to summarise is not already a list column, make it into a list column
    # (making it a list column first is faster than anything else I've tried)
    if (!is.list(data[[col_name]])) {
      data = summarise_at(data, col_name, list)
    }

    .curve_interval(data, col_name, ".lower", ".upper", .width, .conditional_groups)
  } else {
    iwalk(col_exprs, function(col_expr, col_name) {
      data[[col_name]] <<- eval_tidy(col_expr, data)
    })

    # if the values we are going to summarise are not already list columns, make them into list columns
    # (making them list columns first is faster than anything else I've tried)
    if (!all(map_lgl(data[,names(col_exprs)], is.list))) {
      data = summarise_at(data, names(col_exprs), list)
    }

    for (col_name in names(col_exprs)) {
      data = .curve_interval(
        data, col_name, paste0(col_name, ".lower"), paste0(col_name, ".upper"), .width, .conditional_groups
      )
    }

    data
  }

  result[[".point"]] = point_name
  result[[".interval"]] = interval_name

  result
}

halfspace_depth = function(x) {
  rank_x = rank(x)
  n = length(x)
  pmin(rank_x/n, (n + 1 - rank_x)/n)
}

.curve_interval = function(data, col_name, lower, upper, .width, .conditional_groups) {
  if (length(unique(lengths(data[[col_name]]))) != 1) {
    stop("Must have the same number of values in each group.")
  }

  dfs = group_split(group_by_at(data, .conditional_groups))

  map_dfr(dfs, function(d) {
    # draws x y matrix
    draws = do.call(cbind, d[[col_name]])
    # draws x depth matrix
    depths = apply(draws, 2, halfspace_depth)
    # mean depth of each draw
    mean_depth = rowMeans(depths)

    # median draw = the one with the maximum depth
    median_draw = which.max(mean_depth)
    median_y = map_dbl(d[[col_name]], `[[`, median_draw)

    map_dfr(.width, function(w) {
      depth_cutoff = quantile(mean_depth, 1 - w)
      selected_draws = mean_depth >= depth_cutoff

      selected_y = lapply(d[[col_name]], `[`, selected_draws)
      d[[lower]] = map_dbl(selected_y, min)
      d[[upper]] = map_dbl(selected_y, max)
      d[[col_name]] = median_y
      d[[".width"]] = w
      d
    })
  })
}
