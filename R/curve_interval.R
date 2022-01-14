# curve_interval function for constructing curve boxplots
#
# Author: mjskay
###############################################################################

#' Curvewise point and interval summaries for tidy data frames of draws from distributions
#'
#' Translates draws from distributions in a grouped data frame into a set of point and
#' interval summaries using a curve boxplot-inspired approach.
#'
#' Intervals are calculated by ranking the curves using some measure of *data depth*, then
#' using binary search to find a cutoff `k` such that an envelope containing the `k`% "deepest"
#' curves also contains `.width`% of the curves, for each value of `.width` (note that `k`
#' and `.width` are not necessarily the same). This is in contrast to most functional boxplot
#' or curve boxplot approaches, which tend to simply take the `.width`% deepest curves, and
#' are generally quite conservative (i.e. they may contain more than `.width`% of the curves).
#'
#' See Mirzargar *et al.* (2014) or Juul *et al.* (2020) for an accessible introduction
#' to data depth and curve boxplots / functional boxplots.
#'
#' @param .data Data frame (or grouped data frame as returned by [group_by()])
#' that contains draws to summarize.
#' @param ... Bare column names or expressions that, when evaluated in the context of
#' `.data`, represent draws to summarize. If this is empty, then by default all
#' columns that are not group columns and which are not in `.exclude` (by default
#' `".chain"`, `".iteration"`, `".draw"`, and `".row"`) will be summarized.
#' This can be list columns.
#' @param .along Which columns are the input values to the function describing the curve (e.g., the "x"
#' values). Supports tidyselect syntax, as in [dplyr::select()]. Intervals are calculated jointly with
#' respect to these variables, conditional on all other grouping variables in the data frame. The default
#' (`NULL`) causes [curve_interval()] to use all grouping variables in the input data frame as the value
#' for `.along`, which will generate the most conservative intervals. However, if you want to calculate
#' intervals for some function `y = f(x)` conditional on some other variable(s) (say, conditional on a
#' factor `g`), you would group by `g`, then use `.along = x` to calculate intervals jointly over `x`
#' conditional on `g`.
#' @param .width vector of probabilities to use that determine the widths of the resulting intervals.
#' If multiple probabilities are provided, multiple rows per group are generated, each with
#' a different probability interval (and value of the corresponding `.width` column).
#' @param .interval The method used to calculate the intervals. Currently, all methods rank the curves
#' using some measure of *data depth*, then create envelopes containing the `.width`% "deepest" curves.
#' Available methods are:
#'   - `"mhd"`: mean halfspace depth (Fraiman and Muniz 2001).
#'   - `"mbd"`: modified band depth (Sun and Genton 2011): calls [fda::fbplot()] with `method = "MBD"`.
#'   - `"bd"`: band depth (Sun and Genton 2011): calls [fda::fbplot()] with `method = "BD2"`.
#'   - `"bd-mbd"`: band depth, breaking ties with modified band depth (Sun and Genton 2011): calls
#'     [fda::fbplot()] with `method = "Both"`.
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
#' If `FALSE` (the default), the presence of `NA` values in the columns to be summarized will generally
#' result in an error. If `TRUE`, `NA` values will be removed in the calculation of intervals so long
#' as `.interval` is `"mhd"`; other methods do not currently support `na.rm`. Be cautious in applying
#' this parameter: in general, it is unclear what a joint interval should be when any of the values
#' are missing!
#' @return A data frame containing point summaries and intervals, with at least one column corresponding
#' to the point summary, one to the lower end of the interval, one to the upper end of the interval, the
#' width of the interval (`.width`), the type of point summary (`.point`), and the type of interval (`.interval`).
#' @author Matthew Kay
#' @encoding UTF-8
#' @references
#'
#' Fraiman, Ricardo and Graciela Muniz. (2001).
#' "Trimmed means for functional data".
#' *Test* 10: 419–440.
#' \doi{10.1007/BF02595706}.
#'
#' Sun, Ying and Marc G. Genton. (2011).
#' "Functional Boxplots".
#' *Journal of Computational and Graphical Statistics*, 20(2): 316-334.
#' \doi{10.1198/jcgs.2011.09224}
#'
#' Mirzargar, Mahsa, Ross T Whitaker, and Robert M Kirby. (2014).
#' "Curve Boxplot: Generalization of Boxplot for Ensembles of Curves".
#' *IEEE Transactions on Visualization and Computer Graphics*. 20(12): 2654-2663.
#' \doi{10.1109/TVCG.2014.2346455}
#'
#' Juul Jonas, Kaare Græsbøll, Lasse Engbo Christiansen, and Sune Lehmann. (2020).
#' "Fixed-time descriptive statistics underestimate extremes of epidemic curve ensembles".
#' *arXiv e-print*.
#' [arXiv:2007.05035](https://arxiv.org/abs/2007.05035)
#'
#' @seealso [point_interval()] for pointwise intervals. See `vignette("lineribbon")` for more examples
#' and discussion of the differences between pointwise and curvewise intervals.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' # generate a set of curves
#' k = 11 # number of curves
#' n = 201
#' df = tibble(
#'     .draw = rep(1:k, n),
#'     mean = rep(seq(-5,5, length.out = k), n),
#'     x = rep(seq(-15,15,length.out = n), each = k),
#'     y = dnorm(x, mean, 3)
#'   )
#'
#' # see pointwise intervals...
#' df %>%
#'   group_by(x) %>%
#'   median_qi(y, .width = c(.5)) %>%
#'   ggplot(aes(x = x, y = y)) +
#'   geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
#'   geom_line(aes(group = .draw), alpha=0.15, data = df) +
#'   scale_fill_brewer() +
#'   ggtitle("50% pointwise intervals with point_interval()") +
#'   theme_ggdist()
#'
#' @examplesIf requireNamespace("posterior", quietly = TRUE)
#' # ... compare them to curvewise intervals
#' df %>%
#'   group_by(x) %>%
#'   curve_interval(y, .width = c(.5)) %>%
#'   ggplot(aes(x = x, y = y)) +
#'   geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
#'   geom_line(aes(group = .draw), alpha=0.15, data = df) +
#'   scale_fill_brewer() +
#'   ggtitle("50% curvewise intervals with curve_interval()") +
#'   theme_ggdist()
#'
#' @importFrom dplyr group_vars summarise_at %>% group_split
#' @importFrom rlang quos quos_auto_name eval_tidy quo_get_expr syms enquo
#' @importFrom tidyselect eval_select
#' @export
curve_interval = function(.data, ..., .along = NULL, .width = .5,
  .interval = c("mhd", "mbd", "bd", "bd-mbd"), .simple_names = TRUE,
  na.rm = FALSE, .exclude = c(".chain", ".iteration", ".draw", ".row")
) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    stop0('curve_interval() requires the `posterior` package to be installed.') #nocov
  }

  .interval = match.arg(.interval)
  data = .data    # to avoid conflicts with tidy eval's `.data` pronoun
  col_exprs = quos(..., .named = TRUE)

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
      syms() %>%
      quos_auto_name()

    if (length(col_exprs) == 0) {
      #still nothing to aggregate? not sure what the user wants
      stop0("No columns found to calculate point and interval summaries for.")
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

    .curve_interval(data, col_name, ".lower", ".upper", .width, .interval, .conditional_groups, na.rm = na.rm)
  } else {
    for (i in seq_along(col_exprs)) {
      data[[names(col_exprs)[[i]]]] = eval_tidy(col_exprs[[i]], data)
    }

    # if the values we are going to summarise are not already list columns, make them into list columns
    # (making them list columns first is faster than anything else I've tried)
    if (!all(map_lgl_(data[,names(col_exprs)], is.list))) {
      data = summarise_at(data, names(col_exprs), list)
    }

    result = NULL
    actual_widths = NULL
    for (col_name in names(col_exprs)) {
      col_result = .curve_interval(
        data, col_name, paste0(col_name, ".lower"), paste0(col_name, ".upper"), .width, .interval, .conditional_groups, na.rm = na.rm
      )

      # actual widths aren't always going to be equal so we'll take the means of them
      actual_widths = cbind(actual_widths, col_result$.actual_width)

      result = bind_cols(
        result[, names(result) != col_name],
        col_result[, names(col_result) == col_name | (!names(col_result) %in% names(result))]
      )
    }
    result$.actual_width = rowMeans(actual_widths)

    result
  }

  result[[".point"]] = .interval
  result[[".interval"]] = .interval

  result
}

halfspace_depth = function(x) {
  rank_x = rank(x, na.last = "keep")
  n = length(x)
  pmin(rank_x/n, (n + 1 - rank_x)/n)
}

.curve_interval = function(data, col_name, lower, upper, .width, .interval, .conditional_groups, na.rm = FALSE) {
  if (length(unique(lengths(data[[col_name]]))) != 1) {
    stop0("Must have the same number of values in each group.")
  }

  dfs = group_split(group_by_at(data, .conditional_groups))

  # translate our names to names fda::fbplot understands
  .interval_internal = switch(.interval,
    mbd = "MBD",
    bd = "BD2",
    `bd-mbd` = "Both",
    "mhd"
  )

  map_dfr_(dfs, function(d) {

    # draws x y matrix
    draws = do.call(cbind, d[[col_name]])
    y_rvar = posterior::rvar(draws)

    if (.interval_internal == "mhd") { #mean halfspace depth
      # draws x depth matrix
      pointwise_depths = apply(draws, 2, halfspace_depth)
      # mean depth of each draw
      draw_depth = rowMeans(pointwise_depths, na.rm = na.rm)
    } else { # band depth using fbplot
      if (!requireNamespace("fda", quietly = TRUE)) {
        stop0(                                                                           # nocov
          'curve_interval(.interval = "', .interval, '") requires the `fda` package.\n', # nocov
          'Please install the `fda` package or use .interval = "mhd".'                   # nocov
        )                                                                                # nocov
      }
      # depth of each draw
      draw_depth = fda::fbplot(t(draws), plot = FALSE, method = .interval_internal)$depth
    }

    # median draw = the one with the maximum depth
    median_draw = which.max(draw_depth)
    median_y = map_dbl_(d[[col_name]], `[[`, median_draw)

    # function for determining the intervals given a selected draw depth
    calc_intervals_at_depth_cutoff = function(depth_cutoff) {
      selected_draws = draw_depth >= depth_cutoff

      selected_y = lapply(d[[col_name]], `[`, selected_draws)
      d[[lower]] = map_dbl_(selected_y, min)
      d[[upper]] = map_dbl_(selected_y, max)
      d[[".actual_width"]] = posterior::Pr(posterior::rvar_all(d[[lower]] <= y_rvar & y_rvar <= d[[upper]]))

      d
    }

    # for each requested width (as a probability), translate it into a
    # draw depth cutoff where approximately width% draws are contained
    # by the envelope around all draws deeper than the depth cutoff
    sorted_draw_depths = sort(draw_depth)
    map_dfr_(.width, function(w) {
      # The naive approach would be to just use quantiles of draw depths to determine
      # the cutoff; something like:
      #   depth_cutoff = quantile(draw_depth, 1 - w, na.rm = na.rm)
      # However this does not work well, since the envelope around a w% set of curves
      # determined via quantiles tends to incidentally cover some other curves, making
      # the coverage be (sometimes substantially) more than w%.
      # Thus instead, we use binary search to find a depth cutoff that contains
      # w% of the curves:
      draw_depth_i = binary_search(
        function(i) {
          depth_cutoff = sorted_draw_depths[i]
          actual_width = calc_intervals_at_depth_cutoff(depth_cutoff)[[".actual_width"]][[1]]
          actual_width >= w
        },
        min_i = 1,
        max_i = length(sorted_draw_depths)
      )
      depth_cutoff = sorted_draw_depths[draw_depth_i]

      d = calc_intervals_at_depth_cutoff(depth_cutoff)
      d[[col_name]] = median_y
      d[[".width"]] = w
      d
    })
  })
}


# use binary search to find the largest i such that f(i) is TRUE
# assumes there is some i such that for all j < i, f(i) is TRUE and
# for all k > i, f(i) is FALSE
binary_search = function(f, min_i, max_i) {
  # pre-conditions: f(min_i) should be TRUE, f(max_i) should be FALSE
  if (!f(min_i)) return(min_i)
  if (f(max_i)) return(max_i)

  repeat {
    if (max_i - min_i == 1) return(min_i)

    i = ceiling((min_i + max_i) / 2)
    if (f(i)) {
      min_i = i
    } else {
      max_i = i
    }
  }
}
