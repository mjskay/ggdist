# [point]_[interval] functions for use with tidy data
#
# Author: mjskay
###############################################################################

# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("y", "ymin", "ymax"))


#' Point and interval summaries for tidy data frames of draws from distributions
#'
#' Translates draws from distributions in a (possibly grouped) data frame into point and
#' interval summaries (or set of point and interval summaries, if there are
#' multiple groups in a grouped data frame).
#'
#' If \code{.data} is a data frame, then \code{...} is a list of bare names of
#' columns (or expressions derived from columns) of \code{.data}, on which
#' the point and interval summaries are derived. Column expressions are processed
#' using the tidy evaluation framework (see \code{\link[rlang]{eval_tidy}}).
#'
#' For a column named \code{x}, the resulting data frame will have a column
#' named \code{x} containing its point summary. If there is a single
#' column to be summarized and \code{.broom} is \code{TRUE}, the output will
#' also contain columns \code{conf.low} (the lower end of the interval),
#' \code{conf.high} (the upper end of the interval).
#' Otherwise, for every summarized column \code{x}, the output will contain
#' \code{x.low} (the lower end of the interval) and \code{x.high} (the upper
#' end of the interval). Finally, the output will have a \code{.prob} column
#' containing the' probability for the interval on each output row.
#'
#' If \code{.data} includes groups (see e.g. \code{\link[dplyr]{group_by}}),
#' the points and intervals are calculated within the groups.
#'
#' If \code{.data} is a vector, \code{...} is ignored and the result is a
#' data frame with one row per value of \code{.prob} and three columns:
#' \code{y} (the point summary), \code{ymin} (the lower end of the interval),
#' \code{ymax} (the upper end of the interval), and \code{.prob}, the probability
#' corresponding to the interval. This behavior allows \code{point_interval}
#' and its derived functions (like \code{median_qi}, \code{mean_qi}, \code{mode_hdi}, etc)
#' to be easily used to plot intervals in ggplot using methods like
#' \code{\link{geom_eye}}, \code{\link{geom_eyeh}}, or \code{\link{stat_summary}}.
#'
#' The functions ending in \code{h} (e.g., \code{point_intervalh}, \code{median_qih})
#' behave identically to the function without the h, except that when passed a vector,
#' they return a data frame with \code{x}/\code{xmin}/\code{xmax} instead of
#' \code{y}/\code{ymin}/\code{ymax}. This allows them to be used as values of the
#' \code{fun.data = } argument of \code{stat_summaryh}. \strong{Note:} these
#' functions are not necessary if you use the \code{point_interval}
#' argument of \code{stat}s and \code{geom}s in the \code{tidybayes} package (e.g.
#' \code{\link{stat_pointintervalh}}, \code{\link{geom_halfeyeh}}, etc), as
#' these automatically adjust the function output to match their required aesthetics.
#'
#' \code{median_qi}, \code{mode_hdi}, etc are short forms for
#' \code{point_interval(..., .point = median, .interval = qi)}, etc.
#'
#' \code{qi} yields the quantile interval (also known as the percentile interval or
#' equi-tailed interval) as a 1x2 matrix.
#'
#' \code{hdi} yields the highest-density interval(s) (also known as the highest posterior
#' density interval). \emph{Note:} If the distribution is multimodal, \code{hdi} may return multiple
#' intervals for each probability level (these will be spread over rows). Internally it uses \code{\link[HDInterval]{hdi}}.
#'
#' @param .data Data frame (or grouped data frame as returned by \code{\link{group_by}})
#' that contains draws to summarize.
#' @param ... Bare column names or expressions that, when evaluated in the context of
#' \code{.data}, represent draws to summarise. If this is empty, then by default all
#' columns that are not group columns and which are not in \code{.exclude} (by default
#' \code{".chain"}, \code{".iteration"}, \code{".draw"}, and \code{".row"}) will be summarised.
#' This can be list columns.
#' @param .prob vector of probabilities to use for generating intervals. If multiple
#' probabilities are provided, multiple rows per group are generated, each with
#' a different probabilty interval (and value of the corresponding \code{.prob} column).
#' @param .point Point summary function, which takes a vector and returns a single
#' value, e.g. \code{\link{mean}}, \code{\link{median}}, or \code{\link{Mode}}.
#' @param .interval Interval function, which takes a vector and a probability
#' (\code{.prob}) and returns a two-element vector representing the lower and upper
#' bound of an interval; e.g. \code{\link{qi}}, \code{\link{hdi}}
#' @param .broom When \code{TRUE} and only a single column / vector is to be summarised, use the
#' name \code{conf.low} for the lower end of the interval and \code{conf.high} for the
#' upper end for consistency with \code{\link[broom]{tidy}} in the broom package. If
#' \code{.data} is a vector and this is \code{TRUE}, this will also set the column name
#' of the point summary to \code{estimate}.
#' @param .exclude A character vector of names of columns to be excluded from summarisation
#' if no column names are specified to be summarised. Default ignores several meta-data column
#' names used in tidybayes.
#' @param x vector to summarise (for interval functions: \code{qi} and \code{hdi})
#' @author Matthew Kay
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' set.seed(123)
#'
#' rnorm(1000) %>%
#'   median_qi()
#'
#' data.frame(x = rnorm(1000)) %>%
#'   median_qi(x, .prob = c(.50, .80, .95))
#'
#' data.frame(
#'     x = rnorm(1000),
#'     y = rnorm(1000, mean = 2, sd = 2)
#'   ) %>%
#'   median_qi(x, y)
#'
#' data.frame(
#'     x = rnorm(1000),
#'     group = "a"
#'   ) %>%
#'   rbind(data.frame(
#'     x = rnorm(1000, mean = 2, sd = 2),
#'     group = "b")
#'   ) %>%
#'   group_by(group) %>%
#'   median_qi(.prob = c(.50, .80, .95))
#'
#' multimodal_draws = data.frame(
#'     x = c(rnorm(5000, 0, 1), rnorm(2500, 4, 1))
#'   )
#'
#' multimodal_draws %>%
#'   mode_hdi(.prob = c(.66, .95))
#'
#' multimodal_draws %>%
#'   ggplot(aes(x = x, y = 0)) +
#'   geom_halfeyeh(fun.data = mode_hdih, .prob = c(.66, .95))
#'
#' @importFrom purrr map_dfr map map2 discard map_dbl map_lgl iwalk
#' @importFrom dplyr do bind_cols group_vars
#' @importFrom stringi stri_startswith_fixed
#' @importFrom rlang set_names quos quos_auto_name eval_tidy as_quosure
#' @export
point_interval = function(.data, ..., .prob=.95, .point = median, .interval = qi, .broom = TRUE,
  .exclude = c(".chain", ".iteration", ".draw", ".row")
) {
  UseMethod("point_interval")
}

#' @rdname point_interval
#' @export
point_interval.default = function(.data, ..., .prob=.95, .point = median, .interval = qi, .broom = TRUE,
  .exclude = c(".chain", ".iteration", ".draw", ".row")
) {
  data = .data    # to avoid conflicts with tidy eval's `.data` pronoun
  col_exprs = quos(..., .named = TRUE)

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

  if (length(col_exprs) == 1 && .broom) {
    # only one column provided => summarise that column and use "conf.low" and "conf.high" as
    # the generated column names for consistency with tidy() in broom
    col_expr = col_exprs[[1]]
    col_name = names(col_exprs)

    # evaluate the expression that will result in the draws we want to summarise
    data[[col_name]] = eval_tidy(col_expr, data)

    # if the value we are going to summarise is not already a list column, make it into a list column
    # (making it a list column first is faster than anything else I've tried)
    if (is.list(data[[col_name]])) {
      draws = data[[col_name]]
    } else {
      data = summarise_at(data, col_name, list)
      draws = data[[col_name]]
    }

    map_dfr(.prob, function(p) {
      data[[col_name]] = map_dbl(draws, .point)

      intervals = map(draws, .interval, .prob = p)
      # can't use map_dbl here because sometimes (e.g. with hdi) these can
      # return multiple intervals, hence map() here and unnest() below
      data[["conf.low"]] = map(intervals, ~ .[, 1])
      data[["conf.high"]] = map(intervals, ~ .[, 2])
      data = unnest(data, conf.low, conf.high)

      data[[".prob"]] = p

      data
    })
  } else {
    iwalk(col_exprs, function(col_expr, col_name) {
      data[[col_name]] <<- eval_tidy(col_expr, data)
    })

    # if the values we are going to summarise are not already list columns, make them into list columns
    # (making them list columns first is faster than anything else I've tried)
    if (!all(map_lgl(data[,names(col_exprs)], is.list))) {
      data = summarise_at(data, names(col_exprs), list)
    }

    map_dfr(.prob, function(p) {
      for (col_name in names(col_exprs)) {
        draws = data[[col_name]]
        data[[col_name]] = NULL  # to move the column to the end so that the column is beside its interval columns

        data[[col_name]] = map_dbl(draws, .point)

        intervals = map(draws, .interval, .prob = p)

        # can't use map_dbl here because sometimes (e.g. with hdi) these can
        # return multiple intervals, which we need to check for (since it is
        # not possible to support in this format).
        lower = map(intervals, ~ .[, 1])
        upper = map(intervals, ~ .[, 2])
        if (any(map_dbl(lower, length) > 1) || any(map_dbl(upper, length) > 1)) {
          stop(
            "You are summarizing a multimodal distribution using a method that returns multiple intervals",
            "(such as `hdi`), but you are attempting to generate intervals for multiple columns in wide format.",
            "To use a multiple-interval method like `hdi` on distributions that are multi-modal, you can",
            "only summarize one column at a time. You might try using `gather_variables` to put all your draws",
            "into a single column before summarizing them, or use an interval type (such as `qi`) that always",
            "returns exactly one interval per probability level."
          )
        }
        data[[paste0(col_name, ".low")]] = unlist(lower)
        data[[paste0(col_name, ".high")]] = unlist(upper)
      }

      data[[".prob"]] = p

      data
    })
  }
}

#' @rdname point_interval
#' @importFrom dplyr rename
#' @export
point_interval.numeric = function(.data, ..., .prob = .95, .point = median, .interval = qi, .broom = FALSE,
  .exclude = c(".chain", ".iteration", ".draw", ".row")
) {
  data = .data    # to avoid conflicts with tidy eval's `.data` pronoun

  result = map_dfr(.prob, function(p) {
    interval = .interval(data, .prob = p)
    data.frame(
      y = .point(data),
      ymin = interval[, 1],
      ymax = interval[, 2],
      .prob = p
    )
  })

  if (.broom) {
    result %>%
      rename(estimate = y, conf.low = ymin, conf.high = ymax)
  }
  else {
    result
  }
}

#' @rdname point_interval
#' @export
point_intervalh = flip_aes(point_interval)

#' @importFrom stats quantile
#' @export
#' @rdname point_interval
qi = function(x, .prob = .95) {
  lower_prob = (1 - .prob) / 2
  upper_prob = (1 + .prob) / 2
  matrix(quantile(x, c(lower_prob, upper_prob)), ncol = 2)
}

#' @importFrom coda HPDinterval as.mcmc
#' @export
#' @rdname point_interval
hdi = function(x, .prob = .95) {
  intervals = HDInterval::hdi(density(x), credMass = .prob, allowSplit = TRUE)
  if (nrow(intervals) == 1) {
    # the above method tends to be a little conservative on unimodal distributions, so if the
    # result is unimodal, switch to the method below (which will be slightly narrower)
    intervals = HDInterval::hdi(x, credMass = .prob)
  }
  matrix(intervals, ncol = 2)
}

#' @export
#' @rdname point_interval
mean_qi = function(.data, ..., .prob = .95)
  point_interval(.data, ..., .prob = .prob, .point = mean, .interval = qi)

#' @export
#' @rdname point_interval
mean_qih = flip_aes(mean_qi)

#' @export
#' @rdname point_interval
median_qi = function(.data, ..., .prob = .95)
  point_interval(.data, ..., .prob = .prob, .point = median, .interval = qi)

#' @export
#' @rdname point_interval
median_qih = flip_aes(median_qi)

#' @importFrom LaplacesDemon Mode
#' @export
#' @rdname point_interval
mode_qi = function(.data, ..., .prob = .95)
  point_interval(.data, ..., .prob = .prob, .point = Mode, .interval = qi)

#' @export
#' @rdname point_interval
mode_qih = flip_aes(mode_qi)

#' @export
#' @rdname point_interval
mean_hdi = function(.data, ..., .prob = .95)
  point_interval(.data, ..., .prob = .prob, .point = mean, .interval = hdi)

#' @export
#' @rdname point_interval
mean_hdih = flip_aes(mean_hdi)

#' @export
#' @rdname point_interval
median_hdi = function(.data, ..., .prob = .95)
  point_interval(.data, ..., .prob = .prob, .point = median, .interval = hdi)

#' @export
#' @rdname point_interval
median_hdih = flip_aes(median_hdi)

#' @importFrom LaplacesDemon Mode
#' @export
#' @rdname point_interval
mode_hdi = function(.data, ..., .prob = .95)
  point_interval(.data, ..., .prob = .prob, .point = Mode, .interval = hdi)

#' @export
#' @rdname point_interval
mode_hdih = flip_aes(mode_hdi)
