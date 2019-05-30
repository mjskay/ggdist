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
#' column to be summarized and \code{.simple_names} is \code{TRUE}, the output will
#' also contain columns \code{.lower} (the lower end of the interval),
#' \code{.upper} (the upper end of the interval).
#' Otherwise, for every summarized column \code{x}, the output will contain
#' \code{x.lower} (the lower end of the interval) and \code{x.upper} (the upper
#' end of the interval). Finally, the output will have a \code{.width} column
#' containing the' probability for the interval on each output row.
#'
#' If \code{.data} includes groups (see e.g. \code{\link[dplyr]{group_by}}),
#' the points and intervals are calculated within the groups.
#'
#' If \code{.data} is a vector, \code{...} is ignored and the result is a
#' data frame with one row per value of \code{.width} and three columns:
#' \code{y} (the point summary), \code{ymin} (the lower end of the interval),
#' \code{ymax} (the upper end of the interval), and \code{.width}, the probability
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
#' density interval). \strong{Note:} If the distribution is multimodal, \code{hdi} may return multiple
#' intervals for each probability level (these will be spread over rows). You may wish to use
#' \code{hdci} (below) instead if you want a single highest-density interval, with the caveat that when
#' the distribution is multimodal \code{hdci} is not a highest-density interval. Internally \code{hdi} uses
#' \code{\link[HDInterval]{hdi}} with \code{allowSplit = TRUE} (when multimodal) and with
#' \code{allowSplit = FALSE} (when not multimodal).
#'
#' \code{hdci} yields the highest-density \emph{continuous} interval. \strong{Note:} If the distribution
#' is multimodal, this may not actually be the highest-density interval (there may be a higher-density
#' discontinuous interval). Internally \code{hdci} uses
#' \code{\link[HDInterval]{hdi}} with \code{allowSplit = FALSE}; see that function for more
#' information on multimodality and continuous versus discontinuous intervals.
#'
#' @param .data Data frame (or grouped data frame as returned by \code{\link{group_by}})
#' that contains draws to summarize.
#' @param ... Bare column names or expressions that, when evaluated in the context of
#' \code{.data}, represent draws to summarize. If this is empty, then by default all
#' columns that are not group columns and which are not in \code{.exclude} (by default
#' \code{".chain"}, \code{".iteration"}, \code{".draw"}, and \code{".row"}) will be summarized.
#' This can be list columns.
#' @param .width vector of probabilities to use that determine the widths of the resulting intervals.
#' If multiple probabilities are provided, multiple rows per group are generated, each with
#' a different probability interval (and value of the corresponding \code{.width} column).
#' @param .prob Deprecated. Use \code{.width} instead.
#' @param .point Point summary function, which takes a vector and returns a single
#' value, e.g. \code{\link{mean}}, \code{\link{median}}, or \code{\link{Mode}}.
#' @param .interval Interval function, which takes a vector and a probability
#' (\code{.width}) and returns a two-element vector representing the lower and upper
#' bound of an interval; e.g. \code{\link{qi}}, \code{\link{hdi}}
#' @param .simple_names When \code{TRUE} and only a single column / vector is to be summarized, use the
#' name \code{.lower} for the lower end of the interval and \code{.upper} for the
#' upper end. If \code{.data} is a vector and this is \code{TRUE}, this will also set the column name
#' of the point summary to \code{.value}. When \code{FALSE} and \code{.data} is a data frame,
#' names the lower and upper intervals for each column \code{x} \code{x.lower} and \code{x.upper}.
#' When \code{FALSE} and \code{.data} is a vector, uses the naming scheme \code{y}, \code{ymin}
#' and \code{ymax} (for use with ggplot).
#' @param .exclude A character vector of names of columns to be excluded from summarization
#' if no column names are specified to be summarized. Default ignores several meta-data column
#' names used in tidybayes.
#' @param na.rm logical value indicating whether \code{NA} values should be stripped before the computation proceeds.
#' If \code{FALSE} (the default), any vectors to be summarised that contain \code{NA} will result in
#' point and interval summaries equal to \code{NA}.
#' @param x vector to summarize (for interval functions: \code{qi} and \code{hdi})
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
#'   median_qi(x, .width = c(.50, .80, .95))
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
#'   median_qi(.width = c(.50, .80, .95))
#'
#' multimodal_draws = data.frame(
#'     x = c(rnorm(5000, 0, 1), rnorm(2500, 4, 1))
#'   )
#'
#' multimodal_draws %>%
#'   mode_hdi(.width = c(.66, .95))
#'
#' multimodal_draws %>%
#'   ggplot(aes(x = x, y = 0)) +
#'   geom_halfeyeh(fun.data = mode_hdih, .width = c(.66, .95))
#'
#' @importFrom purrr map_dfr map map2 discard map_dbl map_lgl iwalk
#' @importFrom dplyr do bind_cols group_vars summarise_at
#' @importFrom tidyr unnest
#' @importFrom rlang set_names quos quos_auto_name eval_tidy as_quosure
#' @export
point_interval = function(.data, ..., .width = .95, .point = median, .interval = qi, .simple_names = TRUE,
  na.rm = FALSE, .exclude = c(".chain", ".iteration", ".draw", ".row"), .prob
) {
  UseMethod("point_interval")
}

#' @rdname point_interval
#' @export
point_interval.default = function(.data, ..., .width = .95, .point = median, .interval = qi, .simple_names = TRUE,
  na.rm = FALSE, .exclude = c(".chain", ".iteration", ".draw", ".row"), .prob
) {
  .width = .Deprecated_argument_alias(.width, .prob)
  data = .data    # to avoid conflicts with tidy eval's `.data` pronoun
  col_exprs = quos(..., .named = TRUE)
  point_name = tolower(quo_name(enquo(.point)))
  interval_name = tolower(quo_name(enquo(.interval)))

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

  if (length(col_exprs) == 1 && .simple_names) {
    # only one column provided => summarise that column and use ".lower" and ".upper" as
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

    result = map_dfr(.width, function(p) {
      data[[col_name]] = map_dbl(draws, .point, na.rm = na.rm)

      intervals = map(draws, .interval, .width = p, na.rm = na.rm)
      # can't use map_dbl here because sometimes (e.g. with hdi) these can
      # return multiple intervals, hence map() here and unnest() below
      data[[".lower"]] = map(intervals, ~ .[, 1])
      data[[".upper"]] = map(intervals, ~ .[, 2])
      data = unnest(data, .lower, .upper)

      data[[".width"]] = p

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

    result = map_dfr(.width, function(p) {
      for (col_name in names(col_exprs)) {
        draws = data[[col_name]]
        data[[col_name]] = NULL  # to move the column to the end so that the column is beside its interval columns

        data[[col_name]] = map_dbl(draws, .point, na.rm = na.rm)

        intervals = map(draws, .interval, .width = p, na.rm = na.rm)

        # can't use map_dbl here because sometimes (e.g. with hdi) these can
        # return multiple intervals, which we need to check for (since it is
        # not possible to support in this format).
        lower = map(intervals, ~ .[, 1])
        upper = map(intervals, ~ .[, 2])
        if (any(map_dbl(lower, length) > 1) || any(map_dbl(upper, length) > 1)) {
          stop(
            "You are summarizing a multimodal distribution using a method that returns multiple intervals ",
            "(such as `hdi`), but you are attempting to generate intervals for multiple columns in wide format. ",
            "To use a multiple-interval method like `hdi` on distributions that are multi-modal, you can ",
            "only summarize one column at a time. You might try using `gather_variables` to put all your draws ",
            "into a single column before summarizing them, or use an interval type (such as `hdci` or `qi`) that ",
            "always returns exactly one interval per probability level."
          )
        }
        data[[paste0(col_name, ".lower")]] = unlist(lower)
        data[[paste0(col_name, ".upper")]] = unlist(upper)
      }

      data[[".width"]] = p

      data
    })
  }

  result[[".point"]] = point_name
  result[[".interval"]] = interval_name

  result
}

#' @rdname point_interval
#' @importFrom dplyr rename
#' @export
point_interval.numeric = function(.data, ..., .width = .95, .point = median, .interval = qi, .simple_names = FALSE,
  na.rm = FALSE, .exclude = c(".chain", ".iteration", ".draw", ".row"), .prob
) {
  .width = .Deprecated_argument_alias(.width, .prob)
  data = .data    # to avoid conflicts with tidy eval's `.data` pronoun
  point_name = tolower(quo_name(enquo(.point)))
  interval_name = tolower(quo_name(enquo(.interval)))

  result = map_dfr(.width, function(p) {
    interval = .interval(data, .width = p, na.rm = na.rm)
    data.frame(
      y = .point(data, na.rm = na.rm),
      ymin = interval[, 1],
      ymax = interval[, 2],
      .width = p
    )
  })

  result[[".point"]] = point_name
  result[[".interval"]] = interval_name

  if (.simple_names) {
    result %>%
      rename(.value = y, .lower = ymin, .upper = ymax)
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
qi = function(x, .width = .95, .prob, na.rm = FALSE) {
  .width = .Deprecated_argument_alias(.width, .prob)
  if (!na.rm && any(is.na(x))) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }

  lower_prob = (1 - .width) / 2
  upper_prob = (1 + .width) / 2
  matrix(quantile(x, c(lower_prob, upper_prob), na.rm = na.rm), ncol = 2)
}

#' @importFrom coda HPDinterval as.mcmc
#' @export
#' @rdname point_interval
hdi = function(x, .width = .95, .prob, na.rm = FALSE) {
  .width = .Deprecated_argument_alias(.width, .prob)
  if (!na.rm && any(is.na(x))) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }

  intervals = HDInterval::hdi(density(x, na.rm = na.rm), credMass = .width, allowSplit = TRUE)
  if (nrow(intervals) == 1) {
    # the above method tends to be a little conservative on unimodal distributions, so if the
    # result is unimodal, switch to the method below (which will be slightly narrower)
    intervals = HDInterval::hdi(x, credMass = .width)
  }
  matrix(intervals, ncol = 2)
}

#' @export
#' @rdname point_interval
#' @importFrom rlang is_integerish
Mode = function(x, na.rm = FALSE) {
  if (na.rm) {
    x = x[!is.na(x)]
  }
  else if (any(is.na(x))) {
    return(NA_real_)
  }

  if (is_integerish(x)) {
    # for the discrete case, based on https://stackoverflow.com/a/8189441
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  } else {
    # for the continuous case
    d = density(x)
    d$x[which.max(d$y)]
  }
}

#' @export
#' @rdname point_interval
hdci = function(x, .width = .95, na.rm = FALSE) {
  if (!na.rm && any(is.na(x))) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }

  intervals = HDInterval::hdi(x, credMass = .width)
  matrix(intervals, ncol = 2)
}

#' @export
#' @rdname point_interval
mean_qi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = mean, .interval = qi)

#' @export
#' @rdname point_interval
mean_qih = flip_aes(mean_qi)

#' @export
#' @rdname point_interval
median_qi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = median, .interval = qi)

#' @export
#' @rdname point_interval
median_qih = flip_aes(median_qi)

#' @export
#' @rdname point_interval
mode_qi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = Mode, .interval = qi)

#' @export
#' @rdname point_interval
mode_qih = flip_aes(mode_qi)

#' @export
#' @rdname point_interval
mean_hdi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = mean, .interval = hdi)

#' @export
#' @rdname point_interval
mean_hdih = flip_aes(mean_hdi)

#' @export
#' @rdname point_interval
median_hdi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = median, .interval = hdi)

#' @export
#' @rdname point_interval
median_hdih = flip_aes(median_hdi)

#' @export
#' @rdname point_interval
mode_hdi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = Mode, .interval = hdi)

#' @export
#' @rdname point_interval
mode_hdih = flip_aes(mode_hdi)

#' @export
#' @rdname point_interval
mean_hdci = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = mean, .interval = hdci)

#' @export
#' @rdname point_interval
mean_hdcih = flip_aes(mean_hdci)

#' @export
#' @rdname point_interval
median_hdci = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = median, .interval = hdci)

#' @export
#' @rdname point_interval
median_hdcih = flip_aes(median_hdci)

#' @export
#' @rdname point_interval
mode_hdci = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = Mode, .interval = hdci)

#' @export
#' @rdname point_interval
mode_hdcih = flip_aes(mode_hdci)
