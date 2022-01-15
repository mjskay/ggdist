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
#' If `.data` is a data frame, then `...` is a list of bare names of
#' columns (or expressions derived from columns) of `.data`, on which
#' the point and interval summaries are derived. Column expressions are processed
#' using the tidy evaluation framework (see [rlang::eval_tidy()]).
#'
#' For a column named `x`, the resulting data frame will have a column
#' named `x` containing its point summary. If there is a single
#' column to be summarized and `.simple_names` is `TRUE`, the output will
#' also contain columns `.lower` (the lower end of the interval),
#' `.upper` (the upper end of the interval).
#' Otherwise, for every summarized column `x`, the output will contain
#' `x.lower` (the lower end of the interval) and `x.upper` (the upper
#' end of the interval). Finally, the output will have a `.width` column
#' containing the' probability for the interval on each output row.
#'
#' If `.data` includes groups (see e.g. [dplyr::group_by()]),
#' the points and intervals are calculated within the groups.
#'
#' If `.data` is a vector, `...` is ignored and the result is a
#' data frame with one row per value of `.width` and three columns:
#' `y` (the point summary), `ymin` (the lower end of the interval),
#' `ymax` (the upper end of the interval), and `.width`, the probability
#' corresponding to the interval. This behavior allows `point_interval`
#' and its derived functions (like `median_qi`, `mean_qi`, `mode_hdi`, etc)
#' to be easily used to plot intervals in ggplot stats using methods like
#' [stat_eye()], [stat_halfeye()], or [stat_summary()].
#'
#' `median_qi`, `mode_hdi`, etc are short forms for
#' `point_interval(..., .point = median, .interval = qi)`, etc.
#'
#' `qi` yields the quantile interval (also known as the percentile interval or
#' equi-tailed interval) as a 1x2 matrix.
#'
#' `hdi` yields the highest-density interval(s) (also known as the highest posterior
#' density interval). **Note:** If the distribution is multimodal, `hdi` may return multiple
#' intervals for each probability level (these will be spread over rows). You may wish to use
#' `hdci` (below) instead if you want a single highest-density interval, with the caveat that when
#' the distribution is multimodal `hdci` is not a highest-density interval. Internally `hdi` uses
#' [HDInterval::hdi()] with `allowSplit = TRUE` (when multimodal) and with
#' `allowSplit = FALSE` (when not multimodal).
#'
#' `hdci` yields the highest-density *continuous* interval. **Note:** If the distribution
#' is multimodal, this may not actually be the highest-density interval (there may be a higher-density
#' discontinuous interval). Internally `hdci` uses
#' [HDInterval::hdi()] with `allowSplit = FALSE`; see that function for more
#' information on multimodality and continuous versus discontinuous intervals.
#'
#' `ll` and `ul` yield lower limits and upper limits, respectively (where the opposite
#' limit is set to either `Inf` or `-Inf`).
#'
#' @param .data Data frame (or grouped data frame as returned by [group_by()])
#' that contains draws to summarize.
#' @param ... Bare column names or expressions that, when evaluated in the context of
#' `.data`, represent draws to summarize. If this is empty, then by default all
#' columns that are not group columns and which are not in `.exclude` (by default
#' `".chain"`, `".iteration"`, `".draw"`, and `".row"`) will be summarized.
#' These columns can be numeric, \pkg{distributional} objects, `posterior::rvar`s,
#' or list columns of numeric values to summarise.
#' @param .width vector of probabilities to use that determine the widths of the resulting intervals.
#' If multiple probabilities are provided, multiple rows per group are generated, each with
#' a different probability interval (and value of the corresponding `.width` column).
#' @param .prob Deprecated. Use `.width` instead.
#' @param .point Point summary function, which takes a vector and returns a single
#' value, e.g. [mean()], [median()], or [Mode()].
#' @param .interval Interval function, which takes a vector and a probability
#' (`.width`) and returns a two-element vector representing the lower and upper
#' bound of an interval; e.g. [qi()], [hdi()]
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
#'   stat_halfeye(point_interval = mode_hdi, .width = c(.66, .95))
#'
#' @importFrom dplyr do bind_cols group_vars summarise_at %>%
#' @importFrom rlang quos quos_auto_name eval_tidy syms
#' @importFrom stats median
#' @importFrom tibble as_tibble
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
      syms() %>%
      quos_auto_name()

    if (length(col_exprs) == 0) {
      #still nothing to aggregate? not sure what the user wants
      stop0("No columns found to calculate point and interval summaries for.")
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

    result = map_dfr_(.width, function(p) {
      # compute intervals; this is robust to grouped data frames and
      # to intervals that can return multiple intervals (e.g., hdi())

      # reduce `data` to just point estimate column and grouping factors (if any)
      data[[col_name]] = map_dbl_(draws, .point, na.rm = na.rm)

      # for each row of `data`, compute the intervals (may be more than one),
      # and construct a tibble with grouping factors (if any), point estimate,
      # lower and upper values, and width
      # - equivalent to unnest_legacy()
      data = map2_dfr_(seq_len(nrow(data)), draws, function(row_i, draws_i) {
        interval = .interval(draws_i, .width = p, na.rm = na.rm) # intervals (one or more rows)
        dimnames(interval)[[2]] = c(".lower", ".upper")
        cbind(
          data[row_i, , drop = FALSE], # each row of `data`; grouping factors and point estimate
          interval,
          .width = p # width
        )
      })

      as_tibble(data)
    })
  } else {
    for (i in seq_along(col_exprs)) {
      data[[names(col_exprs)[[i]]]] = eval_tidy(col_exprs[[i]], data)
    }

    # if the values we are going to summarise are not already list columns, make them into list columns
    # (making them list columns first is faster than anything else I've tried)
    # this also ensures that rvars and distributional objects are supported (as those act as lists)
    if (!all(map_lgl_(data[,names(col_exprs)], is.list))) {
      data = summarise_at(data, names(col_exprs), list)
    }

    result = map_dfr_(.width, function(p) {
      for (col_name in names(col_exprs)) {
        draws = data[[col_name]]
        data[[col_name]] = NULL  # to move the column to the end so that the column is beside its interval columns

        data[[col_name]] = map_dbl_(draws, .point, na.rm = na.rm)

        intervals = lapply(draws, .interval, .width = p, na.rm = na.rm)

        # can't use map_dbl_ here because sometimes (e.g. with hdi) these can
        # return multiple intervals, which we need to check for (since it is
        # not possible to support in this format).
        lower = lapply(intervals, function(x) x[, 1])
        upper = lapply(intervals, function(x) x[, 2])
        if (any(lengths(lower) > 1) || any(lengths(upper) > 1)) {
          stop0(
            "You are summarizing a multimodal distribution using a method that returns\n",
            "multiple intervals (such as `hdi()`), but you are attempting to generate\n",
            "intervals for multiple columns in wide format.\n\n",
            "To use a multiple-interval method like `hdi()` on distributions that are\n",
            "multi-modal, you can only summarize one column at a time.\n\n",
            "You might try using `tidybayes::gather_variables()` to put all your draws into\n",
            "a single column before summarizing them, or use an interval type that always\n",
            "returns exactly one interval per probability level (such as `hdci()` or `qi()`)."
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

  result = map_dfr_(.width, function(p) {
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
point_interval.rvar = function(
  .data, ...,
  .width = .95, .point = median, .interval = qi, .simple_names = TRUE, na.rm = FALSE
) {
  x = .data
  # using substitute here so that names of .point / .interval are passed down correctly
  eval(substitute(point_interval(
    tibble(.value = x), ...,
    .width = .width, .point = .point, .interval = .interval, .simple_names = .simple_names, na.rm = na.rm
  )))
}

#' @rdname point_interval
#' @export
point_interval.distribution = point_interval.rvar


#' @importFrom stats quantile
#' @export
#' @rdname point_interval
qi = function(x, .width = .95, .prob, na.rm = FALSE) {
  .width = .Deprecated_argument_alias(.width, .prob)

  lower_prob = (1 - .width) / 2
  upper_prob = (1 + .width) / 2

  qi_(x, lower_prob, upper_prob, na.rm)
}

qi_ = function(x, lower_prob, upper_prob, na.rm) {
  if (!na.rm && anyNA(x)) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }

  if (distributional::is_distribution(x)) {
    #TODO: when #114 / distributional#72 is fixed, pass na.rm to quantile in this call
    do.call(rbind, quantile(x, c(lower_prob, upper_prob)))
  } else {
    matrix(quantile(x, c(lower_prob, upper_prob), na.rm = na.rm), ncol = 2)
  }
}

#' @export
#' @rdname point_interval
ll = function(x, .width = .95, na.rm = FALSE) {
  lower_prob = 1 - .width
  upper_prob = rep(1, length(.width))

  out = qi_(x, lower_prob, upper_prob, na.rm)
  out[,2] = Inf
  out
}

#' @export
#' @rdname point_interval
ul = function(x, .width = .95, na.rm = FALSE) {
  lower_prob = rep(0, length(.width))
  upper_prob = .width

  out = qi_(x, lower_prob, upper_prob, na.rm)
  out[,1] = -Inf
  out
}

#' @export
#' @rdname point_interval
hdi = function(x, .width = .95, .prob, na.rm = FALSE, ...) {
  .width = .Deprecated_argument_alias(.width, .prob)
  hdi_(x, .width = .width, na.rm = na.rm)
}
hdi_ = function(x, ...) {
  UseMethod("hdi_")
}
#' @importFrom stats density
#' @export
hdi_.numeric = function(x, .width = .95, na.rm = FALSE, ...) {
  if (!na.rm && anyNA(x)) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }

  intervals = HDInterval::hdi(density(x, cut = 0, na.rm = na.rm), credMass = .width, allowSplit = TRUE)
  if (nrow(intervals) == 1) {
    # the above method tends to be a little conservative on unimodal distributions, so if the
    # result is unimodal, switch to the method below (which will be slightly narrower)
    intervals = HDInterval::hdi(x, credMass = .width)
  }
  matrix(intervals, ncol = 2)
}
#' @export
hdi_.rvar = function(x, ...) {
  if (length(x) > 1) {
    stop0("HDI for non-scalar rvars is not implemented")
  }
  hdi_.numeric(posterior::draws_of(x), ...)
}
#' @importFrom distributional hdr
#' @export
hdi_.distribution = function(x, .width = .95, ...) {
  if (length(x) > 1) {
    stop0("HDI for non-scalar distributions is not implemented")
  }
  if (anyNA(x)) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }

  hilos = hdr(x, .width * 100, ...)
  matrix(c(unlist(vctrs::field(hilos, "lower")), unlist(vctrs::field(hilos, "upper"))), ncol = 2)
}

#' @export
#' @rdname point_interval
#' @importFrom rlang is_integerish
#' @importFrom stats density
Mode = function(x, na.rm = FALSE) {
  UseMethod("Mode")
}
#' @export
#' @rdname point_interval
Mode.default = function(x, na.rm = FALSE) {
  if (na.rm) {
    x = x[!is.na(x)]
  }
  else if (anyNA(x)) {
    return(NA_real_)
  }

  if (is_integerish(x)) {
    # for the discrete case, based on https://stackoverflow.com/a/8189441
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  } else {
    # for the continuous case
    d = density(x, cut = 0)
    d$x[which.max(d$y)]
  }
}
#' @export
#' @rdname point_interval
Mode.rvar = function(x, na.rm = FALSE) {
  draws <- posterior::draws_of(x)
  dim <- dim(draws)
  apply(draws, seq_along(dim)[-1], Mode, na.rm = na.rm)
}
#' @importFrom stats optim
#' @export
#' @rdname point_interval
Mode.distribution = function(x, na.rm = FALSE) {
  find_mode = function(x) {
    if (anyNA(x)) {
      NA_real_
    } else if (distr_is_sample(x)) {
      Mode(distr_get_sample(x), na.rm = na.rm)
    } else {
      optim(
        median(x, na.rm = na.rm),
        function(q) -density(x, at = q, na.rm = na.rm),
        #TODO: when #114 / distributional#72 is fixed, pass na.rm to quantile below
        lower = quantile(x, 0),
        upper = quantile(x, 1),
        method = "L-BFGS-B"
      )$par
    }
  }

  map_dbl_(x, find_mode)
}


#' @export
#' @rdname point_interval
hdci = function(x, .width = .95, na.rm = FALSE) {
  hdci_(x, .width = .width, na.rm = na.rm)
}
hdci_ = function(x, ...) {
  UseMethod("hdci_")
}
#' @importFrom stats density
#' @export
hdci_.numeric = function(x, .width = .95, na.rm = FALSE, ...) {
  if (!na.rm && anyNA(x)) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }

  intervals = HDInterval::hdi(x, credMass = .width)
  matrix(intervals, ncol = 2)
}
#' @export
hdci_.rvar = function(x, ...) {
  if (length(x) > 1) {
    stop0("HDCI for non-scalar rvars is not implemented")
  }
  hdci_.numeric(posterior::draws_of(x), ...)
}
#' @importFrom distributional hdr
#' @export
hdci_.distribution = function(x, .width = .95, na.rm = FALSE, ...) {
  if (length(x) > 1) {
    stop0("HDCI for non-scalar distributions is not implemented")
  }
  if (anyNA(x)) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }

  #TODO: after #114, pass na.rm to quantile here
  intervals = HDInterval::hdi(function(p) quantile(x, p), credMass = .width)
  matrix(intervals, ncol = 2)
}

#' @export
#' @rdname point_interval
mean_qi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = mean, .interval = qi)

#' @export
#' @rdname point_interval
median_qi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = median, .interval = qi)

#' @export
#' @rdname point_interval
mode_qi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = Mode, .interval = qi)

#' @export
#' @rdname point_interval
mean_ll = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = mean, .interval = ll)

#' @export
#' @rdname point_interval
median_ll = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = median, .interval = ll)

#' @export
#' @rdname point_interval
mode_ll = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = Mode, .interval = ll)

#' @export
#' @rdname point_interval
mean_ul = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = mean, .interval = ul)

#' @export
#' @rdname point_interval
median_ul = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = median, .interval = ul)

#' @export
#' @rdname point_interval
mode_ul = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = Mode, .interval = ul)

#' @export
#' @rdname point_interval
mean_hdi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = mean, .interval = hdi)

#' @export
#' @rdname point_interval
median_hdi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = median, .interval = hdi)

#' @export
#' @rdname point_interval
mode_hdi = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = Mode, .interval = hdi)

#' @export
#' @rdname point_interval
mean_hdci = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = mean, .interval = hdci)

#' @export
#' @rdname point_interval
median_hdci = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = median, .interval = hdci)

#' @export
#' @rdname point_interval
mode_hdci = function(.data, ..., .width = .95)
  point_interval(.data, ..., .width = .width, .point = Mode, .interval = hdci)
