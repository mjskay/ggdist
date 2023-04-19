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
#' the distribution is multimodal `hdci` is not a highest-density interval.
#'
#' `hdci` yields the highest-density *continuous* interval, also known as the shortest
#' probability interval. **Note:** If the distribution is multimodal, this may not actually
#' be the highest-density interval (there may be a higher-density
#' discontinuous interval, which can be found using `hdi`).
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
#' names used in \pkg{ggdist} and \pkg{tidybayes}.
#' @param na.rm logical value indicating whether `NA` values should be stripped before the computation proceeds.
#' If `FALSE` (the default), any vectors to be summarized that contain `NA` will result in
#' point and interval summaries equal to `NA`.
#' @param x vector to summarize (for interval functions: `qi` and `hdi`)
#' @param density For [hdi()] and [Mode()], the kernel density estimator to use, either as
#' a function (e.g. [`density_bounded`], [`density_unbounded`]) or as a string giving the
#' suffix to a function that starts with `density_` (e.g. `"bounded"` or `"unbounded"`). The
#' default, `"bounded"`, uses the bounded density estimator of [density_bounded()], which
#' itself estimates the bounds of the distribution, and tends to work well on both bounded
#' and unbounded data.
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
#' @importFrom dplyr bind_cols group_vars summarise_at %>%
#' @importFrom rlang quos quos_auto_name eval_tidy syms
#' @importFrom stats median
#' @importFrom tibble as_tibble
#' @export
point_interval = function(.data, ..., .width = .95, .point = median, .interval = qi, .simple_names = TRUE,
  na.rm = FALSE, .exclude = c(".chain", ".iteration", ".draw", ".row"), .prob
) {
  if (missing(.data)) return(partial_self("point_interval"))

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
    if (!is.list(data[[col_name]])) {
      data = summarise_at(data, col_name, list)
    }

    result = map_dfr_(.width, function(p) {
      # compute intervals; this is robust to grouped data frames and
      # to intervals that can return multiple intervals (e.g., hdi())

      # for each row of `data`, compute the point and the intervals (may be more than one),
      # and construct a tibble with grouping factors (if any), point estimate,
      # lower and upper values, and width
      # - equivalent to unnest_legacy()
      data = row_map_dfr_(data, function(row) {
        draws = row[[col_name]]

        # if multivariate rvar => flatten it first
        if (inherits(draws, "rvar") && length(draws) > 1) {
          flat_draws = flatten_array(draws)
          draws = flat_draws$x
          row[[col_name]] = NA # the next line will have to recycle row[[col_name]]
                               # which may be expensive b/c it is an rvar, so just
                               # skip that since we're overwriting it after anyway
          row = bind_cols(row, .index = flat_draws$index_names)
          row[[col_name]] = draws
        }

        # unless draws is multivariate this will usually be just one iteration
        map_dfr_(seq_len(nrow(row)), function(j) {
          # get row of `data` with grouping factors
          # faster version of row_j = row[j, , drop = FALSE]
          row_j = tibble::new_tibble(lapply(row, vctrs::vec_slice, j), nrow = 1)
          row.names(row_j) = NULL
          draws_j = draws[[j]]

          # calculate point estimate --- usually a scalar
          point_j = .point(draws_j, na.rm = na.rm)

          # if this is a multivariate distributional object, flatten the point estimate
          if (distributional::is_distribution(draws_j) && length(point_j) > 1) {
            flat_point = flatten_array(point_j)
            point_j = flat_point$x
            row_j[[col_name]] = NA
            row_j = bind_cols(row_j, .index = flat_point$index_names)
          }
          row_j[[col_name]] = as.vector(point_j)

          # calculate intervals (one or more rows)
          interval = .interval(draws_j, .width = p, na.rm = na.rm)
          dimnames(interval)[[2]] = c(".lower", ".upper")

          cbind(
            row_j,
            interval,
            .width = p
          )
        })
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
    do.call(rbind, lapply(quantile(x, c(lower_prob, upper_prob)), t))
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
}

#' @export
#' @rdname point_interval
ul = function(x, .width = .95, na.rm = FALSE) {
  lower_prob = rep(0, length(.width))
  upper_prob = .width

  out = qi_(x, lower_prob, upper_prob, na.rm)
}

#' @export
#' @rdname point_interval
hdi = function(x, .width = .95, .prob, na.rm = FALSE, density = "bounded", ...) {
  .width = .Deprecated_argument_alias(.width, .prob)
  hdi_(x, .width = .width, na.rm = na.rm, density = density)
}
hdi_ = function(x, ...) {
  UseMethod("hdi_")
}
#' @importFrom stats density
#' @export
hdi_.numeric = function(x, .width = .95, na.rm = FALSE, density = "bounded", ...) {
  if (!na.rm && anyNA(x)) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }
  if (isTRUE(.width == 1)) {
    return(matrix(range(x), ncol = 2))
  }
  x = check_na(x, na.rm)

  intervals = .hdi_numeric(x, .width = .width, density = density)
  if (nrow(intervals) == 1) {
    # if the result is unimodal, switch to hdci (which will be more accurate)
    intervals = hdci_.numeric(x, .width = .width)
  }
  intervals
}

# based on hdr.dist_default from {distributional}
# https://github.com/mitchelloharawild/distributional/blob/50e29554456d99e9b7671ba6110bebe5961683d2/R/default.R#L137
#' @importFrom stats approx
.hdi_numeric = function(x, .width = 0.95, n = 4096, density = "bounded", ...) {
  density = match_function(density, "density_")

  dist_x = quantile(x, ppoints(n, a = 0.5))
  # Remove duplicate values of dist_x from less continuous distributions
  dist_x = unique(dist_x)
  d = density(x, n = n)
  dist_y = approx(d$x, d$y, dist_x)$y
  alpha = quantile(dist_y, probs = 1 - .width)

  it = seq_len(length(dist_y) - 1)
  y_minus_alpha = dist_y - alpha
  dd = y_minus_alpha[it + 1] * y_minus_alpha[it]
  index = it[dd <= 0]
  # unique() removes possible duplicates if sequential dd has same value.
  y0 = y_minus_alpha[index]
  y1 = y_minus_alpha[index + 1]
  x0 = dist_x[index]
  x1 = dist_x[index + 1]
  hdr = unique(x1 - (x1 - x0) / (y1 - y0) * y1)
  # Add boundary values which may exceed the crossing point.
  hdr = c(dist_x[1][dist_y[1] > alpha], hdr, dist_x[length(dist_x)][dist_y[length(dist_y)] > alpha])

  matrix(hdr, ncol = 2, byrow = TRUE)
}

#' @export
hdi_.rvar = function(x, ...) {
  if (length(x) > 1) {
    stop0("HDI for non-scalar rvars is not implemented")
  }
  hdi_.numeric(posterior::draws_of(x), ...)
}
#' @importFrom distributional hdr support
#' @export
hdi_.distribution = function(x, .width = .95, ...) {
  if (length(x) > 1) {
    stop0("HDI for non-scalar distribution objects is not implemented")
  }
  if (length(dim(vctrs::field(support(x), "x")[[1]])) > 1) {
    stop0("HDI for multivariate distribution objects is not implemented")
  }
  if (anyNA(x)) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }
  if (isTRUE(.width == 1)) {
    return(matrix(quantile(x, c(0, 1))[[1]], ncol = 2))
  }

  hilos = hdr(x, .width * 100, ...)
  matrix(c(unlist(vctrs::field(hilos, "lower")), unlist(vctrs::field(hilos, "upper"))), ncol = 2)
}

#' @export
#' @rdname point_interval
#' @importFrom rlang is_integerish
#' @importFrom stats density
Mode = function(x, na.rm = FALSE, ...) {
  UseMethod("Mode")
}
#' @export
#' @rdname point_interval
Mode.default = function(x, na.rm = FALSE, density = "bounded", ...) {
  if (na.rm) {
    x = x[!is.na(x)]
  }
  else if (anyNA(x)) {
    return(NA_real_)
  }
  density = match_function(density, "density_")

  if (is_integerish(x)) {
    # for the discrete case, based on https://stackoverflow.com/a/8189441
    ux = unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  } else {
    # for the continuous case
    d = density(x, n = 2001)
    d$x[which.max(d$y)]
  }
}
#' @export
#' @rdname point_interval
Mode.rvar = function(x, na.rm = FALSE, ...) {
  draws <- posterior::draws_of(x)
  dim <- dim(draws)
  apply(draws, seq_along(dim)[-1], Mode, na.rm = na.rm)
}
#' @importFrom stats optim
#' @export
#' @rdname point_interval
Mode.distribution = function(x, na.rm = FALSE, ...) {
  find_mode = function(x) {
    if (anyNA(x)) {
      NA_real_
    } else if (distr_is_sample(x)) {
      Mode(distr_get_sample(x), na.rm = na.rm)
    } else if (distr_is_discrete(x)) {
      bounds = quantile(x, c(0, 1))[[1]]
      non_finite_bounds = !is.finite(bounds)
      if (any(non_finite_bounds)) {
        bounds[non_finite_bounds] = quantile(x, c(0.001, 0.999)[non_finite_bounds])[[1]]
      }
      at = seq(bounds[[1]], bounds[[2]])
      d = density(x, at = at)[[1]]
      at[which.max(d)]
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
  if (isTRUE(.width == 1)) {
    return(matrix(range(x), ncol = 2))
  }

  # d = density_bounded(x, na.rm = na.rm, adjust = 0.1)
  # .hdci_function(ggdist::weighted_quantile_fun(d$x, weights = d$y, type = 5), .width = .width)
  .hdci_function(ggdist::weighted_quantile_fun(x, na.rm = na.rm, type = 5), .width = .width)
}

#' find the hdci using a quantile function
#' @noRd
.hdci_function = function(quantile_fun, .width = .95) {
  p_lower = optimize(
    function(p) quantile_fun(p + .width) - quantile_fun(p),
    lower = 0,
    upper = 1 - .width,
    tol = sqrt(.Machine$double.eps)
  )$minimum
  endpoints = quantile_fun(c(p_lower, p_lower + .width))
  matrix(endpoints, ncol = 2)
}

#' @export
hdci_.rvar = function(x, ...) {
  if (length(x) > 1) {
    stop0("HDCI for non-scalar rvars is not implemented")
  }
  hdci_.numeric(posterior::draws_of(x), ...)
}
#' @importFrom distributional hdr support
#' @export
hdci_.distribution = function(x, .width = .95, na.rm = FALSE, ...) {
  if (length(x) > 1) {
    stop0("HDCI for non-scalar distribution objects is not implemented")
  }
  if (length(dim(vctrs::field(support(x), "x")[[1]])) > 1) {
    stop0("HDCI for multivariate distribution objects is not implemented")
  }
  if (anyNA(x)) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }
  if (isTRUE(.width == 1)) {
    return(matrix(quantile(x, c(0, 1))[[1]], ncol = 2))
  }

  .hdci_function(function(p) quantile(x, p)[[1]], .width = .width)
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
