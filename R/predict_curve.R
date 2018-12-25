# fit_curve and for generate fit curves and densities from posteriors
#
# Author: mjskay
###############################################################################



#' Deprecated: Prediction curves for arbitrary functions of posteriors
#'
#' Deprecated function for generating prediction curves (or a density for a
#' prediction curve).
#'
#' \strong{This function is deprecated.} Use \code{\link[modelr]{data_grid}} combined
#' with \code{\link{point_interval}} or \code{\link[dplyr]{do}} and
#' \code{\link{density_bins}} instead.
#'
#' The function generates a predictive curve given posterior draws
#' (\code{data}), an expression (\code{formula}), and a set of variables
#' defining the curve (\code{...}). For every group in \code{data} (if it is a
#' grouped data frame---see \code{\link{group_by}}; otherwise the entire data
#' frame is taken at once), and for each combination of values in \code{...},
#' the right-hand side of \code{formula} is evaluated and its results passed to
#' the \code{summary} function. This allows a predictive curve to be generated,
#' given (e.g.) some samples of coefficients in \code{data} and a set of
#' predictors defining the space of the curve in \code{...}.
#'
#' Given a summary function like \code{\link{median}} or \code{\link{mean}},
#' this function will produce the median (resp. mean) prediction at each point
#' on the curve.
#'
#' Given a summary function like \code{\link{density_bins}}, this function will
#' produce a predictive distribution for each point on the curve.
#' \code{predict_curve_density} is a shorthand for such a call, with a
#' convenient argument for adjusting the number of bins per point on the
#' curve.
#'
#' @param data A \code{\link{data.frame}}, \code{\link{tbl_df}} or
#' \code{link{grouped_df}} representing posteriors from a Bayesian model as
#' might be obtained through \code{\link{spread_draws}}. Grouped data frames
#' as returned by \code{\link{group_by}} are supported.
#' @param formula A formula specifying the prediction curve. The left-hand side
#' of the formula should be a name representing the name of the column that
#' will hold the predicted response in the returned data frame. The right-hand
#' side is an expression that may include numeric columns from \code{data} and
#' variables passed into this function in \code{...}.
#' @param summary The function to apply to summarize each predicted response.
#' Useful functions (if you just want a curve) might be \code{\link{median}},
#' \code{\link{mean}}, or \code{\link{Mode}}. If you want predictive distribution
#' at each point on the curve, try \code{\link{density_bins}} or
#' \code{\link{histogram_bins}}.
#' @param ...  Variables defining the curve. The right-hand side of
#' \code{formula} is evaluated for every combination of values of variables in
#' \code{...}.
#' @param n For \code{predict_curve_density}, the number of bins to use to
#' represent the distribution at each point on the curve.
#' @return If \code{formula} is in the form \code{lhs ~ rhs} and \code{summary}
#' is a function that returns a single value, such as \code{median} or
#' \code{mode}, then \code{predict_curve} returns a \code{data.frame} with a
#' column for each group in \code{data} (if it was grouped), a column for each
#' variable in \code{...}, and a column named \code{lhs} with the value of
#' \code{summary(rhs)} evaluated for every group in \code{data} and combination
#' of variables in \code{...}.
#'
#' If \code{summary} is a function that returns a \code{data.frame}, such as
#' \code{\link{density_bins}}, \code{predict_curve} has the same set of columns
#' as above, except that in place of the \code{lhs} column is a set of columns
#' named \code{lhs.x} for every column named \code{x} returned by
#' \code{summary}. For example, \code{\link{density_bins}} returns a data frame
#' with the columns \code{mid}, \code{lower}, \code{upper}, and \code{density},
#' so the data frame returned by \code{predict_curve} with \code{summary =
#' density_bins} will have columns \code{lhs.mid}, \code{lhs.lower},
#' \code{lhs.upper}, and \code{lhs.density} in place of \code{lhs}.
#'
#' @author Matthew Kay
#' @seealso See \code{\link{density_bins}}.
#' @keywords manip
#' @examples
#'
#' # Deprecated; see examples for density_bins
#'
#' @importFrom plyr ldply
#' @importFrom stats median
#' @import dplyr
#' @export
predict_curve = function(data, formula, summary = median, ...) {
  .Deprecated("density_bins", package = "tidybayes",
    paste("predict_curve and predict_curve_density will be removed in a future version;",
      "use modelr::data_grid + point_interval, or modelr::data_grid + dplyr::do + density_bins instead."))

  #get response name and formula to generate response
  response_name = formula[[2]]         # nolint
  prediction_formula = formula[[3]]    # nolint

  #get the predictors we will vary over the curve
  varied_predictors = expand.grid(..., KEEP.OUT.ATTRS = FALSE)
  if (nrow(varied_predictors) == 0) {
    #no predictors provided => use a data frame with one row
    #and no columns so that the predictors are evaluated once
    #per group in data
    varied_predictors = data.frame(row.names = 1)
  }

  eval(bquote(
    do(data, {
      #for every group defined in data ...
      ldply(seq_len(nrow(varied_predictors)), function(r) {
        #and for every prediction point on the curve
        #defined by the values in (...)
        predictor_row = as.list(varied_predictors[r, , drop = FALSE]) # nolint
        # N.B. we convert predictor_row to a list first (then the final result back
        # to a data.frame) because if summary returns more than one row
        # (as in density prediction, for example) we can't just do the calculation
        # within a data.frame: predictor_row is always exactly one row, so there
        # will be a row count mismatch if summary returns > 1 row.
        # By doing the calculation within a list, the single predictor_row
        # will automatically be repeated to match the number of rows returned
        # when we convert back to a data.frame afterwards.
        within(predictor_row,
          #get the response value summarized by the summary function
          .(response_name) <- summary(with(., .(prediction_formula)))
        ) %>% as.data.frame(optional = TRUE)
      })
    })
  ))
}

#' @rdname predict_curve
#' @export
predict_curve_density = function(data, formula, summary = function(...) density_bins(..., n = n), n = 50, ...) {
  predict_curve(data, formula, summary = summary, ...)
}

#' Density bins as data frames suitable for use with predict_curve
#'
#' Generates a data frame of bins representing the kernel density (or
#' histogram) of a vector, suitable for use in generating predictive
#' distributions using predict_curve.
#'
#' These functions are simple wrappers to \code{\link{density}} and
#' \code{\link{hist}} that compute density estimates and return their results
#' in a consistent format: a data frame of bins suitable for use with
#' \code{\link{predict_curve}}.
#'
#' \code{density_bins} computes a kernel density estimate using
#' \code{\link{density}}.
#'
#' \code{histogram_bins} computes a density histogram using \code{\link{hist}}.
#'
#' @param x A numeric vector
#' @param n Number of bins
#' @param breaks Used to set bins for \code{histogram_bins}. Can be number of bins (by default it is set to the value
#' of \code{n}) or a method for setting bins. See the \code{breaks} argument of \code{\link{hist}}.
#' @param ...  Additional arguments passed to \code{\link{density}} or
#' \code{\link{hist}}.
#' @return A data frame representing bins and their densities with the
#' following columns: \item{mid}{Bin midpoint} \item{lower}{Lower endpoint of
#' each bin} \item{upper}{Upper endpoint of each bin} \item{density}{Density
#' estimate of the bin}
#' @author Matthew Kay
#' @seealso See \code{\link{add_predicted_draws}} and \code{\link{stat_lineribbon}} for a better approach. This
#' function may be deprecated in the future.
#' @keywords manip
#' @examples
#' \donttest{
#'
#' library(ggplot2)
#' library(dplyr)
#' library(purrr)
#' library(tidyr)
#'
#' if (
#'   require("rstanarm", quietly = TRUE) &&
#'   require("modelr", quietly = TRUE)
#' ) {
#'
#'   theme_set(theme_light())
#'
#'   m_mpg = stan_glm(mpg ~ hp * cyl, data = mtcars)
#'
#'   step = 1
#'   mtcars %>%
#'     group_by(cyl) %>%
#'     data_grid(hp = seq_range(hp, by = step)) %>%
#'     add_predicted_draws(m_mpg) %>%
#'     summarise_all(list) %>%
#'     mutate(densities = map(.prediction, density_bins)) %>%
#'     unnest(densities) %>%
#'     ggplot() +
#'     geom_rect(aes(
#'       xmin = hp - step/2, ymin = lower, ymax = upper, xmax = hp + step/2,
#'       fill = ordered(cyl), alpha = density
#'     )) +
#'     geom_point(aes(x = hp, y = mpg, fill = ordered(cyl)), shape = 21, data = mtcars) +
#'     scale_alpha_continuous(range = c(0, 1)) +
#'     scale_fill_brewer(palette = "Set2")
#' }
#' }
#' @importFrom stats density
#' @export
density_bins = function(x, n = 101, ...) {
  d = density(x, n = n, ...)

  mid = d$x
  last_mid = length(mid)
  x_diffs = mid[-1] - mid[-last_mid]

  tibble(
    mid = mid,
    lower = c(mid[[1]] - x_diffs[[1]] / 2, mid[-1] - x_diffs / 2),
    upper = c(mid[-last_mid] + x_diffs / 2, mid[[last_mid]] + x_diffs[[last_mid - 1]] / 2),
    density = d$y
  )
}

#' @rdname density_bins
#' @importFrom graphics hist
#' @importFrom stats embed
#' @export
histogram_bins = function(x, n = 30, breaks = n, ...) {
  h = hist(x, breaks = breaks, ..., plot = FALSE)

  tibble(
    mid = rowMeans(embed(h$breaks, 2)),
    lower = h$breaks[-length(h$breaks)],
    upper = h$breaks[-1],
    density = h$density
  )
}
