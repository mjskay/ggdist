# Deprecated function for generating fit curves and densities from posteriors
#
# Author: mjskay
###############################################################################


#' Deprecated: Prediction curves for arbitrary functions of posteriors
#'
#' Deprecated function for generating prediction curves (or a density for a
#' prediction curve).
#'
#' **This function is deprecated.** Use [modelr::data_grid()] combined
#' with [point_interval()] or [dplyr::do()] and
#' [density_bins()] instead.
#'
#' The function generates a predictive curve given posterior draws
#' (`data`), an expression (`formula`), and a set of variables
#' defining the curve (`...`). For every group in `data` (if it is a
#' grouped data frame---see [group_by()]; otherwise the entire data
#' frame is taken at once), and for each combination of values in `...`,
#' the right-hand side of `formula` is evaluated and its results passed to
#' the `summary` function. This allows a predictive curve to be generated,
#' given (e.g.) some samples of coefficients in `data` and a set of
#' predictors defining the space of the curve in `...`.
#'
#' Given a summary function like [median()] or [mean()],
#' this function will produce the median (resp. mean) prediction at each point
#' on the curve.
#'
#' Given a summary function like [density_bins()], this function will
#' produce a predictive distribution for each point on the curve.
#' `predict_curve_density` is a shorthand for such a call, with a
#' convenient argument for adjusting the number of bins per point on the
#' curve.
#'
#' @param data A [data.frame], [tbl_df] or
#' [grouped_df] representing posteriors from a Bayesian model as
#' might be obtained through [spread_draws()]. Grouped data frames
#' as returned by [group_by()] are supported.
#' @param formula A formula specifying the prediction curve. The left-hand side
#' of the formula should be a name representing the name of the column that
#' will hold the predicted response in the returned data frame. The right-hand
#' side is an expression that may include numeric columns from `data` and
#' variables passed into this function in `...`.
#' @param summary The function to apply to summarize each predicted response.
#' Useful functions (if you just want a curve) might be [median()],
#' [mean()], or [Mode()]. If you want predictive distribution
#' at each point on the curve, try [density_bins()] or
#' [histogram_bins()].
#' @param ...  Variables defining the curve. The right-hand side of
#' `formula` is evaluated for every combination of values of variables in
#' `...`.
#' @param n For `predict_curve_density`, the number of bins to use to
#' represent the distribution at each point on the curve.
#' @return If `formula` is in the form `lhs ~ rhs` and `summary`
#' is a function that returns a single value, such as `median` or
#' `mode`, then `predict_curve` returns a `data.frame` with a
#' column for each group in `data` (if it was grouped), a column for each
#' variable in `...`, and a column named `lhs` with the value of
#' `summary(rhs)` evaluated for every group in `data` and combination
#' of variables in `...`.
#'
#' If `summary` is a function that returns a `data.frame`, such as
#' [density_bins()], `predict_curve` has the same set of columns
#' as above, except that in place of the `lhs` column is a set of columns
#' named `lhs.x` for every column named `x` returned by
#' `summary`. For example, [density_bins()] returns a data frame
#' with the columns `mid`, `lower`, `upper`, and `density`,
#' so the data frame returned by `predict_curve` with `summary =
#' density_bins` will have columns `lhs.mid`, `lhs.lower`,
#' `lhs.upper`, and `lhs.density` in place of `lhs`.
#'
#' @author Matthew Kay
#' @seealso See [density_bins()].
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
      "use modelr::data_grid + point_interval, or modelr::data_grid + dplyr::summarise + density_bins instead."))

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
