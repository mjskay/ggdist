# Deprecated function for generating fit curves and densities from posteriors
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
