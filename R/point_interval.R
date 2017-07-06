# [point]_[interval] functions for use with tidy data
# 
# Author: mjskay
###############################################################################


#' Point and interval estimates for tidy sample data
#' 
#' Translates samples in a (possibly grouped) data frame into a point and
#' interval estimate (or set of point and interval estimates, if there are
#' multiple groups in a grouped data frame).
#' 
#' If \code{data} is a data frame, then \code{...} is a list of bare names of
#' columns (or expressions derived from columns) of \code{data}, on which
#' the point and interval estimates are derived. For a column named \code{x},
#' the resulting data frame will have columns \code{x} (the point estimate),
#' \code{x.lower} (the lower end of the interval), \code{x.upper} (the upper
#' end of the interval), and \code{x.prob} (the value of \code{prob}).
#' 
#' If \code{data} includes groups (see e.g. \code{\link[dplyr]{group_by}}),
#' the points and intervals are calculated within the groups.
#' 
#' If \code{data} is a vector, \code{...} is ignored and the result is a
#' data frame with one row per value of \code{prob} and three columns:
#' \code{y} (the point estimate), \code{ymin} (the lower end of the interval),
#' \code{ymax} (the upper end of the interval), and \code{prob}, the probability
#' corresponding to the interval. This behavior allows \code{point_interval}
#' and its derived functions (like \code{mean_qi}, \code{median_qi}, etc)
#' to be easily used to plot intervals in ggplot using methods like 
#' \code{\link{geom_eye}}, \code{\link{ggeye}}, or \code{\link{stat_summary}}.
#'
#' \code{mean_qi}, \code{mode_qi}, etc are short forms for 
#' \code{point_interval(..., point = mean, interval = qi)}, etc.
#' 
#' \code{qi} yields the quantile interval (also known as the percentile interval or
#' equi-tailed interval).
#' 
#' \code{hdi} yields the highest-density interval (also known as the highest posterior
#' density interval).
#' 
#' @param data Data frame (or grouped data frame as returned by \code{\link{group_by}})
#' that contains samples to summarize.
#' @param ... Bare column names or expressions that, when evaluated in the context of
#' \code{data}, represent samples to summarise. If this is empty, then by default all
#' columns that are not group columns or start with \code{"."} (e.g. \code{".chain"}
#' or \code{".iteration"}) will be summarised.
#' @param prob vector of probabilities to use for generating intervals. If multiple
#' probabilities are provided, multiple rows per group are generated, each with
#' a different probabilty interval (and value of the corresponding \code{.prob} columns).
#' @param point Point estimate function, which takes a vector and returns a single
#' value, e.g. \code{\link{mean}}, \code{\link{median}}, or \code{\link{Mode}}.
#' @param interval Interval estimate function, which takes a vector and a probability
#' (\code{prob}) and returns a two-element vector representing the lower and upper
#' bound of an interval; e.g. \code{\link{qi}}, \code{\link{hdi}}
#' @param x vector to summarise (for interval functions: \code{qi} and \code{hdi})
#' @author Matthew Kay
#' @examples
#' 
#' ##TODO
#' 
#' @importFrom purrr map_df map map2 discard
#' @importFrom dplyr do bind_cols
#' @importFrom lazyeval lazy_dots as.lazy_dots auto_name
#' @importFrom stringi stri_startswith_fixed
#' @export
point_interval = function(data, ..., prob=.95, point = mean, interval = qi) UseMethod("point_interval")
#' @rdname point_interval
#' @export
point_interval.default = function(data, ..., prob=.95, point = mean, interval = qi) {
    col_exprs = auto_name(lazy_dots(...))

    if (length(col_exprs) == 0) {
        # no column expressions provided => summarise all columns that are not groups and which
        # do not start with "."
        col_exprs = names(data) %>%
            map(as.name) %>% 
            #don't aggregate groups because we aggregate within these
            setdiff(groups(data)) %>% 
            #don't aggregate columns that start with "." because these are special columns (such
            #as .chain or .iteration)
            discard(~ stri_startswith_fixed(.x, ".")) %>%
            as.lazy_dots() %>%
            auto_name()

        if (length(col_exprs) == 0) {
            #still nothing to aggregate? not sure what the user wants
            stop("No columns found to calculate point and interval estimates for.")
        }
    }
    
    map_df(prob, function(p) {
        do(data, bind_cols(map2(col_exprs, names(col_exprs), function(col_expr, col_name) {
            col_samples = lazy_eval(col_expr, .)
            interval = interval(col_samples, prob = p)
            result = data.frame(
                point(col_samples),
                interval[[1]],
                interval[[2]],
                p
            )
            names(result) = c(
                col_name,
                paste0(col_name, ".lower"),
                paste0(col_name, ".upper"),
                paste0(col_name, ".prob")
            )
            result
        })))
    })
}
#' @rdname point_interval
#' @export
point_interval.numeric = function(data, ..., prob=.95, point = mean, interval = qi) {
    map_df(prob, function(p) {
        interval = interval(data, prob = p)
        data.frame(
            y = point(data),
            ymin = interval[[1]],
            ymax = interval[[2]],
            prob = p
        ) 
    })
}

#' @importFrom stats quantile
#' @export
#' @rdname point_interval
qi = function(x, prob = .95) {
    lower_prob = (1 - prob)/2
    upper_prob = (1 + prob)/2
    unname(quantile(x, c(lower_prob, upper_prob)))
}

#' @importFrom coda HPDinterval as.mcmc
#' @export
#' @rdname point_interval
hdi = function(x, prob = .95) {
    as.vector(HPDinterval(as.mcmc(x), prob = prob))
}

#' @export
#' @rdname point_interval
mean_qi = function(data, ..., prob=.95) 
    point_interval(data, ..., prob = prob, point = mean, interval = qi)

#' @export
#' @rdname point_interval
median_qi = function(data, ..., prob=.95) 
    point_interval(data, ..., prob = prob, point = median, interval = qi)

#' @importFrom LaplacesDemon Mode
#' @export
#' @rdname point_interval
mode_qi = function(data, ..., prob=.95) 
    point_interval(data, ..., prob = prob, point = Mode, interval = qi)

#' @export
#' @rdname point_interval
mean_hdi = function(data, ..., prob=.95) 
    point_interval(data, ..., prob = prob, point = mean, interval = hdi)

#' @export
#' @rdname point_interval
median_hdi = function(data, ..., prob=.95) 
    point_interval(data, ..., prob = prob, point = median, interval = hdi)

#' @importFrom LaplacesDemon Mode
#' @export
#' @rdname point_interval
mode_hdi = function(data, ..., prob=.95) 
    point_interval(data, ..., prob = prob, point = Mode, interval = hdi)
