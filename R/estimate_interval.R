# [estimate]_[interval] functions for use with tidy data
# 
# Author: mjskay
###############################################################################

#' @importFrom stats quantile
#' @export
qi = function(x, prob = .95) {
    lower_prob = (1 - prob)/2
    upper_prob = (1 + prob)/2
    unname(quantile(x, c(lower_prob, upper_prob)))
}

# TODO: rename point_interval?
#' @export
estimate_interval = function(data, ..., prob=.95, estimate = mean, interval = qi) {
    #this gets a list of unevaluated parameters passed in using `...`
    .names = as.list(substitute(list(...)))[-1L]

    do(data, with(., Reduce(cbind, lapply(.names, function(.name) {
        interval = interval(eval(.name), prob)
        result = data.frame(
            estimate(eval(.name)),
            interval[[1]],
            interval[[2]],
            prob
        )
        names(result) = c(
            deparse(.name),
            paste0(deparse(.name), ".lower"),
            paste0(deparse(.name), ".upper"),
            paste0(deparse(.name), ".prob")
        )
        result
    }))))
}

#' @export
mean_qi = function(data, ..., prob=.95) 
    estimate_interval(data, ..., prob = prob, estimate = mean, interval = qi)

#' @importFrom LaplacesDemon Mode
#' @export
mode_qi = function(data, ..., prob=.95) 
    estimate_interval(data, ..., prob = prob, estimate = Mode, interval = qi)
