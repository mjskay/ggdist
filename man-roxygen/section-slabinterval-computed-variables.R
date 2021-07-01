#' @section Computed Variables:
#' The following variables are computed by this stat and made available for
#' use in aesthetic specifications (`aes()`) using the `stat()` or `after_stat()`
#' functions:
#' - `x` or `y`: For slabs, the input values to the slab function.
#'   For intervals, the point summary from the interval function. Whether it is `x` or `y` depends on `orientation`
#' - `xmin` or `ymin`: For intervals, the lower end of the interval from the interval function.
#' - `xmax` or `ymax`: For intervals, the upper end of the interval from the interval function.
#' - `.width`: For intervals, the interval width as a numeric value in [0, 1].
#' - `level`: For intervals, the interval width as an ordered factor.
#' -  `f`: For slabs, the output values from the slab function (such as the PDF, CDF, or CCDF),
#'   determined by `slab_type`.
#' - `pdf`: For slabs, the probability density function.
#' - `cdf`: For slabs, the cumulative distribution function.
#' - `n`: For slabs, the number of data points summarized into that slab.
