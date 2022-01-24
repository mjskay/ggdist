# Unbounded density estimator
#
# Author: mjskay
###############################################################################



# unbounded density estimator -----------------------------------------------

#' Unbounded density estimator
#'
#' Uses [stats::density()]. Supports [partial function application][partial-functions].
#' @noRd
density_unbounded = function(
  x,
  n = 512, bandwidth = "bw.nrd0", adjust = 1,
  trim = TRUE,
  kernel = "gaussian",
  ...
) {
  if (missing(x)) return(partial_self("density_unbounded"))

  if (!is.numeric(bandwidth)) {
    bandwidth = match.fun(bandwidth)(x)
  }

  cut = if (trim) 0 else 3
  out = density(x, n = n, bw = bandwidth, adjust = adjust, cut = cut, kernel = kernel)
  out$call = match.call()
  out
}
