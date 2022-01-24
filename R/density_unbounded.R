# Unbounded density estimator
#
# Author: mjskay
###############################################################################



# unbounded density estimator -----------------------------------------------

#' Unbounded density estimator
#'
#' Uses [stats::density()]
#' @noRd
density_unbounded = function(
  x,
  n = 512, bandwidth = "bw.nrd0", adjust = 1,
  trim = TRUE,
  kernel = "gaussian",
  ...
) {
  if (missing(x)) return(partial_self("density_unbounded"))

  cut = if (trim) 0 else 3
  density(x, n = n, bw = bandwidth, adjust = adjust, cut = cut, kernel = kernel)
}
