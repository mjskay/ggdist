# Helpers for testthat tests
#
# Author: mjskay
###############################################################################

#' skip tests if there is no vdiffr setup or if the setup is likely
#' to produce false positive test failures (e.g. old version of ggplot2)
#' @noRd
skip_if_no_vdiffr = function() {
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not_installed("ggplot2", "3.3.3.9000")
}

#' skip tests if linearGradient support for visual test cases is not available
#' (old versions of svglite did not support it and so test cases with
#' linearGradients would be incorrect)
skip_if_no_linearGradient = function() {
  testthat::skip_if_not(getRversion() >= "4.1")
  testthat::skip_if_not_installed("svglite", "2.0.0.9000")
}
