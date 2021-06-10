# Helpers for testthat tests
#
# Author: mjskay
###############################################################################

# skip tests if there is no vdiffr setup or if the setup is likely
# to produce false positive test failures (e.g. old version of ggplot2)
skip_if_no_vdiffr = function() {
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not_installed("svglite")
  testthat::skip_if_not_installed("ggplot2", "3.3.3.9000")
}
