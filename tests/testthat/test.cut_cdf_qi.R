# Tests for cut_cdf_qi
#
# Author: mjskay
###############################################################################




test_that("cut_cdf_qi works", {
  x = qnorm(ppoints(10))
  p = pnorm(x)

  expect_equal(
    cut_cdf_qi(p, .width = c(.5, .8)),
    ordered(c(NA, 0.8, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.8, NA))
  )

  expect_equal(
    cut_cdf_qi(p, .width = c(.5, .8), labels = function(x) paste0(x * 100, "%")),
    ordered(c(NA, "80%", "50%", "50%", "50%", "50%", "50%", "50%", "80%", NA))
  )

})
