# Tests for weighted histograms
#
# Author: mjskay
###############################################################################


test_that("weighted_hist works", {
  expect_error(weighted_hist(numeric()))

  expect_equal(weighted_hist(1), hist(1, plot = FALSE))

  x = c(1,1,1,1,1,1,3)
  expect_equal(weighted_hist(x), hist(x, plot = FALSE))

  xw = c(1, 3)
  w = c(6, 1)
  wh = weighted_hist(xw, w)
  expect_equal(wh$xname, "[xw, w]")
  wh$xname = "x"
  expect_equal(wh, hist(x))
})
