# Tests for weighted quantiles
#
# Author: mjskay
###############################################################################


test_that("weighted_quantile is equivalent to quantile on non-weighted samples", {
  x = 1:10

  p = ppoints(20, a = 1)
  for (type in 1:9) {
    expect_equal(weighted_quantile(x, p, type = !!type), quantile(x, p, type = !!type, names = FALSE))
  }
})

test_that("weighted_quantile is equivalent to quantile on weighted samples", {
  x = c(1,1,1,1,2,2,2,3,3,4)
  xw = 1:4
  w = 4:1

  p = ppoints(20, a = 1)
  for (type in 1:9) {
    expect_equal(
      weighted_quantile(xw, p, weights = w, n = "sum", type = !!type),
      quantile(x, p, type = !!type, names = FALSE)
    )
  }
})
