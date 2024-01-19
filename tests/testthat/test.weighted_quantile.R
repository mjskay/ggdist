# Tests for weighted quantiles
#
# Author: mjskay
###############################################################################


test_that("weighted_quantile is equivalent to quantile on non-weighted samples", {
  x = 1:10

  p = ppoints(20, a = 1)
  for (type in 1:9) {
    expect_equal(weighted_quantile(x, p, type = !!type), quantile(x, p, type = !!type))
  }
})

test_that("weighted_quantile is equivalent to quantile on weighted samples", {
  x = c(1,1,1,1,2,2,2,3,3,4)
  xw = c(1:4)
  w = c(4:1)

  p = ppoints(20, a = 1)
  for (type in 1:9) {
    expect_equal(
      weighted_quantile(xw, p, weights = w, n = "sum", type = !!type),
      quantile(x, p, type = !!type)
    )
  }
})

test_that("na.rm works", {
  expect_equal(
    weighted_quantile(c(1:4, NA), weights = c(4:2, NA, 1), ppoints(10), na.rm = TRUE),
    weighted_quantile(1:3, weights = 4:2, ppoints(10))
  )
})

test_that("type is in 1:9", {
  expect_error(weighted_quantile(1, 0.5, type = 0))
})

test_that("0- and 1-length vectors work", {
  # these fail in R < 4.0.3 because of a bug in quantile()
  skip_if_not(getRversion() >= "4.0.3")

  expect_equal(weighted_quantile(1, c(0, 0.5, 1, NA)), c("0%" = 1, "50%" = 1, "100%" = 1, NA))
  expect_equal(weighted_quantile(c(1,1), c(0, 0.5, 1, NA), names = FALSE), c(1, 1, 1, NA))
  expect_equal(weighted_quantile(c(1,2), weights = c(1,0), c(0, 0.5, 1, NA), names = FALSE), c(1, 1, 1, NA))

  expect_equal(weighted_quantile(numeric(), c(0, 0.5, 1, NA), names = FALSE), c(NA_real_, NA_real_, NA_real_, NA_real_))
})

