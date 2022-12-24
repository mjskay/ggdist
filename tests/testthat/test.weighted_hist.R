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
  expect_equal(wh, hist(x, plot = FALSE))
})

test_that("weighted_hist is equivalent to hist on non-weighted samples", {
  x = c(1,2,3,3,4,5,8,2,7)

  for (breaks in list("Sturges", "Scott", "FD", c(1,3,6,10))) {
    expect_equal(weighted_hist(x, breaks = !!breaks), hist(x, breaks = !!breaks, plot = FALSE))
  }

  for (breaks in list("Sturges", "Scott", "FD")) {
    # hist with some values of breaks fails on length(x) == 1, but all these should
    # be the same so we just test against hist without setting breaks
    expect_equal(weighted_hist(1, breaks = !!breaks), hist(1, plot = FALSE))
  }
})

test_that("weighted_hist is equivalent to hist on weighted samples", {
  x = c(1,1,1,1,2,2,2,3,3,4,6)
  xw = c(1:4,6)
  w = c(4:1,1)

  for (breaks in list("Sturges", "Scott", "FD", c(1,3,4,7))) {
    wh = weighted_hist(xw, weights = w, breaks = breaks)
    wh$xname = "x"
    expect_equal(
      wh,
      hist(x, breaks = !!breaks, plot = FALSE)
    )
  }
})
