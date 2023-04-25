# Tests for weighted histograms
#
# Author: mjskay
###############################################################################


test_that("weighted_hist works", {
  expect_error(weighted_hist(numeric()))

  expect_equal(weighted_hist(1), hist(1, breaks = c(0.5, 1.5), plot = FALSE))

  x = c(1,1,1,1,1,1,4)
  expect_equal(weighted_hist(x), hist(x, plot = FALSE))

  xw = c(1, 4)
  w = c(6, 1)
  wh = weighted_hist(xw, w)
  expect_equal(wh$xname, "[xw, w]")
  wh$xname = "x"
  expect_equal(wh, hist(x, plot = FALSE))
})

test_that("weighted_hist is roughly equivalent to hist on non-weighted samples", {
  x = c(1,2,3,3,4,5,8,2,7)

  # breaks / nclass functions match
  for (breaks in list(c("Sturges", "Sturges"), c("Scott","scott"), c("FD","FD"))) {
    expect_equal(match.fun(paste0("breaks_", !!breaks[[1]]))(x), match.fun(paste0("nclass.", !!breaks[[2]]))(x))
  }

  # explicit breaks match
  expect_equal(weighted_hist(x, breaks = c(1,3,6,10)), hist(x, breaks = c(1,3,6,10), plot = FALSE))

  # hist with some values of breaks fails on length(x) == 1, but all these should
  # be the same so we test against hist with explicit breaks equal to what we want
  for (breaks in list("Sturges", "Scott", "FD")) {
    expect_equal(weighted_hist(1, breaks = !!breaks), hist(1, breaks = c(0.5, 1.5), plot = FALSE))
  }
})

test_that("weighted_hist is equivalent to hist on weighted samples", {
  x = c(1,1,1,1,2,2,2,3,3,4,6)
  xw = c(1:4,6)
  w = c(4:1,1)

  # breaks / nclass functions match
  for (breaks in list(c("Sturges", "Sturges"), c("Scott","scott"), c("FD","FD"))) {
    expect_equal(match.fun(paste0("breaks_", !!breaks[[1]]))(xw, w), match.fun(paste0("nclass.", !!breaks[[2]]))(x))
  }

  # explicit breaks match
  wh = weighted_hist(xw, weights = w, breaks = c(1,3,4,7))
  wh$xname = "x"
  expect_equal(wh, hist(x, breaks = c(1,3,4,7), plot = FALSE))
})
