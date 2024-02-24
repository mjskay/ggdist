# Tests for weighted histograms
#
# Author: mjskay
###############################################################################


test_that("weighted_hist works", {
  expect_error(weighted_hist(numeric()))

  expect_equal(weighted_hist(1), hist(1, breaks = c(0.5, 1.5), plot = FALSE))
  expect_equal(weighted_hist(c(1,1)), hist(c(1,1), breaks = c(0.5, 1.5), plot = FALSE))

  x = c(1,1,1,4)
  expect_equal(weighted_hist(x, breaks = "Sturges"), hist(x, plot = FALSE))

  xw = c(1, 4)
  w = c(3, 1)
  wh = weighted_hist(xw, w, breaks = "Sturges")
  expect_equal(wh$xname, "[xw, w]")
  wh$xname = "x"
  expect_equal(wh, hist(x, plot = FALSE))
})

test_that("weighted_hist is roughly equivalent to hist on non-weighted samples", {
  x = c(1,2,3,3,4,5,8,2,7)
  x1 = c(rep(10,20), 10.1)
  x2 = c(rep(10,1000), 10.1)

  # breaks / nclass functions match
  for (breaks in list(c("Sturges", "Sturges"), c("Scott","scott"), c("FD","FD"))) {
    breaks_fun = paste0("breaks_", breaks[[1]])
    nclass_fun = paste0("nclass.", breaks[[2]])
    expect_equal((!!breaks_fun)(x), (!!nclass_fun)(x))
    expect_equal((!!breaks_fun)(x1), (!!nclass_fun)(x1))
    expect_equal((!!breaks_fun)(x2), (!!nclass_fun)(x2))

    #partial application
    expect_equal((!!breaks_fun)()(x), (!!breaks_fun)(x))
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
    breaks_fun = paste0("breaks_", breaks[[1]])
    nclass_fun = paste0("nclass.", breaks[[2]])
    expect_equal((!!breaks_fun)(xw, w), (!!nclass_fun)(x))
  }

  # explicit breaks match
  wh = weighted_hist(xw, weights = w, breaks = c(1,3,4,7))
  wh$xname = "x"
  expect_equal(wh, hist(x, breaks = c(1,3,4,7), plot = FALSE))
})


# breaks_fixed ------------------------------------------------------------

test_that("breaks_fixed works on n = 1", {
  expect_equal(breaks_fixed(2), c(1.5, 2.5))
})


# breaks_quantiles --------------------------------------------------------

test_that("breaks_quantiles works", {
  x = c(1,2,3,3,4,5,8,2,7)
  x1 = c(rep(10,20), 10.1)
  x2 = c(rep(10,1000), 10.1)

  expect_equal(breaks_quantiles(0), 1)
  expect_equal(breaks_quantiles(c(0,0)), 1)
  expect_equal(breaks_quantiles(x, max_n = 4), quantile(x, ppoints(5, a = 1), names = FALSE))
  expect_equal(breaks_quantiles(x1), c(10, 10.1))
  expect_equal(breaks_quantiles(x2), c(10, 10.1))
})


# align functions ---------------------------------------------------------

test_that("align functions work", {
  x = c(1,2,3,4,5,6)
  breaks = c(0.25, 2.25, 4.25, 6.25)

  expect_equal(
    weighted_hist(x, breaks = breaks, align = 0.25),
    weighted_hist(x, breaks = breaks - 0.25)
  )
  expect_equal(
    weighted_hist(x, breaks = breaks, align = align_none()),
    weighted_hist(x, breaks = breaks)
  )
  expect_equal(
    weighted_hist(x, breaks = breaks, align = align_center(at = 2)),
    weighted_hist(x, breaks = breaks + 0.75)
  )
  expect_equal(
    weighted_hist(x, breaks = breaks, align = align_boundary(at = 2)),
    weighted_hist(x, breaks = breaks - 0.25)
  )
})


# argument preconditions --------------------------------------------------

test_that("align is valid", {
  expect_error(weighted_hist(1:10, align = -1), "must be between 0 and the bin width")
})

test_that("breaks are valid", {
  expect_error(weighted_hist(1:10, breaks = c(1,2)), "must\\s+cover\\s+all\\s+values")
})
