# Tests for binning methods
#
# Author: mjskay
###############################################################################

library(dplyr)

context("binning_methods")


test_that("binning works on symmetric distributions", {

  expect_equal(automatic_bin(0, width = .5),
    list(bins = 1, bin_midpoints = 0))

  expect_equal(automatic_bin(c(1,2,3), width = 1),
    list(bins = c(1, 2, 3), bin_midpoints = c(1, 2, 3)))

  expect_equal(automatic_bin(c(1,2,3), width = 2.01),
    list(bins = c(1, 1, 1), bin_midpoints = 2))

  expect_equal(automatic_bin(qnorm(ppoints(20, a = 1/2)), width = .5),
    list(
      bins = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 7, 7, 8),
      bin_midpoints = c(-1.95996398454005, -1.29494042565723,
        -0.766174708557979, -0.258234484056547, 0.258234484056547, 0.766174708557979,
        1.29494042565723, 1.95996398454005)
    )
  )

  expect_equal(automatic_bin(ppoints(12, a = 1/2), width = .25),
    list(bins = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4), bin_midpoints = c(0.125,
      0.375, 0.625, 0.875))
  )

  expect_equal(automatic_bin(ppoints(9, a = 1/2), width = .25),
    list(bins = c(1, 1, 1, 2, 2, 2, 3, 3, 3), bin_midpoints = c(0.166666666666667,
      0.5, 0.833333333333333))
  )

  expect_equal(automatic_bin(c(1,2,2,3,3), width = 1),
    list(bins = c(1L, 2L, 2L, 3L, 3L), bin_midpoints = c(1, 2, 3))
  )

  expect_equal(automatic_bin(c(1,2,3), width = 1),
    list(bins = c(1, 2, 3), bin_midpoints = c(1, 2, 3))
  )

  expect_equal(automatic_bin(c(1,2,2,3), width = 1),
    list(bins = c(1, 2, 2, 3), bin_midpoints = c(1, 2, 3))
  )

  expect_equal(automatic_bin(c(1,2), width = 1.01),
    list(bins = c(1, 1), bin_midpoints = 1.5)
  )

  expect_equal(automatic_bin(c(1,2), width = 1),
    list(bins = c(1, 2), bin_midpoints = c(1, 2))
  )
})

test_that("binning works on empty data", {

  expect_equal(automatic_bin(NULL, width = 1),
    list(bins = integer(0), bin_midpoints = NULL)
  )

  expect_equal(wilkinson_bin_from_center(NULL, width = 1),
    list(bins = integer(0), bin_midpoints = NULL)
  )

  expect_equal(wilkinson_bin_to_right(NULL, width = 1),
    list(bins = integer(0), bin_midpoints = NULL)
  )

  expect_equal(wilkinson_bin_to_left(NULL, width = 1),
    list(bins = integer(0), bin_midpoints = NULL)
  )
})


test_that("bin nudging works", {

  # when no nudging required
  expect_equal(
    nudge_bins(list(bins = integer(0), bin_midpoints = NULL), width = 1),
    list(bins = integer(0), bin_midpoints = NULL)
  )
  expect_equal(
    nudge_bins(list(bins = c(1), bin_midpoints = c(1)), width = 1),
    list(bins = c(1), bin_midpoints = c(1))
  )
  expect_equal(
    nudge_bins(list(bins = c(1,2), bin_midpoints = c(1,2)), width = 1),
    list(bins = c(1,2), bin_midpoints = c(1,2))
  )
  expect_equal(
    nudge_bins(list(bins = c(1,2,3,4), bin_midpoints = c(1,2,3,4)), width = 1),
    list(bins = c(1,2,3,4), bin_midpoints = c(1,2,3,4))
  )
  expect_equal(
    nudge_bins(list(bins = c(1,2,3), bin_midpoints = c(1,2,3)), width = 1),
    list(bins = c(1,2,3), bin_midpoints = c(1,2,3))
  )

  # on even number of bins
  expect_equal(
    nudge_bins(list(bins = c(1,2), bin_midpoints = c(1,2)), width = 1.1),
    list(bins = c(1,2), bin_midpoints = c(0.95,2.05))
  )
  expect_equal(
    nudge_bins(list(bins = c(1,2,3,4), bin_midpoints = c(1,2,3,4)), width = 1.1),
    list(bins = c(1,2,3,4), bin_midpoints = c(.85,1.95,3.05,4.15))
  )

  # on odd number of bins
  expect_equal(
    nudge_bins(list(bins = c(1,2,3), bin_midpoints = c(1,2,3)), width = 1.1),
    list(bins = c(1,2,3), bin_midpoints = c(0.9,2,3.1))
  )
})
