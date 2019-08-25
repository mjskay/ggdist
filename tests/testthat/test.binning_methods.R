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


})
