# Tests for weighted ecdfs
#
# Author: mjskay
###############################################################################


test_that("weighted_ecdf works", {
  expect_error(weighted_ecdf(NULL), "Need at least 1 or more values")

  expect_equal(weighted_ecdf(1)(0:2), c(0, 1, 1))
  expect_equal(weighted_ecdf(c(1, 2, 2, 3), 1:4)(0:4), c(0, 1, 6, 10, 10)/10)
})
