# Tests for lkj_marginal
#
# Author: mjskay
###############################################################################

library(dplyr)

context("lkj_marginal")


test_that("log of dlkjcorr_marginal is correct", {
  x = c(0.1, 0.5, 0.9)

  ref = dbeta((x + 1)/2, 3, 3, log = TRUE) - log(2)

  expect_equal(dlkjcorr_marginal(x, 4, 2, log = TRUE), ref)
})
