# Tests for lkj_marginal
#
# Author: mjskay
###############################################################################

library(dplyr)

context("lkj_marginal")


test_that("log of dlkjcorr_marginal is correct", {
  x = ppoints(10)

  ref = dbeta((x + 1)/2, 3, 3, log = TRUE) - log(2)

  expect_equal(dlkjcorr_marginal(x, 4, 2, log = TRUE), ref)
  expect_equal(dlkjcorr_marginal(x, 4, 2, log = TRUE), log(dlkjcorr_marginal(x, 4, 2)))
})

test_that("plkjcorr_marginal is correct", {
  x = ppoints(10)

  expect_equal(plkjcorr_marginal(x, 4, 2), pbeta((x + 1)/2, 3, 3))
  expect_equal(plkjcorr_marginal(x, 2, 4), pbeta((x + 1)/2, 4, 4))
})

test_that("rlkjcorr_marginal is correct", {
  set.seed(1234)

  ref = rbeta(10, 3, 3) * 2 - 1

  set.seed(1234)
  expect_equal(rlkjcorr_marginal(10, 4, 2), ref)
})

test_that("lkjcorr_marginal throws an error for invalid K", {
  error_regex = "Correlation matrix dimension \\(K\\) must be an integer greater than or equal to 2"

  expect_error(lkjcorr_marginal_alpha(2.01, 4), error_regex)
  expect_error(lkjcorr_marginal_alpha(1, 4), error_regex)
})
