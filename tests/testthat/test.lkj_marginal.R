# Tests for lkj_marginal
#
# Author: mjskay
###############################################################################

library(dplyr)
library(distributional)



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

test_that("marginalize_lkjcorr works", {
  ref = as.data.frame(tibble(
    coef = c("a", "b"),
    prior = c("lkjcorr(3)", "lkjcorr(3)"),
    .dist = c("lkjcorr_marginal", "lkjcorr_marginal"),
    .args = list(list(2, 3), list(4, 3)),
    .dist_obj = dist_wrap(dist = "lkjcorr_marginal", c(2, 4), c(3, 3), package = "ggdist")
  ))

  expect_equal(
    data.frame(coef = c("a", "b"), prior = "lkjcorr(3)", stringsAsFactors = FALSE) %>%
      parse_dist(prior) %>%
      marginalize_lkjcorr(K = 2, coef == "a") %>%
      marginalize_lkjcorr(K = 4, coef == "b"),
    ref
  )
})
