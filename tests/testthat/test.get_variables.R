# Tests for parameters
#
# Author: mjskay
###############################################################################

library(dplyr)

context("get_variables")


test_that("mcmc variable extraction works", {
  data(RankCorr, package = "tidybayes")

  ref = dimnames(RankCorr[[1]])[[2]]

  tidy_draws_ref = RankCorr %>%
    tidy_draws() %>%
    names() %>%
    setdiff(c(".chain", ".iteration", ".draw"))

  expect_equal(get_variables(RankCorr), ref)
  expect_equal(get_variables(RankCorr[[1]]), ref)
  expect_equal(get_variables(RankCorr), tidy_draws_ref)
})
