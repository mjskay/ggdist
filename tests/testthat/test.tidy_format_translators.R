# Tests for tidy format translators
#
# Author: mjskay
###############################################################################

library(dplyr)

context("tidy format translators")


test_that("ggmcmc translators work", {
  data(line, package = "coda")

  orig = line %>%
    gather_draws(alpha, beta, sigma)

  result = orig %>%
    to_ggmcmc_names()

  expect_equal(names(result), c("Chain", "Iteration", ".draw", "Parameter", "value"))
  expect_equal(group_vars(result), "Parameter")
  expect_equal(from_ggmcmc_names(result), orig)
})

test_that("broom translators work", {
  data(line, package = "coda")

  orig = line %>%
    gather_draws(alpha, beta, sigma) %>%
    median_qi(.value)

  result = orig %>%
    to_broom_names()

  expect_equal(names(result), c("term", "estimate", "conf.low", "conf.high", ".width", ".point", ".interval"))
  expect_equal(from_broom_names(result), orig)
})
