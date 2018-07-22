# Tests for tidy format translators
#
# Author: mjskay
###############################################################################

library(dplyr)

context("tidy format translators")


test_that("basic translators work", {
  result = line %>%
    gather_draws(alpha, beta, sigma) %>%
    from_broom_names() %>% #TODO: drop once names are fixed
    to_ggmcmc_names()

  expect_equal(names(result), c("Chain", "Iteration", ".draw", "Parameter", "value"))
  expect_equal(group_vars(result), "Parameter")
})
