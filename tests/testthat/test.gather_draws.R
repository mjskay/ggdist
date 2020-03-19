# Tests for gather_draws
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)

context("gather_draws")


test_that("regular expressions for parameter names work on non-indexed parameters", {
  data(RankCorr, package = "tidybayes")

  ref = RankCorr %>% spread_draws(typical_r) %>% gather_variables()

  expect_equal(gather_draws(RankCorr, `typical..`, regex = TRUE), ref)
})

test_that("regular expressions for parameter names work on indexed parameters", {
  data(RankCorr, package = "tidybayes")

  ref = RankCorr %>% spread_draws(c(tau, u_tau)[i]) %>% gather_variables()

  expect_equal(gather_draws(RankCorr, `.*tau`[i], regex = TRUE), ref)
})

test_that("gather_draws works on a combination of 0 and 1-dimensional values (with correct groups)", {
  data(RankCorr, package = "tidybayes")

  ref = RankCorr %>%
    spread_draws(tau[i], typical_r) %>%
    gather(".variable", ".value", -one_of(".chain", ".iteration", ".draw", "i")) %>%
    ungroup() %>%
    filter(.variable != "typical_r" | (.variable == "typical_r" & i == 1))
  ref[ref$.variable == "typical_r", "i"] = NA
  ref = group_by(ref, i, .variable, .drop = FALSE)

  result = gather_draws(RankCorr, tau[i], typical_r)

  expect_equal(result, ref)
  expect_equal(group_vars(result), group_vars(ref))
})
