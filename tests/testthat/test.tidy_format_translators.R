# Tests for tidy format translators
#
# Author: mjskay
###############################################################################

library(dplyr)




test_that("ggmcmc translators work", {
  data("RankCorr_u_tau", package = "ggdist")

  orig = RankCorr_u_tau %>%
    dplyr::rename(.variable = i, .value = u_tau) %>%
    group_by(.variable)

  result = to_ggmcmc_names(orig)

  expect_named(result, c("Parameter", "value", "Chain", "Iteration", ".draw"))
  expect_equal(dplyr::group_vars(result), "Parameter")
  expect_equal(from_ggmcmc_names(result), orig)
})

test_that("broom translators work", {
  data("RankCorr_u_tau", package = "ggdist")

  orig = RankCorr_u_tau %>%
    dplyr::rename(.variable = i, .value = u_tau) %>%
    group_by(.variable) %>%
    median_qi(.value) %>%
    group_by(.variable)

  result = to_broom_names(orig)

  expect_named(result, c("term", "estimate", "conf.low", "conf.high", ".width", ".point", ".interval"))
  expect_equal(dplyr::group_vars(result), "term")
  expect_equal(from_broom_names(result), orig)
})
