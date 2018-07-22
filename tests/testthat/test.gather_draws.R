# Tests for gather_draws
#
# Author: mjskay
###############################################################################

import::from(dplyr, `%>%`, inner_join, data_frame)
import::from(purrr, map_df)
library(tidyr)

context("gather_draws")


test_that("regular expressions for parameter names work on non-indexed parameters", {
  data(RankCorr, package = "tidybayes")

  ref = gather_draws(RankCorr, typical_r)

  expect_equal(gather_draws(RankCorr, `typical..`, regex = TRUE), ref)
})

test_that("regular expressions for parameter names work on indexed parameters", {
  data(RankCorr, package = "tidybayes")

  ref = gather_draws(RankCorr, c(tau, u_tau)[i])

  expect_equal(gather_draws(RankCorr, `.*tau`[i], regex = TRUE), ref)
})
