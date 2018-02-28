# Tests for gather_samples
#
# Author: mjskay
###############################################################################

import::from(dplyr, `%>%`, inner_join, data_frame)
import::from(purrr, map_df)
library(tidyr)

context("gather_samples")


test_that("regular expressions for parameter names work on non-indexed parameters", {
  data(RankCorr, package = "tidybayes")

  ref = gather_samples(RankCorr, typical_r)

  expect_equal(gather_samples(RankCorr, `typical..`, regex = TRUE), ref)
})

test_that("regular expressions for parameter names work on indexed parameters", {
  data(RankCorr, package = "tidybayes")

  ref = gather_samples(RankCorr, c(tau, u_tau)[i])

  expect_equal(gather_samples(RankCorr, `.*tau`[i], regex = TRUE), ref)
})
