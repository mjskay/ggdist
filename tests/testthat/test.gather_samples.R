# Tests for gather_samples
#
# Author: mjskay
###############################################################################

library(testthat)
import::from(dplyr, `%>%`, inner_join, data_frame)
import::from(purrr, map_df)
library(tidyr)
library(tidybayes)

context("gather_samples")

test_that("regular expressions for parameter names work on non-indexed parameters", {
  data(RankCorr, package = "tidybayes")

  expect_equal(gather_samples(RankCorr, typical_r), gather_samples(RankCorr, `typical..`))
})

test_that("regular expressions for parameter names work on indexed parameters", {
  data(RankCorr, package = "tidybayes")

  expect_equal(gather_samples(RankCorr, c(tau, u_tau)[i]), gather_samples(RankCorr, `.*tau`[i]))
})
