# Tests for unspread_samples
#
# Author: mjskay
###############################################################################

library(testthat)
import::from(dplyr, `%>%`, inner_join, data_frame)
import::from(purrr, map_df)
library(tidyr)
library(tidybayes)

context("unspread_samples")

test_that("unspread_samples works on a simple parameter with no indices", {
  data(RankCorr, package = "tidybayes")

  ref = data_frame(
    .chain = as.integer(1),
    .iteration = 1:nrow(RankCorr),
    typical_r = RankCorr[, "typical_r"]
  )

  RankCorr %>%
    spread_samples(typical_r) %>%
    unspread_samples(typical_r) %>%
    expect_equal(ref)

  RankCorr %>%
    spread_samples(typical_r, b[i, j]) %>%
    unspread_samples(typical_r) %>%
    expect_equal(ref)
})


test_that("unspread_samples works on a multiple parameters with different indices", {
  data(RankCorr, package = "tidybayes")

  result = RankCorr %>%
    spread_samples(b[i, j], tau[i]) %>%
    unspread_samples(b[i, j], tau[i])

  ref = RankCorr %>%
    as_sample_tibble() %>%
    select(.chain, .iteration, starts_with("b"), starts_with("tau"))

  expect_equal(result[, order(names(result))], ref[, order(names(ref))])
})
