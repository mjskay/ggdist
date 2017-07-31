# Tests for ungather_samples
#
# Author: mjskay
###############################################################################

library(testthat)
import::from(dplyr, `%>%`, inner_join, data_frame)
import::from(purrr, map_df)
library(tidyr)
library(tidybayes)

context("ungather_samples")

test_that("ungather_samples works on a simple parameter with no indices", {
  data(RankCorr, package = "tidybayes")

  ref = data_frame(
    .chain = as.integer(1),
    .iteration = 1:nrow(RankCorr),
    typical_r = RankCorr[, "typical_r"]
  )

  RankCorr %>%
    gather_samples(typical_r) %>%
    ungather_samples(typical_r) %>%
    expect_equal(ref)

  RankCorr %>%
    gather_samples(typical_r, b[i, j]) %>%
    ungather_samples(typical_r) %>%
    expect_equal(ref)
})


test_that("ungather_samples works on multiple parameters with different indices", {
  data(RankCorr, package = "tidybayes")

  result = RankCorr %>%
    gather_samples(b[i, j], c(u_tau, tau)[i]) %>%
    ungather_samples(b[i, j], c(u_tau, tau)[i])

  ref = RankCorr %>%
    as_sample_tibble() %>%
    select(.chain, .iteration, starts_with("b"), starts_with("tau"), starts_with("u_tau"))

  expect_equal(result[, order(names(result))], ref[, order(names(ref))])
})


test_that("ungather_samples(drop_indices = TRUE) drops indices", {
  data(RankCorr, package = "tidybayes")

  result = RankCorr %>%
    gather_samples(b[i, j], c(u_tau, tau)[i]) %>%
    ungather_samples(b[i, j], c(u_tau, tau)[i], drop_indices = TRUE)

  ref = RankCorr %>%
    as_sample_tibble() %>%
    select(starts_with("b"), starts_with("tau"), starts_with("u_tau"))

  expect_equal(result[, order(names(result))], ref[, order(names(ref))])
})
