# Tests for unspread_draws
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)

context("unspread_draws")


test_that("unspread_draws works on a simple parameter with no dimensions", {
  data(RankCorr, package = "tidybayes")

  ref = data_frame(
      .chain = as.integer(1),
      .iteration = seq_len(nrow(RankCorr[[1]])),
      .draw = seq_len(nrow(RankCorr[[1]])),
      typical_r = as.vector(RankCorr[[1]][, "typical_r"])
    ) %>%
    bind_rows(data_frame(
      .chain = as.integer(2),
      .iteration = seq_len(nrow(RankCorr[[2]])),
      .draw = nrow(RankCorr[[2]]) + seq_len(nrow(RankCorr[[2]])),
      typical_r = as.vector(RankCorr[[2]][, "typical_r"])
    ))

  RankCorr %>%
    spread_draws(typical_r) %>%
    unspread_draws(typical_r) %>%
    expect_equal(ref)

  RankCorr %>%
    spread_draws(typical_r, b[i, j]) %>%
    unspread_draws(typical_r) %>%
    expect_equal(ref)
})


test_that("unspread_draws works on a multiple parameters with different dimensions", {
  data(RankCorr, package = "tidybayes")

  result = RankCorr %>%
    spread_draws(b[i, j], c(u_tau, tau)[i]) %>%
    unspread_draws(b[i, j], c(u_tau, tau)[i])

  ref = RankCorr %>%
    tidy_draws() %>%
    select(.chain, .iteration, .draw, starts_with("b"), starts_with("tau"), starts_with("u_tau"))

  expect_equal(result[, order(names(result))], ref[, order(names(ref))])
})


test_that("unspread_draws(drop_indices = TRUE) drops draw indices", {
  data(RankCorr, package = "tidybayes")

  result = RankCorr %>%
    spread_draws(b[i, j], c(u_tau, tau)[i]) %>%
    unspread_draws(b[i, j], c(u_tau, tau)[i], drop_indices = TRUE)

  ref = RankCorr %>%
    tidy_draws() %>%
    select(starts_with("b"), starts_with("tau"), starts_with("u_tau"))

  expect_equal(result[, order(names(result))], ref[, order(names(ref))])
})


test_that("unspread_draws does not support wide dimension syntax (`|`)", {
  expect_error(unspread_draws(data.frame(), b[i ,j] | i),
    'unspread_draws does not support the wide dimension syntax \\(`\\|`\\).')
})
