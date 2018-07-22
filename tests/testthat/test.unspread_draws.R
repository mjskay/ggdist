# Tests for unspread_draws
#
# Author: mjskay
###############################################################################

import::from(dplyr, `%>%`, inner_join, data_frame)
import::from(purrr, map_df)
library(tidyr)

context("unspread_draws")


test_that("unspread_draws works on a simple parameter with no dimensions", {
  data(RankCorr, package = "tidybayes")

  ref = data_frame(
    .chain = as.integer(1),
    .iteration = seq_len(nrow(RankCorr)),
    .draw = seq_len(nrow(RankCorr)),
    typical_r = RankCorr[, "typical_r"]
  )

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
    as_sample_tibble() %>%
    select(.chain, .iteration, .draw, starts_with("b"), starts_with("tau"), starts_with("u_tau"))

  expect_equal(result[, order(names(result))], ref[, order(names(ref))])
})


test_that("unspread_draws(drop_indices = TRUE) drops draw indices", {
  data(RankCorr, package = "tidybayes")

  result = RankCorr %>%
    spread_draws(b[i, j], c(u_tau, tau)[i]) %>%
    unspread_draws(b[i, j], c(u_tau, tau)[i], drop_indices = TRUE)

  ref = RankCorr %>%
    as_sample_tibble() %>%
    select(starts_with("b"), starts_with("tau"), starts_with("u_tau"))

  expect_equal(result[, order(names(result))], ref[, order(names(ref))])
})


test_that("unspread_draws does not support wide dimension syntax (`|`)", {
  expect_error(unspread_draws(data.frame(), b[i ,j] | i),
    'unspread_draws does not support the wide dimension syntax \\(`\\|`\\).')
})
