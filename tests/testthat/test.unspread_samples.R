# Tests for unspread_samples
#
# Author: mjskay
###############################################################################

import::from(dplyr, `%>%`, inner_join, data_frame)
import::from(purrr, map_df)
library(tidyr)

context("unspread_samples")


test_that("unspread_samples works on a simple parameter with no indices", {
  data(RankCorr, package = "tidybayes")

  ref = data_frame(
    .chain = as.integer(1),
    .iteration = seq_len(nrow(RankCorr)),
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
    spread_samples(b[i, j], c(u_tau, tau)[i]) %>%
    unspread_samples(b[i, j], c(u_tau, tau)[i])

  ref = RankCorr %>%
    as_sample_tibble() %>%
    select(.chain, .iteration, starts_with("b"), starts_with("tau"), starts_with("u_tau"))

  expect_equal(result[, order(names(result))], ref[, order(names(ref))])
})


test_that("unspread_samples(drop_indices = TRUE) drops indices", {
  data(RankCorr, package = "tidybayes")

  result = RankCorr %>%
    spread_samples(b[i, j], c(u_tau, tau)[i]) %>%
    unspread_samples(b[i, j], c(u_tau, tau)[i], drop_indices = TRUE)

  ref = RankCorr %>%
    as_sample_tibble() %>%
    select(starts_with("b"), starts_with("tau"), starts_with("u_tau"))

  expect_equal(result[, order(names(result))], ref[, order(names(ref))])
})


test_that("unspread_samples does not support wide indexing syntax (`|`)", {
  expect_error(unspread_samples(data.frame(), b[i ,j] | i),
    'unspread_samples does not support the wide indexing syntax \\(`\\|`\\).')
})
