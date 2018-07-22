# Tests for gather_terms
#
# Author: mjskay
###############################################################################

import::from(dplyr, `%>%`, group_by)
library(tidyr)

context("gather_terms")


test_that("gather_terms works on the results of as_sample_tibble", {
  data(RankCorr, package = "tidybayes")

  ref = RankCorr %>%
    as_sample_tibble() %>%
    gather(term, estimate, -.chain, -.iteration, -.draw) %>%
    group_by(term, add = TRUE)

  result = RankCorr %>%
    as_sample_tibble() %>%
    gather_terms()

  expect_equal(result, ref)
})


test_that("gather_terms works on the results of spread_draws with multiple params and indices", {
  data(RankCorr, package = "tidybayes")

  ref = RankCorr %>%
    spread_draws(b[i, v], tau[i]) %>%
    gather(term, estimate, -.chain, -.iteration, -.draw, -i, -v) %>%
    group_by(term, add = TRUE)

  result = RankCorr %>%
    spread_draws(b[i, v], tau[i]) %>%
    gather_terms()

  expect_equal(result, ref)
})
