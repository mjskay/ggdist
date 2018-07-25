# Tests for gather_variables
#
# Author: mjskay
###############################################################################

import::from(dplyr, `%>%`, group_by)
library(tidyr)

context("gather_variables")


test_that("gather_variables works on the results of tidy_draws", {
  data(RankCorr, package = "tidybayes")

  ref = RankCorr %>%
    tidy_draws() %>%
    gather(.variable, .value, -.chain, -.iteration, -.draw) %>%
    group_by(.variable, add = TRUE)

  result = RankCorr %>%
    tidy_draws() %>%
    gather_variables()

  expect_equal(result, ref)
  expect_equal(group_vars(result), group_vars(ref))
})


test_that("gather_variables works on the results of spread_draws with multiple variables and dimensions", {
  data(RankCorr, package = "tidybayes")

  ref = RankCorr %>%
    spread_draws(b[i, v], tau[i]) %>%
    gather(.variable, .value, -.chain, -.iteration, -.draw, -i, -v) %>%
    group_by(i, v, .variable)

  result = RankCorr %>%
    spread_draws(b[i, v], tau[i]) %>%
    gather_variables()

  expect_equal(result, ref)
  expect_equal(group_vars(result), group_vars(ref))
})
