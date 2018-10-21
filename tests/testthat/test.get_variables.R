# Tests for get_variables
#
# Author: mjskay
###############################################################################

library(dplyr)

context("get_variables")


test_that("mcmc variable extraction works", {
  data(RankCorr, package = "tidybayes")

  ref = dimnames(RankCorr[[1]])[[2]]

  tidy_draws_ref = RankCorr %>%
    tidy_draws() %>%
    names() %>%
    setdiff(c(".chain", ".iteration", ".draw"))

  expect_equal(get_variables(RankCorr), ref)
  expect_equal(get_variables(RankCorr[[1]]), ref)
  expect_equal(get_variables(RankCorr), tidy_draws_ref)
})

test_that("rstanarm variable extraction works", {
  skip_if_not_installed("rstanarm")

  m_hp_wt = readRDS("../models/models.rstanarm.m_hp_wt.rds")

  expect_equal(get_variables(m_hp_wt),
    c("(Intercept)", "hp", "wt", "hp:wt", "sigma", "accept_stat__",
      "stepsize__", "treedepth__", "n_leapfrog__", "divergent__", "energy__")
  )
})
