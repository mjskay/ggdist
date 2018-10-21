# Tests for gather_emmeans_draws
#
# Author: mjskay
###############################################################################

suppressWarnings(suppressMessages({
  library(dplyr)
  library(tidyr)
  library(magrittr)
}))

context("gather_emmeans_draws")


# data
mtcars_tbl = mtcars %>%
  set_rownames(seq_len(nrow(.))) %>%
  as_data_frame()


test_that("gather_emmeans_draws works on a simple rstanarm model", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("rstanarm")

  m_hp_wt = readRDS("../models/models.rstanarm.m_hp_wt.rds")

  estimate_grid = list(hp = c(100, 110), wt = 0)

  fits = rstanarm::posterior_linpred(m_hp_wt, newdata = as.data.frame(estimate_grid)) %>%
    as.data.frame() %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .value, -.chain, -.iteration, -.draw) %>%
    as_data_frame()

  ref = as_data_frame(estimate_grid) %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    select(-.row)

  # recover_data for stanreg objects seems to require the data to be in the same environment as in the
  # call that created the model (here, the global environment).
  # So we'll specify mtcars_tbl manually using `data =`
  result = m_hp_wt %>%
    emmeans::ref_grid(estimate_grid, data = mtcars_tbl) %>%
    gather_emmeans_draws()

  expect_equal(ref, result)
})
