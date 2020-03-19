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
  as_tibble()


test_that("gather_emmeans_draws works on a simple rstanarm model", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("rstanarm")

  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  estimate_grid = list(hp = c(100, 110), wt = 0)

  fits = rstanarm::posterior_linpred(m_hp_wt, newdata = as.data.frame(estimate_grid)) %>%
    as.data.frame() %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .value, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = as_tibble(estimate_grid) %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    select(-.row) %>%
    group_by(hp, wt)

  # recover_data for stanreg objects seems to require the data to be in the same environment as in the
  # call that created the model (here, the global environment).
  # So we'll specify mtcars_tbl manually using `data =`
  grid = emmeans::ref_grid(m_hp_wt, estimate_grid, data = mtcars_tbl)

  expect_equal(gather_emmeans_draws(grid), ref)
  expect_equal(gather_emmeans_draws(grid, value = "v"), ref %>% rename(v = ".value"))
})


test_that("gather_emmeans_draws works on an emm_list", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("rstanarm")

  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  estimate_grid = list(hp = c(100, 110, 120), wt = 0)

  grid_list = emmeans::ref_grid(m_hp_wt, estimate_grid, data = mtcars_tbl) %>%
    emmeans::emmeans(pairwise ~ hp|wt)

  ref = map_dfr(grid_list, ~ gather_emmeans_draws(.x) %>%
      ungroup() %>%
      mutate_at(vars(matches("contrast")), as.character),
    .id = ".grid"
    ) %>%
    group_by(hp, wt, contrast, .grid)

  expect_equal(gather_emmeans_draws(grid_list), ref)
  expect_equal(gather_emmeans_draws(grid_list, value = "v", grid = "g"),
    ref %>% rename(g = .grid, v = .value) %>% group_by(hp, wt, contrast, g))
})
