# Tests for gather_emmeans_samples
#
# Author: mjskay
###############################################################################

library(testthat)
library(tidybayes)
suppressWarnings(suppressMessages({
  library(dplyr)
  library(tidyr)
  library(rstanarm)
  library(emmeans)
}))
import::from(magrittr, set_rownames)

context("gather_emmeans_samples")


# data
mtcars_tbl = mtcars %>%
  set_rownames(1:nrow(.)) %>%
  as_data_frame()


test_that("gather_emmeans_samples works on a simple rstanarm model", {
  m_hp_wt = readRDS("models.rstanarm.m_hp_wt.rds")

  estimate_grid = list(hp = c(100, 110), wt = 0)

  fits = posterior_linpred(m_hp_wt, newdata = as.data.frame(estimate_grid)) %>%
    as.data.frame() %>%
    mutate(
      .chain = as.integer(NA),
      .iteration = as.integer(1:n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  ref = inner_join(as_data_frame(estimate_grid) %>% mutate(.row = rownames(.)), fits, by = ".row") %>%
    select(-.row)

  # recover_data for stanreg objects seems to require the data to be in the same environment as in the
  # call that created the model (here, the global environment). So we'll put mtcars_tbl there temporarily.
  old_global_mtcars_tbl = .GlobalEnv[["mtcars_tbl"]]
  .GlobalEnv[["mtcars_tbl"]] = mtcars_tbl
  result = m_hp_wt %>%
    ref_grid(estimate_grid) %>%
    gather_emmeans_samples()
  .GlobalEnv[["mtcars_tbl"]] = old_global_mtcars_tbl

  expect_equal(ref, result)
})
