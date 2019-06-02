# Tests for add_draws
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)
library(magrittr)

context("add_draws")


# data
mtcars_tbl = mtcars %>%
  set_rownames(seq_len(nrow(.))) %>%
  as_tibble()


test_that("add_draws works on fit from a simple rstanarm model", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS("../models/models.rstanarm.m_hp_wt.rds")

  fits_matrix = rstanarm::posterior_linpred(m_hp_wt, newdata = mtcars_tbl)

  fits = fits_matrix %>%
    as.data.frame() %>%
    mutate(.draw = seq_len(n())) %>%
    gather(.row, .value, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(add_draws(mtcars, fits_matrix), ref)
  expect_equal(add_draws(mtcars_tbl, fits_matrix), ref)
})
