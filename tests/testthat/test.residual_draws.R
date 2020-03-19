# Tests for [add_]residual_draws
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)
library(magrittr)

context("residual_draws")


# data
mtcars_tbl = mtcars %>%
  set_rownames(seq_len(nrow(.))) %>%
  as_tibble()


test_that("[add_]residual_draws throws error on unsupported models", {
  expect_error(residual_draws(list()), "Models of type \"list\" are not currently supported by `residual_draws`")
})

test_that("[add_]residual_draws works on a simple brms model", {
  skip_if_not_installed("brms")
  m_hp = readRDS(test_path("../models/models.brms.m_hp.rds"))

  resids = residuals(m_hp, mtcars_tbl, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .residual, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(resids, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(residual_draws(m_hp, mtcars_tbl), ref)
  expect_equal(add_residual_draws(mtcars_tbl, m_hp), ref)
})
