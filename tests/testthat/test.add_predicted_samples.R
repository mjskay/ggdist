# Tests for predicted_samples
#
# Author: mjskay
###############################################################################

library(testthat)
library(tidybayes)
suppressWarnings(suppressMessages({
  library(bindrcpp)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rstan)
  library(rstanarm)
}))
import::from(magrittr, set_rownames)

context("predicted_samples")


# data
mtcars_tbl = mtcars %>%
  set_rownames(1:nrow(.)) %>%
  as_data_frame()


test_that("[add_]predicted_samples and basic arguments works on a simple rstanarm model", {
  m_hp_wt = readRDS("models.rstanarm.m_hp_wt.rds")

  preds = posterior_predict(m_hp_wt, mtcars_tbl, draws = 100, seed = 123) %>%
    as.data.frame() %>%
    mutate(
      .chain = as.numeric(NA),
      .iteration = as.numeric(1:n())
    ) %>%
    gather(.row, pred, -.chain, -.iteration) %>%
    as_data_frame()

  ref = inner_join(mtcars_tbl %>% mutate(.row = rownames(.)), preds, by = ".row") %>%
    mutate(.row = factor(as.numeric(.row))) %>%
    arrange(.iteration, .row)

  expect_equal(ref, predicted_samples(m_hp_wt, mtcars_tbl, n = 100, seed = 123))
  expect_equal(ref, add_predicted_samples(mtcars_tbl, m_hp_wt, n = 100, seed = 123))
})
