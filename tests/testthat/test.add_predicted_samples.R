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
  set_rownames(seq_len(nrow(.))) %>%
  as_data_frame()


test_that("[add_]predicted_samples and basic arguments works on a simple rstanarm model", {
  m_hp_wt = readRDS("models.rstanarm.m_hp_wt.rds")

  preds = posterior_predict(m_hp_wt, mtcars_tbl, draws = 100, seed = 123) %>%
    as.data.frame() %>%
    mutate(
      .chain = as.integer(NA),
      .iteration = seq_len(n())
    ) %>%
    gather(.row, pred, -.chain, -.iteration) %>%
    as_data_frame()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(preds, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(ref, predicted_samples(m_hp_wt, mtcars_tbl, n = 100, seed = 123))
  expect_equal(ref, add_predicted_samples(mtcars_tbl, m_hp_wt, n = 100, seed = 123))
})


test_that("[add_]predicted_samples and basic arguments works on an rstanarm model with random effects", {
  m_cyl = readRDS("models.rstanarm.m_cyl.rds")

  preds = posterior_predict(m_cyl, mtcars_tbl, draws = 100, seed = 123) %>%
    as.data.frame() %>%
    mutate(
      .chain = as.integer(NA),
      .iteration = seq_len(n())
    ) %>%
    gather(.row, pred, -.chain, -.iteration) %>%
    as_data_frame()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(preds, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(ref, predicted_samples(m_cyl, mtcars_tbl, n = 100, seed = 123))
  expect_equal(ref, add_predicted_samples(mtcars_tbl, m_cyl, n = 100, seed = 123))
})


test_that("[add_]predicted_samples works on a simple brms model", {
  m_hp = readRDS("models.brms.m_hp.rds")

  set.seed(123)
  preds = predict(m_hp, mtcars_tbl, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = as.integer(NA),
      .iteration = seq_len(n())
    ) %>%
    gather(.row, pred, -.chain, -.iteration) %>%
    as_data_frame()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(preds, by = ".row") %>%
    mutate(.row = as.integer(.row))

  set.seed(123)
  expect_equal(ref, predicted_samples(m_hp, mtcars_tbl))
  set.seed(123)
  expect_equal(ref, add_predicted_samples(mtcars_tbl, m_hp, seed = 123))
})

test_that("[add_]predicted_samples throws an error when nsamples is called instead of n in brms", {
  m_hp = readRDS("models.brms.m_hp.rds")

  expect_error(m_hp %>% predicted_samples(newdata = mtcars_tbl, nsamples = 100),
               ".See the documentation for additional details.")
  expect_error(m_hp %>% add_predicted_samples(newdata = mtcars_tbl, nsamples = 100),
               ".See the documentation for additional details.")
})

test_that("[add_]predicted_samples throws an error when draws is called instead of n in rstanarm", {
  m_hp_wt = readRDS("models.rstanarm.m_hp_wt.rds")
  
  expect_error(m_hp_wt %>% predicted_samples(newdata = mtcars_tbl, draws = 100),
               ".See the documentation for additional details.")
  expect_error(m_hp_wt %>% add_predicted_samples(newdata = mtcars_tbl, draws = 100),
               ".See the documentation for additional details.")
})

test_that("[add_]predicted_samples throws an error when re.form is called instead of re_formula in rstanarm", {
  m_hp_wt = readRDS("models.rstanarm.m_hp_wt.rds")
  
  expect_error(m_hp_wt %>% predicted_samples(newdata = mtcars_tbl, re.form = NULL),
               ".See the documentation for additional details.")
  expect_error(m_hp_wt %>% add_predicted_samples(newdata = mtcars_tbl, re.form = NULL),
               ".See the documentation for additional details.")
})

