# Tests for fitted_samples
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
  library(brms)
  library(arrayhelpers)
}))
import::from(magrittr, set_rownames)

context("fitted_samples")


# data
mtcars_tbl = mtcars %>%
  set_rownames(seq_len(nrow(.))) %>%
  as_data_frame()


test_that("[add_]fitted_samples works on a simple rstanarm model", {
  m_hp_wt = readRDS("models.rstanarm.m_hp_wt.rds")

  fits = posterior_linpred(m_hp_wt, newdata = mtcars_tbl) %>%
    as.data.frame() %>%
    mutate(
      .chain = as.integer(NA),
      .iteration = seq_len(n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(ref, fitted_samples(m_hp_wt, mtcars_tbl))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp_wt))
})

test_that("[add_]fitted_samples works on an rstanarm model with grouped newdata", {
  m_hp_wt = readRDS("models.rstanarm.m_hp_wt.rds")

  fits = posterior_linpred(m_hp_wt, newdata = mtcars_tbl) %>%
    as.data.frame() %>%
    mutate(
      .chain = as.integer(NA),
      .iteration = seq_len(n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(ref, fitted_samples(m_hp_wt, group_by(mtcars_tbl, hp)))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp_wt))
})


test_that("[add_]fitted_samples works on brms models without auxpars", {
  m_hp = readRDS("models.brms.m_hp.rds")

  fits = fitted(m_hp, mtcars_tbl, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = as.integer(NA),
      .iteration = seq_len(n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(ref, fitted_samples(m_hp, mtcars_tbl))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp, auxpars = FALSE))
})


test_that("[add_]fitted_samples works on brms models with auxpars", {
  m_hp_sigma = readRDS("models.brms.m_hp_sigma.rds")

  fits = fitted(m_hp_sigma, mtcars_tbl, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = as.integer(NA),
      .iteration = seq_len(n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  fits$sigma = fits_sigma = fitted(m_hp_sigma, mtcars_tbl, summary = FALSE, dpar = "sigma") %>%
    as.data.frame() %>%
    gather(.row, sigma) %$%
    sigma

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row))

  fitted_samples(m_hp_sigma, mtcars_tbl, auxpars = FALSE)

  expect_equal(ref, fitted_samples(m_hp_sigma, mtcars_tbl))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp_sigma))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp_sigma, auxpars = TRUE))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp_sigma, auxpars = "sigma"))
  expect_equal(ref %>% select(-sigma), add_fitted_samples(mtcars_tbl, m_hp_sigma, auxpars = FALSE))
  expect_equal(ref %>% select(-sigma), add_fitted_samples(mtcars_tbl, m_hp_sigma, auxpars = NULL))
})


test_that("[add_]fitted_samples works on brms models with categorical outcomes (response scale)", {
  m_cyl_mpg = readRDS("models.brms.m_cyl_mpg.rds")

  category_names = dimnames(fitted(m_cyl_mpg, mtcars_tbl, summary = TRUE))[[3]]
  fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE) %>%
    array2df(list(.iteration = NA, .row = NA, category = category_names), label.x = "estimate") %>%
    mutate(
      .chain = as.integer(NA),
      .row = as.integer(.row),
      .iteration = as.integer(.iteration)
    )

  ref = inner_join(mtcars_tbl %>% mutate(.row = as.integer(rownames(.))), fits, by = ".row")

  expect_equal(ref, fitted_samples(m_cyl_mpg, mtcars_tbl))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_cyl_mpg))
})


test_that("[add_]fitted_samples works on brms models with categorical outcomes (linear scale)", {
  m_cyl_mpg = readRDS("models.brms.m_cyl_mpg.rds")

  category_names = dimnames(fitted(m_cyl_mpg, mtcars_tbl, summary = TRUE, scale = "linear"))[[3]]
  fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE, scale = "linear") %>%
    array2df(list(.iteration = NA, .row = NA, category = category_names), label.x = "estimate") %>%
    mutate(
      .chain = as.integer(NA),
      .row = as.integer(.row),
      .iteration = as.integer(.iteration)
    )

  ref = inner_join(mtcars_tbl %>% mutate(.row = as.integer(rownames(.))), fits, by = ".row")

  expect_equal(ref, fitted_samples(m_cyl_mpg, mtcars_tbl, scale = "linear"))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_cyl_mpg, scale = "linear"))
})

test_that("[add_]predicted_samples gives same results with standardized arguments and prediction method arguments in brms", {
  m_hp = readRDS("models.brms.m_hp.rds")
  
  set.seed(123)
  std_args_fit = m_hp %>%
    fitted_samples(newdata = mtcars_tbl, n = 100)
  set.seed(123)
  std_args_add = m_hp %>%
    add_fitted_samples(newdata = mtcars_tbl, n = 100)
  
  set.seed(123)
  predict_args_fit = m_hp %>%
    fitted_samples(newdata = mtcars_tbl, nsamples = 100)
  set.seed(123)
  predict_args_add = m_hp %>%
    add_fitted_samples(newdata = mtcars_tbl, nsamples = 100)
  
  expect_equal(nrow(std_args_fit), nrow(predict_args_fit))
  expect_equal(std_args_fit, predict_args_fit)
  
  expect_equal(nrow(std_args_add), nrow(predict_args_add))
  expect_equal(std_args_add, predict_args_add)
})

test_that("[add_]predicted_samples gives same results with standardized arguments and prediction method arguments in rstanarm", {
  m_hp_wt = readRDS("models.rstanarm.m_hp_wt.rds")
  
  set.seed(123)
  std_args_fit = m_hp_wt %>%
    fitted_samples(newdata = mtcars_tbl, n = 100)
  set.seed(123)
  std_args_add = m_hp_wt %>%
    add_fitted_samples(newdata = mtcars_tbl, n = 100)
  
  set.seed(123)
  predict_args_fit = m_hp_wt %>%
    fitted_samples(newdata = mtcars_tbl, nsamples = 100)
  set.seed(123)
  predict_args_add = m_hp_wt %>%
    add_fitted_samples(newdata = mtcars_tbl, nsamples = 100)
  
  expect_equal(nrow(std_args_fit), nrow(predict_args_fit))
  expect_equal(std_args_fit, predict_args_fit)
  
  expect_equal(nrow(std_args_add), nrow(predict_args_add))
  expect_equal(std_args_add, predict_args_add)
})

