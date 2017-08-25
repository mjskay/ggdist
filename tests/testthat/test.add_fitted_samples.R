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
}))
import::from(magrittr, set_rownames)

context("fitted_samples")


# data
mtcars_tbl = mtcars %>%
  set_rownames(1:nrow(.)) %>%
  as_data_frame()


test_that("[add_]fitted_samples works on brms models without auxpars", {
  m_hp = readRDS("models.brms.m_hp.rds")

  fits = fitted(m_hp, mtcars_tbl, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(1:ncol(.)) %>%
    mutate(
      .chain = as.numeric(NA),
      .iteration = as.numeric(1:n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  ref = inner_join(mtcars_tbl %>% mutate(.row = rownames(.)), fits, by = ".row") %>%
    mutate(.row = factor(as.numeric(.row))) %>%
    arrange(.iteration, .row)

  expect_equal(ref, fitted_samples(m_hp, mtcars_tbl))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp, auxpars = FALSE))
})


test_that("[add_]fitted_samples works on brms models with auxpars", {
  m_hp_sigma = readRDS("models.brms.m_hp_sigma.rds")

  fits = fitted(m_hp_sigma, mtcars_tbl, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(1:ncol(.)) %>%
    mutate(
      .chain = as.numeric(NA),
      .iteration = as.numeric(1:n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  fits$sigma = fits_sigma = fitted(m_hp_sigma, mtcars_tbl, summary = FALSE, dpar = "sigma") %>%
    as.data.frame() %>%
    gather(.row, sigma) %$%
    sigma

  ref = inner_join(mtcars_tbl %>% mutate(.row = rownames(.)), fits, by = ".row") %>%
    mutate(.row = factor(as.numeric(.row))) %>%
    arrange(.iteration, .row)

  fitted_samples(m_hp_sigma, mtcars_tbl, auxpars = FALSE)

  expect_equal(ref, fitted_samples(m_hp_sigma, mtcars_tbl))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp_sigma))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp_sigma, auxpars = TRUE))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_hp_sigma, auxpars = "sigma"))
  expect_equal(ref %>% select(-sigma), add_fitted_samples(mtcars_tbl, m_hp_sigma, auxpars = FALSE))
  expect_equal(ref %>% select(-sigma), add_fitted_samples(mtcars_tbl, m_hp_sigma, auxpars = NULL))
})
