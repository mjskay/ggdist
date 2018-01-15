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


test_that("[add_]fitted_samples throws an when dpars is called instead of auxpars on brms models with auxpars", {
  m_hp_sigma = readRDS("models.brms.m_hp_sigma.rds")

  expect_error(
    fitted_samples(m_hp_sigma, mtcars_tbl, dpars = "sigma"),
    "`dpars.*.`auxpars`.*.See the documentation for additional details."
  )
  expect_error(
    add_fitted_samples(mtcars_tbl, m_hp_sigma, dpars = "sigma"),
    "`dpars.*.`auxpars`.*.See the documentation for additional details."
  )
})


test_that("[add_]fitted_samples works on simple brms models with nlpars", {
  m_nlpar = readRDS("models.brms.m_nlpar.rds")
  df_nlpar = as_data_frame(m_nlpar$data)

  fits = fitted(m_nlpar, df_nlpar, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = as.integer(NA),
      .iteration = seq_len(n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  ref = df_nlpar %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(ref, fitted_samples(m_nlpar, df_nlpar))
  expect_equal(ref, add_fitted_samples(df_nlpar, m_nlpar))
  expect_equal(ref, add_fitted_samples(df_nlpar, m_nlpar, auxpars = FALSE))
})


test_that("[add_]fitted_samples works on simple brms models with multiple dpars", {
  m_dpars = readRDS("models.brms.m_dpars.rds")
  df_dpars = as_data_frame(m_dpars$data)

  fits = fitted(m_dpars, df_dpars, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = as.integer(NA),
      .iteration = seq_len(n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  fits$mu1 = fitted(m_dpars, df_dpars, summary = FALSE, dpar = "mu1") %>%
    as.data.frame() %>%
    gather(.row, mu1) %$%
    mu1

  fits$mu2 = fitted(m_dpars, df_dpars, summary = FALSE, dpar = "mu2") %>%
    as.data.frame() %>%
    gather(.row, mu2) %$%
    mu2

  ref = df_dpars %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(ref, fitted_samples(m_dpars, df_dpars))
  expect_equal(ref, add_fitted_samples(df_dpars, m_dpars))
  expect_equal(ref %>% select(-mu1, -mu2), add_fitted_samples(df_dpars, m_dpars, auxpars = FALSE))
})


test_that("[add_]fitted_samples works on brms models with categorical outcomes (response scale)", {
  m_cyl_mpg = readRDS("models.brms.m_cyl_mpg.rds")

  fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE) %>%
    array2df(list(.iteration = NA, .row = NA, category = NA), label.x = "estimate") %>%
    mutate(
      .chain = as.integer(NA),
      .row = as.integer(.row),
      .iteration = as.integer(.iteration),
      category = factor(category)
    )

  ref = inner_join(mtcars_tbl %>% mutate(.row = as.integer(rownames(.))), fits, by = ".row")

  expect_equal(ref, fitted_samples(m_cyl_mpg, mtcars_tbl))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_cyl_mpg))
})


test_that("[add_]fitted_samples works on brms models with categorical outcomes (linear scale)", {
  m_cyl_mpg = readRDS("models.brms.m_cyl_mpg.rds")

  fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE, scale = "linear") %>%
    array2df(list(.iteration = NA, .row = NA, category = NA), label.x = "estimate") %>%
    mutate(
      .chain = as.integer(NA),
      .row = as.integer(.row),
      .iteration = as.integer(.iteration),
      category = factor(category)
    )

  ref = inner_join(mtcars_tbl %>% mutate(.row = as.integer(rownames(.))), fits, by = ".row")

  expect_equal(ref, fitted_samples(m_cyl_mpg, mtcars_tbl, scale = "linear"))
  expect_equal(ref, add_fitted_samples(mtcars_tbl, m_cyl_mpg, scale = "linear"))
})


test_that("[add_]fitted_samples throws an error when nsamples is called instead of n in brms", {
  m_hp = readRDS("models.brms.m_hp.rds")

  expect_error(
    m_hp %>% fitted_samples(newdata = mtcars_tbl, nsamples = 100),
    "`nsamples.*.`n`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp %>% add_fitted_samples(newdata = mtcars_tbl, nsamples = 100),
    "`nsamples.*.`n`.*.See the documentation for additional details."
  )
})

# rstanarm doesn't have a draws method for fitted samples

test_that("[add_]predicted_samples throws an error when re.form is called instead of re_formula in rstanarm", {
  m_hp_wt = readRDS("models.rstanarm.m_hp_wt.rds")

  expect_error(
    m_hp_wt %>% fitted_samples(newdata = mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp_wt %>% add_fitted_samples(newdata = mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
})

test_that("[add_]predicted_samples throws an error when transform is called instead of scale in rstanarm", {
  m_hp_wt = readRDS("models.rstanarm.m_hp_wt.rds")

  expect_error(
    m_hp_wt %>% fitted_samples(newdata = mtcars_tbl, transform = TRUE),
    "`transform.*.`scale`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp_wt %>% add_fitted_samples(newdata = mtcars_tbl, transform = TRUE),
    "`transform.*.`scale`.*.See the documentation for additional details."
  )
})
