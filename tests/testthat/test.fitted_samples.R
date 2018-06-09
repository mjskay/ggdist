# Tests for [add_]fitted_samples
#
# Author: mjskay
###############################################################################

suppressWarnings(suppressMessages({
  library(bindrcpp)
  library(dplyr)
  library(tidyr)
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


test_that("[add_]fitted_samples throws an error on unsupported models", {
  data("RankCorr", package = "tidybayes")

  expect_error(fitted_samples(RankCorr, data.frame()),
    'Models of type "matrix" are not currently supported by `fitted_samples`')
  expect_error(add_fitted_samples(data.frame(), RankCorr),
    'Models of type "matrix" are not currently supported by `fitted_samples`')
})


test_that("[add_]fitted_samples works on a simple rstanarm model", {
  m_hp_wt = readRDS("../models/models.rstanarm.m_hp_wt.rds")

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

  #subsetting to test the `n` argument
  set.seed(1234)
  iterations = sample(ref$.iteration, 10)
  filtered_ref = ref %>%
    filter(.iteration %in% iterations)

  set.seed(1234)
  expect_equal(fitted_samples(m_hp_wt, mtcars_tbl, n = 10), filtered_ref)
  set.seed(1234)
  expect_equal(add_fitted_samples(mtcars_tbl, m_hp_wt, n = 10), filtered_ref)
})

test_that("[add_]fitted_samples works on an rstanarm model with grouped newdata", {
  m_hp_wt = readRDS("../models/models.rstanarm.m_hp_wt.rds")

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


test_that("[add_]fitted_samples works on brms models without dpar", {
  m_hp = readRDS("../models/models.brms.m_hp.rds")

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

  expect_equal(fitted_samples(m_hp, mtcars_tbl), ref)
  expect_equal(add_fitted_samples(mtcars_tbl, m_hp), ref)
  expect_equal(add_fitted_samples(mtcars_tbl, m_hp, dpar = FALSE), ref)
})


test_that("[add_]fitted_samples works on brms models with dpar", {
  m_hp_sigma = readRDS("../models/models.brms.m_hp_sigma.rds")

  fits = fitted(m_hp_sigma, mtcars_tbl, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = as.integer(NA),
      .iteration = seq_len(n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  fits$mu = fitted(m_hp_sigma, mtcars_tbl, summary = FALSE, dpar = "mu") %>%
    as.data.frame() %>%
    gather(.row, mu) %$%
    mu

  fits$sigma = fitted(m_hp_sigma, mtcars_tbl, summary = FALSE, dpar = "sigma") %>%
    as.data.frame() %>%
    gather(.row, sigma) %$%
    sigma

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(fitted_samples(m_hp_sigma, mtcars_tbl, dpar = TRUE), ref)
  expect_equal(add_fitted_samples(mtcars_tbl, m_hp_sigma, dpar = TRUE), ref)
  expect_equal(add_fitted_samples(mtcars_tbl, m_hp_sigma, dpar = "sigma"), select(ref, -mu))
  expect_equal(add_fitted_samples(mtcars_tbl, m_hp_sigma, dpar = "mu"), select(ref, -sigma))
  expect_equal(add_fitted_samples(mtcars_tbl, m_hp_sigma, dpar = FALSE), select(ref, -sigma, -mu))
  expect_equal(add_fitted_samples(mtcars_tbl, m_hp_sigma, dpar = NULL), select(ref, -sigma, -mu))
  expect_equal(add_fitted_samples(mtcars_tbl, m_hp_sigma, dpar = list("mu", "sigma", s1 = "sigma")), mutate(ref, s1 = sigma))
})


test_that("[add_]fitted_samples works on simple brms models with nlpars", {
  m_nlpar = readRDS("../models/models.brms.m_nlpar.rds")
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

  expect_equal(fitted_samples(m_nlpar, df_nlpar), ref)
  expect_equal(add_fitted_samples(df_nlpar, m_nlpar), ref)
  expect_equal(add_fitted_samples(df_nlpar, m_nlpar, dpar = FALSE), ref)
})


test_that("[add_]fitted_samples works on simple brms models with multiple dpars", {
  m_dpars = readRDS("../models/models.brms.m_dpars.rds")
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

  expect_equal(fitted_samples(m_dpars, df_dpars, dpar = TRUE), ref)
  expect_equal(add_fitted_samples(df_dpars, m_dpars, dpar = list("mu1", "mu2")), ref)
  expect_equal(add_fitted_samples(df_dpars, m_dpars, dpar = FALSE), select(ref, -mu1, -mu2))
})


test_that("[add_]fitted_samples works on brms models with categorical outcomes (response scale)", {
  m_cyl_mpg = readRDS("../models/models.brms.m_cyl_mpg.rds")

  fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE) %>%
    array2df(list(.iteration = NA, .row = NA, category = NA), label.x = "estimate") %>%
    mutate(
      .chain = as.integer(NA),
      .row = as.integer(.row),
      .iteration = as.integer(.iteration),
      category = factor(category)
    )

  ref = inner_join(mtcars_tbl %>% mutate(.row = as.integer(rownames(.))), fits, by = ".row")

  expect_equal(fitted_samples(m_cyl_mpg, mtcars_tbl), ref)
  expect_equal(add_fitted_samples(mtcars_tbl, m_cyl_mpg), ref)
})


test_that("[add_]fitted_samples works on brms models with categorical outcomes (linear scale)", {
  m_cyl_mpg = readRDS("../models/models.brms.m_cyl_mpg.rds")

  fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE, scale = "linear") %>%
    array2df(list(.iteration = NA, .row = NA, category = NA), label.x = "estimate") %>%
    mutate(
      .chain = as.integer(NA),
      .row = as.integer(.row),
      .iteration = as.integer(.iteration),
      category = factor(category)
    )

  ref = inner_join(mtcars_tbl %>% mutate(.row = as.integer(rownames(.))), fits, by = ".row")

  expect_equal(fitted_samples(m_cyl_mpg, mtcars_tbl, scale = "linear"), ref)
  expect_equal(add_fitted_samples(mtcars_tbl, m_cyl_mpg, scale = "linear"), ref)
})


# test_that("[add_]fitted_samples allows extraction of dpar on brms models with categorical outcomes", {
#   m_cyl_mpg = readRDS("../models/models.brms.m_cyl_mpg.rds")
#
#   fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE, scale = "linear") %>%
#     array2df(list(.iteration = NA, .row = NA, category = NA), label.x = "estimate") %>%
#     mutate(
#       .chain = as.integer(NA),
#       .row = as.integer(.row),
#       .iteration = as.integer(.iteration),
#       category = factor(category)
#     )
#
#   ref = inner_join(mtcars_tbl %>% mutate(.row = as.integer(rownames(.))), fits, by = ".row")
#
#   expect_equal(fitted_samples(m_cyl_mpg, mtcars_tbl, dpar = TRUE), ref)
#   expect_equal(add_fitted_samples(mtcars_tbl, m_cyl_mpg, dpar = TRUE), ref)
# })


test_that("[add_]fitted_samples throws an error when nsamples is called instead of n in brms", {
  m_hp = readRDS("../models/models.brms.m_hp.rds")

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
  m_hp_wt = readRDS("../models/models.rstanarm.m_hp_wt.rds")

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
  m_hp_wt = readRDS("../models/models.rstanarm.m_hp_wt.rds")

  expect_error(
    m_hp_wt %>% fitted_samples(newdata = mtcars_tbl, transform = TRUE),
    "`transform.*.`scale`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp_wt %>% add_fitted_samples(newdata = mtcars_tbl, transform = TRUE),
    "`transform.*.`scale`.*.See the documentation for additional details."
  )
})
