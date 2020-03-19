# Tests for [add_]fitted_draws
#
# Author: mjskay
###############################################################################

suppressWarnings(suppressMessages({
  library(dplyr)
  library(tidyr)
  library(arrayhelpers)
  library(magrittr)
}))

context("fitted_draws")


# data
mtcars_tbl = mtcars %>%
  set_rownames(seq_len(nrow(.))) %>%
  as_tibble()


test_that("[add_]fitted_draws throws an error on unsupported models", {
  data("RankCorr", package = "tidybayes")

  expect_error(fitted_draws(RankCorr, data.frame()),
    'Models of type "mcmc.list" are not currently supported by `fitted_draws`')
  expect_error(add_fitted_draws(data.frame(), RankCorr),
    'Models of type "mcmc.list" are not currently supported by `fitted_draws`')
})


test_that("[add_]fitted_draws works on a simple rstanarm model", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  fits = rstanarm::posterior_linpred(m_hp_wt, newdata = mtcars_tbl) %>%
    as.data.frame() %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .value, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(fitted_draws(m_hp_wt, mtcars_tbl), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp_wt), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp_wt, value = "foo"), rename(ref, foo = .value))
  expect_equal(linpred_draws(m_hp_wt, mtcars_tbl), ref)
  expect_equal(add_linpred_draws(mtcars_tbl, m_hp_wt), ref)

  #subsetting to test the `n` argument
  set.seed(1234)
  draw_subset = sample(unique(ref$.draw), 10)
  filtered_ref = ref %>%
    filter(.draw %in% draw_subset)

  expect_equal(fitted_draws(m_hp_wt, mtcars_tbl, n = 10, seed = 1234), filtered_ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp_wt, n = 10, seed = 1234), filtered_ref)
})

test_that("[add_]fitted_draws works on an rstanarm model with grouped newdata", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  fits = rstanarm::posterior_linpred(m_hp_wt, newdata = mtcars_tbl) %>%
    as.data.frame() %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .value, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(fitted_draws(m_hp_wt, group_by(mtcars_tbl, hp)), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp_wt), ref)
})


test_that("[add_]fitted_draws works on brms models without dpar", {
  skip_if_not_installed("brms")
  m_hp = readRDS(test_path("../models/models.brms.m_hp.rds"))

  fits = fitted(m_hp, mtcars_tbl, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .value, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(fitted_draws(m_hp, mtcars_tbl), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp, dpar = FALSE), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp, dpar = FALSE, value = "foo"), rename(ref, foo = .value))

  #subsetting to test the `n` argument
  set.seed(1234)
  draw_subset = sample(unique(ref$.draw), 10)
  filtered_ref = ref %>%
    filter(.draw %in% draw_subset)

  expect_equal(add_fitted_draws(mtcars_tbl, m_hp, n = 10, seed = 1234), filtered_ref)
})


test_that("[add_]fitted_draws works on brms models with dpar", {
  skip_if_not_installed("brms")
  m_hp_sigma = readRDS(test_path("../models/models.brms.m_hp_sigma.rds"))

  fits = fitted(m_hp_sigma, mtcars_tbl, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .value, -.chain, -.iteration, -.draw) %>%
    as_tibble()

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
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(fitted_draws(m_hp_sigma, mtcars_tbl, dpar = TRUE), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp_sigma, dpar = TRUE), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp_sigma, dpar = "sigma"), select(ref, -mu))
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp_sigma, dpar = "mu"), select(ref, -sigma))
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp_sigma, dpar = FALSE), select(ref, -sigma, -mu))
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp_sigma, dpar = NULL), select(ref, -sigma, -mu))
  expect_equal(add_fitted_draws(mtcars_tbl, m_hp_sigma, dpar = list("mu", "sigma", s1 = "sigma")), mutate(ref, s1 = sigma))


  #subsetting to test the `n` argument
  set.seed(1234)
  draw_subset = sample(unique(ref$.draw), 10)
  filtered_ref = ref %>%
    filter(.draw %in% draw_subset)

  expect_equal(add_fitted_draws(mtcars_tbl, m_hp_sigma, n = 10, seed = 1234, dpar = TRUE), filtered_ref)
})


test_that("[add_]fitted_draws works on simple brms models with nlpars", {
  skip_if_not_installed("brms")
  m_nlpar = readRDS(test_path("../models/models.brms.m_nlpar.rds"))
  df_nlpar = as_tibble(m_nlpar$data)

  fits = fitted(m_nlpar, df_nlpar, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .value, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = df_nlpar %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(y, x, .row)

  expect_equal(fitted_draws(m_nlpar, df_nlpar), ref)
  expect_equal(add_fitted_draws(df_nlpar, m_nlpar), ref)
  expect_equal(add_fitted_draws(df_nlpar, m_nlpar, dpar = FALSE), ref)
})


test_that("[add_]fitted_draws works on simple brms models with multiple dpars", {
  skip_if_not_installed("brms")
  m_dpars = readRDS(test_path("../models/models.brms.m_dpars.rds"))
  df_dpars = as_tibble(m_dpars$data)

  fits = fitted(m_dpars, df_dpars, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .value, -.chain, -.iteration, -.draw) %>%
    as_tibble()

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
    mutate(.row = as.integer(.row)) %>%
    group_by(count, Age, visit, .row)

  expect_equal(fitted_draws(m_dpars, df_dpars, dpar = TRUE), ref)
  expect_equal(add_fitted_draws(df_dpars, m_dpars, dpar = list("mu1", "mu2")), ref)
  # brms leaves some extra attributes on the resulting df, just ignore those
  # by using expect_equivalent here
  expect_equivalent(add_fitted_draws(df_dpars, m_dpars, dpar = FALSE), select(ref, -mu1, -mu2))
})


test_that("[add_]fitted_draws works on brms models with ordinal outcomes (response scale)", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE) %>%
    array2df(list(.draw = NA, .row = NA, .category = TRUE), label.x = ".value") %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .row = as.integer(.row),
      .draw = as.integer(.draw)
    )

  ref = inner_join(mtcars_tbl %>%
    mutate(.row = as.integer(rownames(.))), fits, by = ".row") %>%
    select(mpg:.row, .chain, .iteration, .draw, .category, .value) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row, .category)

  expect_equal(fitted_draws(m_cyl_mpg, mtcars_tbl), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_cyl_mpg), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_cyl_mpg, category = "foo"), rename(ref, foo = .category))

  #subsetting to test the `n` argument
  set.seed(1234)
  draw_subset = sample(unique(ref$.draw), 10)
  filtered_ref = ref %>%
    filter(.draw %in% draw_subset)

  expect_equal(add_fitted_draws(mtcars_tbl, m_cyl_mpg, n = 10, seed = 1234), filtered_ref)

})


test_that("[add_]fitted_draws works on brms models with ordinal outcomes (linear scale)", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE, scale = "linear") %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .value, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(fitted_draws(m_cyl_mpg, mtcars_tbl, scale = "linear"), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_cyl_mpg, scale = "linear"), ref)
})

test_that("[add_]fitted_draws works on brms models with dirichlet outcomes (response scale)", {
  skip_if_not_installed("brms")
  m_dirich = readRDS(test_path("../models/models.brms.m_dirich.rds"))

  grid = tibble(x = c("A", "B"))
  fits = fitted(m_dirich, grid, summary = FALSE) %>%
    array2df(list(.draw = NA, .row = NA, .category = TRUE), label.x = ".value") %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .row = as.integer(.row),
      .draw = as.integer(.draw)
    )

  ref = inner_join(grid %>% mutate(.row = as.integer(rownames(.))), fits, by = ".row") %>%
    select(x, .row, .chain, .iteration, .draw, .category, .value) %>%
    group_by(x, .row, .category)

  expect_equal(fitted_draws(m_dirich, grid), ref)
})


test_that("[add_]fitted_draws allows extraction of dpar on brms models with ordinal outcomes (linear scale)", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE, scale = "linear") %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .value, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  fits$mu = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE, dpar = "mu", scale = "linear") %>%
    as.data.frame() %>%
    gather(.row, mu) %$%
    mu

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(fits, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(fitted_draws(m_cyl_mpg, mtcars_tbl, scale = "linear", dpar = TRUE), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_cyl_mpg, scale = "linear", dpar = "mu"), ref)
})


test_that("[add_]fitted_draws allows extraction of dpar on brms models with categorical outcomes (response scale)", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS(test_path("../models/models.brms.m_cyl_mpg.rds"))

  fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE, scale = "response") %>%
    array2df(list(.draw = NA, .row = NA, .category = TRUE), label.x = ".value")

  mu_fits = fitted(m_cyl_mpg, mtcars_tbl, summary = FALSE, scale = "response", dpar = "mu") %>%
    array2df(list(.draw = NA, .row = NA), label.x = "mu")

  ref = mtcars_tbl %>% mutate(.row = as.integer(rownames(.))) %>%
    inner_join(fits, by = ".row") %>%
    left_join(mu_fits, by = c(".row", ".draw")) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .row = as.integer(.row),
      .draw = as.integer(.draw),
      .category = factor(.category)
    ) %>%
    select(mpg:.row, .chain, .iteration, .draw, .category, everything()) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row, .category)

  expect_equal(fitted_draws(m_cyl_mpg, mtcars_tbl, scale = "response", dpar = TRUE), ref)
  expect_equal(add_fitted_draws(mtcars_tbl, m_cyl_mpg, scale = "response", dpar = "mu"), ref)
})


test_that("[add_]fitted_draws throws an error when nsamples is called instead of n in brms", {
  skip_if_not_installed("brms")
  m_hp = readRDS(test_path("../models/models.brms.m_hp.rds"))

  expect_error(
    m_hp %>% fitted_draws(newdata = mtcars_tbl, nsamples = 100),
    "`nsamples.*.`n`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp %>% add_fitted_draws(newdata = mtcars_tbl, nsamples = 100),
    "`nsamples.*.`n`.*.See the documentation for additional details."
  )
})

test_that("[add_]predicted_draws throws an error when re.form is called instead of re_formula in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  expect_error(
    m_hp_wt %>% fitted_draws(newdata = mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp_wt %>% add_fitted_draws(newdata = mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
})

test_that("[add_]predicted_draws throws an error when transform is called instead of scale in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS(test_path("../models/models.rstanarm.m_hp_wt.rds"))

  expect_error(
    m_hp_wt %>% fitted_draws(newdata = mtcars_tbl, transform = TRUE),
    "`transform.*.`scale`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp_wt %>% add_fitted_draws(newdata = mtcars_tbl, transform = TRUE),
    "`transform.*.`scale`.*.See the documentation for additional details."
  )
})
