# Tests for [add_]predicted_draws
#
# Author: mjskay
###############################################################################

suppressWarnings(suppressMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(magrittr)
}))

context("predicted_draws")


# data
mtcars_tbl = mtcars %>%
  set_rownames(seq_len(nrow(.))) %>%
  as_tibble()


test_that("[add_]predicted_draws throws an error on unsupported models", {
  data("RankCorr", package = "tidybayes")

  expect_error(predicted_draws(RankCorr, data.frame()),
    'Models of type "mcmc.list" are not currently supported by `predicted_draws`')
  expect_error(add_predicted_draws(data.frame(), RankCorr),
    'Models of type "mcmc.list" are not currently supported by `predicted_draws`')
})


test_that("[add_]predicted_draws and basic arguments works on a simple rstanarm model", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS("../models/models.rstanarm.m_hp_wt.rds")

  preds = rstanarm::posterior_predict(m_hp_wt, mtcars_tbl, draws = 100, seed = 123) %>%
    as.data.frame() %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .prediction, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(preds, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(ref, predicted_draws(m_hp_wt, mtcars_tbl, n = 100, seed = 123))
  expect_equal(ref, add_predicted_draws(mtcars_tbl, m_hp_wt, n = 100, seed = 123))
})


test_that("[add_]predicted_draws and basic arguments works on an rstanarm model with random effects", {
  skip_if_not_installed("rstanarm")
  m_cyl = readRDS("../models/models.rstanarm.m_cyl.rds")

  preds = rstanarm::posterior_predict(m_cyl, mtcars_tbl, draws = 100, seed = 123) %>%
    as.data.frame() %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .prediction, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(preds, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(ref, predicted_draws(m_cyl, mtcars_tbl, n = 100, seed = 123))
  expect_equal(ref, add_predicted_draws(mtcars_tbl, m_cyl, n = 100, seed = 123))
})


test_that("[add_]predicted_draws works on a simple brms model", {
  skip_if_not_installed("brms")
  m_hp = readRDS("../models/models.brms.m_hp.rds")

  set.seed(123)
  preds = predict(m_hp, mtcars_tbl, summary = FALSE, nsamples = 100) %>%
    as.data.frame() %>%
    set_names(seq_len(ncol(.))) %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = seq_len(n())
    ) %>%
    gather(.row, .prediction, -.chain, -.iteration, -.draw) %>%
    as_tibble()

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(preds, by = ".row") %>%
    mutate(.row = as.integer(.row))

  expect_equal(predicted_draws(m_hp, mtcars_tbl, n = 100, seed = 123), ref)
  expect_equal(add_predicted_draws(mtcars_tbl, m_hp, n = 100, seed = 123), ref)
})

test_that("[add_]predicted_draws works on brms models with categorical outcomes", {
  skip_if_not_installed("brms")
  m_cyl_mpg = readRDS("../models/models.brms.m_cyl_mpg.rds")

  set.seed(1234)
  preds = predict(m_cyl_mpg, mtcars_tbl, summary = FALSE, nsamples = 100) %>%
    array2df(list(.draw = NA, .row = NA), label.x = ".prediction") %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .draw = as.integer(.draw),
      .row = as.character(.row),
      .prediction = factor(.prediction, levels = 1:3, labels = paste0("c", c(4,6,8)))
    )

  ref = mtcars_tbl %>%
    mutate(.row = rownames(.)) %>%
    inner_join(preds, by = ".row") %>%
    mutate(.row = as.integer(.row)) %>%
    group_by(mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb, .row)

  expect_equal(predicted_draws(m_cyl_mpg, mtcars_tbl, seed = 1234, n = 100), ref)
  expect_equal(add_predicted_draws(mtcars_tbl, m_cyl_mpg, seed = 1234, n = 100), ref)
})

test_that("[add_]predicted_draws works on brms models with dirichlet responses", {
  skip_if_not_installed("brms")
  m_dirich = readRDS("../models/models.brms.m_dirich.rds")

  set.seed(1234)
  grid = tibble(x = c("A", "B"))
  preds = predict(m_dirich, grid, summary = FALSE, nsamples = 100) %>%
    array2df(list(.draw = NA, .row = NA, .category = TRUE), label.x = ".prediction") %>%
    mutate(
      .chain = NA_integer_,
      .iteration = NA_integer_,
      .row = as.integer(.row),
      .draw = as.integer(.draw)
    )

  ref = grid %>%
    mutate(.row = as.integer(rownames(.))) %>%
    inner_join(preds, by = ".row") %>%
    group_by(x, .row, .category)

  expect_equal(predicted_draws(m_dirich, grid, seed = 1234, n = 100), ref)
})

test_that("[add_]predicted_draws throws an error when nsamples is called instead of n in brms", {
  skip_if_not_installed("brms")
  m_hp = readRDS("../models/models.brms.m_hp.rds")

  expect_error(
    m_hp %>% predicted_draws(newdata = mtcars_tbl, nsamples = 100),
    "`nsamples.*.`n`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp %>% add_predicted_draws(newdata = mtcars_tbl, nsamples = 100),
    "`nsamples.*.`n`.*.See the documentation for additional details."
  )
})

test_that("[add_]predicted_draws throws an error when draws is called instead of n in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS("../models/models.rstanarm.m_hp_wt.rds")

  expect_error(
    m_hp_wt %>% predicted_draws(newdata = mtcars_tbl, draws = 100),
    "`draws.*.`n`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp_wt %>% add_predicted_draws(newdata = mtcars_tbl, draws = 100),
    "`draws.*.`n`.*.See the documentation for additional details."
  )
})

test_that("[add_]predicted_draws throws an error when re.form is called instead of re_formula in rstanarm", {
  skip_if_not_installed("rstanarm")
  m_hp_wt = readRDS("../models/models.rstanarm.m_hp_wt.rds")

  expect_error(
    m_hp_wt %>% predicted_draws(newdata = mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
  expect_error(
    m_hp_wt %>% add_predicted_draws(newdata = mtcars_tbl, re.form = NULL),
    "`re.form.*.`re_formula`.*.See the documentation for additional details."
  )
})
