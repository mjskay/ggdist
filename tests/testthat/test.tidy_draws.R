# Tests for tidy_draws
#
# Author: mjskay
###############################################################################

library(tibble)
library(dplyr)
library(purrr)
library(magrittr)
library(coda)

context("tidy_draws")



# brms --------------------------------------------------------------------
test_that("tidy_draws works with brms", {
  skip_if_not_installed("brms")

  # we use a model with random effects here because they include parameters with multiple dimensions
  m_ranef = readRDS("../models/models.brms.m_ranef.rds")

  draws_tidy =
    brms::posterior_samples(m_ranef, add_chain = TRUE) %>%
    select(.chain = chain, .iteration = iter, everything()) %>%
    mutate(
      .chain = as.integer(.chain),
      .iteration = as.integer(.iteration - min(.iteration) + 1),
      .draw = as.integer((.chain - 1) * max(.iteration) + .iteration)
    ) %>%
    as_tibble() %>%
    select(.chain, .iteration, .draw, everything()) %>%
    bind_cols(map_dfr(rstan::get_sampler_params(m_ranef$fit, inc_warmup = FALSE), as_tibble))

  expect_equal(tidy_draws(m_ranef), draws_tidy)
})


# rstanarm ----------------------------------------------------------------
test_that("tidy_draws works with rstanarm", {
  skip_if_not_installed("rstanarm")

  # we use a model with random effects here because they include parameters with multiple dimensions
  m_ranef = readRDS("../models/models.rstanarm.m_ranef.rds")

  chain_1 = as_tibble(as.array(m_ranef)[,1,]) %>%
    add_column(.chain = 1L, .iteration = 1L:nrow(.), .draw = 1L:nrow(.), .before = 1)
  chain_2 = as_tibble(as.array(m_ranef)[,2,]) %>%
    add_column(.chain = 2L, .iteration = 1L:nrow(.), .draw = (nrow(.) + 1L):(2L * nrow(.)), .before = 1)
  draws_tidy =
    bind_rows(chain_1, chain_2) %>%
    bind_cols(map_dfr(rstan::get_sampler_params(m_ranef$stanfit, inc_warmup = FALSE), as_tibble))

  expect_equal(tidy_draws(m_ranef), draws_tidy)
})



# rstan -------------------------------------------------------------------
test_that("tidy_draws works with rstan", {
  skip_if_not_installed("rstan")

  # we use a model with random effects here because they include parameters with multiple dimensions
  m_ABC = readRDS("../models/models.rstan.m_ABC.rds")

  chain_1 = as_tibble(as.array(m_ABC)[,1,]) %>%
    add_column(.chain = 1L, .iteration = 1L:nrow(.), .draw = 1L:nrow(.), .before = 1)
  chain_2 = as_tibble(as.array(m_ABC)[,2,]) %>%
    add_column(.chain = 2L, .iteration = 1L:nrow(.), .draw = (nrow(.) + 1L):(2L * nrow(.)), .before = 1)
  draws_tidy =
    bind_rows(chain_1, chain_2)  %>%
    bind_cols(map_dfr(rstan::get_sampler_params(m_ABC, inc_warmup = FALSE), as_tibble))

  expect_equal(tidy_draws(m_ABC), draws_tidy)
})


# jags --------------------------------------------------------------------
test_that("tidy_draws works with runjags", {
  # runjags will still load without JAGS, it just fails later (so skipping on runjags alone will
  # not work correctly if runjags is installed but the system does not have JAGS). So we skip if 
  # rjags does not load as well, as rjags will correctly fail to load if JAGS isn't installed.
  skip_if_not_installed("rjags")
  skip_if_not_installed("runjags")

  runjags::runjags.options(inits.warning = FALSE, nodata.warning = FALSE)
  # run.jags has some progress output I can't seem to turn off, hence capture.output
  # also it seems to break w.r.t. n.chains if not run in the global environment with n.chains set(!!),
  # hence all this evalq garbage and such
  n.chains <<- 2
  capture.output(m <- evalq(runjags::run.jags(
    model = "model { a ~ dnorm(0,1); for(i in 1:2) {b[i] ~ dnorm(0,1)} }",
    n.chains = 2,
    monitor = c("a", "b"),
    adapt = 100,
    sample = 100,
    silent.jags = TRUE,
    summarise = FALSE
  ), globalenv()))

  draws = as.mcmc.list(m)
  draws_tidy =
    rbind(
      data.frame(.chain = 1L, .iteration = 1:100L, .draw = 1:100L, draws[[1]], check.names = FALSE),
      data.frame(.chain = 2L, .iteration = 1:100L, .draw = 101:200L, draws[[2]], check.names = FALSE)
    ) %>%
    as_tibble()

  expect_equal(tidy_draws(m), draws_tidy)
})

test_that("tidy_draws works with rjags", {
  skip_if_not_installed("rjags")

  # coda.samples has some progress output I can't seem to turn off, hence capture.output
  capture.output(m <- rjags::coda.samples(
    rjags::jags.model(
      textConnection("model { a ~ dnorm(0,1); for(i in 1:2) {b[i] ~ dnorm(0,1)} }"),
      n.chains = 2,
      n.adapt = 100,
      quiet = TRUE
    ),
    variable.names = c("a", "b"),
    n.iter = 100
  ))

  draws_tidy =
    rbind(
      data.frame(.chain = 1L, .iteration = 1:100L, .draw = 1:100L, m[[1]], check.names = FALSE),
      data.frame(.chain = 2L, .iteration = 1:100L, .draw = 101:200L, m[[2]], check.names = FALSE)
    ) %>%
    as_tibble()

  expect_equal(tidy_draws(m), draws_tidy)
})

test_that("tidy_draws works with jagsUI", {
  skip_if_not_installed("jagsUI")

  # this test model is kind of dumb because jagsUI doesn't seem to allow you to not input data
  # (and I was feeling lazy when modifying the test models for runjags / rjags to work with this API)
  m = jagsUI::jags(
    data = list(y = c(-1,0,1)),
    model.file = textConnection("model { for (j in 1:3) { y[j] ~ dnorm(a, 1) } a ~ dnorm(0,1); for(i in 1:2) {b[i] ~ dnorm(0,1)} }"),
    n.chains = 2,
    n.adapt = 100,
    parameters.to.save = c("a", "b"),
    n.iter = 100,
    verbose = FALSE
  )

  draws_tidy =
    rbind(
      data.frame(.chain = 1L, .iteration = 1:100L, .draw = 1:100L, m$samples[[1]], check.names = FALSE),
      data.frame(.chain = 2L, .iteration = 1:100L, .draw = 101:200L, m$samples[[2]], check.names = FALSE)
    ) %>%
    as_tibble()

  expect_equal(tidy_draws(m), draws_tidy)
})
