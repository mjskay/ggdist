# Tests for fitted_samples
#
# Author: mjskay
###############################################################################

library(testthat)
library(tidybayes)
library(dplyr)
library(tidyr)
library(magrittr)
library(brms)
import::from(purrr, quietly)

context("fitted_samples")

test_that("[add_]fitted_samples works on brms models without auxpars", {
  skip_on_cran()    # let's not compile the model for this test on CRAN

  data = mtcars %>%
    set_rownames(1:nrow(.)) %>%
    as_data_frame()

  m = quietly(~ brm(mpg ~ log(hp)*am, data = data, chains = 1, iter = 1000, family = lognormal))()$result

  fits = fitted(m, data, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(1:ncol(.)) %>%
    mutate(
      .chain = as.numeric(NA),
      .iteration = as.numeric(1:n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  ref = inner_join(data %>% mutate(.row = rownames(.)), fits, by = ".row") %>%
    mutate(.row = factor(as.numeric(.row))) %>%
    arrange(.iteration, .row)

  expect_equal(ref, fitted_samples(m, data))
  expect_equal(ref, add_fitted_samples(data, m))
  expect_equal(ref, add_fitted_samples(data, m, auxpars = FALSE))
})


test_that("[add_]fitted_samples works on brms models with auxpars", {
  skip_on_cran()    # let's not compile the model for this test on CRAN

  data = mtcars %>%
    set_rownames(1:nrow(.)) %>%
    as_data_frame()

  # this is dumb model but at least gets an auxpar
  m = quietly(~ brm(bf(mpg ~ log(hp), sigma ~ hp),
    prior = c(prior(normal(0, 1), class = b)),
    data = data, chains = 1, iter = 4000, family = lognormal
  ))()$result

  fits = fitted(m, data, summary = FALSE) %>%
    as.data.frame() %>%
    set_names(1:ncol(.)) %>%
    mutate(
      .chain = as.numeric(NA),
      .iteration = as.numeric(1:n())
    ) %>%
    gather(.row, estimate, -.chain, -.iteration) %>%
    as_data_frame()

  fits$sigma = fits_sigma = fitted(m, data, summary = FALSE, dpar = "sigma") %>%
    as.data.frame() %>%
    gather(.row, sigma) %$%
    sigma

  ref = inner_join(data %>% mutate(.row = rownames(.)), fits, by = ".row") %>%
    mutate(.row = factor(as.numeric(.row))) %>%
    arrange(.iteration, .row)

  fitted_samples(m, data, auxpars = FALSE)

  expect_equal(ref, fitted_samples(m, data))
  expect_equal(ref, add_fitted_samples(data, m))
  expect_equal(ref, add_fitted_samples(data, m, auxpars = TRUE))
  expect_equal(ref, add_fitted_samples(data, m, auxpars = "sigma"))
  expect_equal(ref %>% select(-sigma), add_fitted_samples(data, m, auxpars = FALSE))
  expect_equal(ref %>% select(-sigma), add_fitted_samples(data, m, auxpars = NULL))
})
