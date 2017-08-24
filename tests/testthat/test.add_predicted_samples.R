# Tests for predicted_samples
#
# Author: mjskay
###############################################################################

library(testthat)
library(tidybayes)
library(dplyr)
library(tidyr)
library(magrittr)
library(rstanarm)
import::from(purrr, quietly)

context("predicted_samples")

test_that("[add_]predicted_samples and basic arguments works on a simple rstanarm model", {
  data = mtcars %>%
    set_rownames(1:nrow(.)) %>%
    as_data_frame()

  m = quietly(~ stan_glm(mpg ~ hp*wt, data = data, chains = 1, iter = 500))()$result

  preds = posterior_predict(m, data, draws = 100, seed = 123) %>%
    as.data.frame() %>%
    mutate(
      .chain = as.numeric(NA),
      .iteration = as.numeric(1:n())
    ) %>%
    gather(.row, pred, -.chain, -.iteration) %>%
    as_data_frame()

  ref = inner_join(data %>% mutate(.row = rownames(.)), preds, by = ".row") %>%
    mutate(.row = factor(as.numeric(.row))) %>%
    arrange(.iteration, .row)

  expect_equal(ref, predicted_samples(m, data, n = 100, seed = 123))
  expect_equal(ref, add_predicted_samples(data, m, n = 100, seed = 123))
})
