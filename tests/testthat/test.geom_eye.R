# Tests for eye plots
#
# Author: mjskay
###############################################################################

library(testthat)
import::from(dplyr, `%>%`, inner_join, data_frame)
import::from(purrr, map_df)
library(tidyr)
library(tidybayes)
library(vdiffr)

context("geom_eye")

test_that("one-parameter eye plots work", {
  set.seed(123)
  df = data.frame(x = rnorm(1000), y = 1)

  p = ggplot(df, aes(x = x, y = y))
  expect_doppelganger("one-parameter horizontal eye", p + geom_eyeh())
  expect_doppelganger("one-parameter horizontal half-eye", p + geom_halfeyeh())

  p = ggplot(df, aes(x = y, y = x))
  expect_doppelganger("one-parameter vertical eye", p + geom_eye())
})


test_that("two-parameter eye plots work", {
  set.seed(123)
  df = data.frame(x = rnorm(1000), y = "a") %>%
    rbind(data.frame(x = rnorm(1000, 1), y = "b"))

  p = ggplot(df, aes(x = x, y = y))
  expect_doppelganger("two-parameter (factor) horizontal eye", p + geom_eyeh())
  expect_doppelganger("two-parameter (factor) horizontal half-eye", p + geom_halfeyeh())

  p = ggplot(df, aes(x = y, y = x))
  expect_doppelganger("two-parameter (factor) vertical eye", p + geom_eye())
})

