# Tests for eye plots
#
# Author: mjskay
###############################################################################

import::from(dplyr, `%>%`, inner_join, data_frame)
import::from(purrr, map_df)
library(tidyr)
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
  df = data.frame(x = rnorm(1000), y = "a", y_int = 1) %>%
    rbind(data.frame(x = rnorm(1000, 1), y = "b", y_int = 2))

  p = ggplot(df, aes(x = x, y = y))
  expect_doppelganger("two-parameter (factor) horizontal eye", p + geom_eyeh())
  expect_doppelganger("two-parameter (factor) horizontal half-eye", p + geom_halfeyeh())

  p = ggplot(df, aes(x = y, y = x))
  expect_doppelganger("two-parameter (factor) vertical eye", p + geom_eye())

  p = ggplot(df, aes(x = x, y = y_int))
  expect_doppelganger("two-parameter (numeric) horizontal eye", p + geom_eyeh())
  expect_doppelganger("two-parameter (numeric) horizontal half-eye", p + geom_halfeyeh())

  p = ggplot(df, aes(x = y_int, y = x))
  expect_doppelganger("two-parameter (numeric) vertical eye", p + geom_eye())

})
