# Tests for line ribbons
#
# Author: mjskay
###############################################################################

library(testthat)
import::from(dplyr, `%>%`, inner_join, data_frame, group_by_all, do, bind_rows, arrange)
import::from(purrr, map_df)
library(tidyr)
library(tidybayes)
library(vdiffr)

context("geom_lineribbon")

make_line_data = function(offset = 0, seed = 123, g = "a") {
  set.seed(seed)
  data_frame(
      x = seq(0, 1, length.out = 25),
      g = g
    ) %>%
    group_by_all() %>%
    do(data_frame(
      y = rnorm(500, mean = .$x + offset)
    ))
}

test_that("one-group lineribbons work", {
  df = make_line_data()

  p = ggplot(df, aes(x = x, y = y))

  expect_doppelganger("one-group lineribbon", p + stat_lineribbon(.prob = c(.50, .75, .90)) + scale_fill_brewer())
  expect_doppelganger("one-group lineribbon (reverse order)", p + stat_lineribbon(.prob = c(.90, .75, .50)) + scale_fill_brewer())
})

test_that("two-group lineribbons work", {
  df = bind_rows(
    make_line_data(),
    make_line_data(offset = 4, seed = 234, g = "b")
  ) %>%
    arrange(x)

  p = ggplot(df, aes(x = x, y = y))

  expect_doppelganger("two-group lineribbons grouped by group", p +
      stat_lineribbon(aes(group = g), .prob = c(.50, .75, .90)) + scale_fill_brewer())
  expect_doppelganger("two-group lineribbons grouped by linetype", p +
      stat_lineribbon(aes(linetype = g), .prob = c(.50, .75, .90)) + scale_fill_brewer())
  expect_doppelganger("two-group lineribbons grouped by color", p +
      stat_lineribbon(aes(color = g), .prob = c(.50, .75, .90)) + scale_fill_brewer())
  expect_doppelganger("two-group lineribbons grouped by color and linetype", p +
      stat_lineribbon(aes(color = g, linetype = g), .prob = c(.50, .75, .90)) + scale_fill_brewer())
})
