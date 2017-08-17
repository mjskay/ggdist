# Tests for compose_data
#
# Author: mjskay
###############################################################################

library(testthat)
library(tidybayes)
import::from(dplyr, transmute)

context("compose_data")

get_nested_data = function() {
  data.frame(
    plot = factor(paste0("p", rep(1:8, times = 2))),
    site = factor(paste0("s", rep(1:4, each = 2, times = 2)))
  )
}

test_that("compose_data returns pure lists (for compatibility with runjags)", {
  df = get_nested_data()

  expect_equal(class(compose_data(df)), "list")
})

test_that("compose_data works for a basic set of factors", {
  df = get_nested_data()

  ref = list(
    plot = as.array(as.numeric(df$plot)),
    n_plot = nlevels(df$plot),
    site = as.array(as.numeric(df$site)),
    n_site = nlevels(df$site),
    n = nrow(df)
  )

  expect_equal(compose_data(df), ref)
})

test_that("compose_data works with two data frames and a named argument", {
  df = get_nested_data()
  df2 = get_nested_data() %>%
    transmute(
      plot2 = plot,
      site2 = site
    )

  ref = list(
    plot = as.array(as.numeric(df$plot)),
    n_plot = nlevels(df$plot),
    site = as.array(as.numeric(df$site)),
    n_site = nlevels(df$site),
    n = nrow(df),
    plot2 = as.array(as.numeric(df2$plot2)),
    n_plot2 = nlevels(df2$plot2),
    site2 = as.array(as.numeric(df2$site2)),
    n_site2 = nlevels(df2$site2),
    n_d2 = nrow(df2)
  )

  expect_equal(compose_data(df, d2 = df2), ref)
})

test_that("compose_data arguments are evaluated within the list composed so far", {
  df = get_nested_data()

  ref = list(
    plot = as.array(as.numeric(df$plot)),
    n_plot = nlevels(df$plot),
    site = as.array(as.numeric(df$site)),
    n_site = nlevels(df$site),
    n = nrow(df),
    site2 = as.array(as.numeric(df$site) + 1)
  )

  expect_equal(compose_data(df, site2 = site + 1), ref)
})
