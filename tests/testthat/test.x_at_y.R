# Tests for point_interval
#
# Author: mjskay
###############################################################################

import::from(magrittr, `%$%`)
import::from(purrr, map_df)

context("x_at_y")


get_nested_data = function() {
  data.frame(
    plot = factor(paste0("p", rep(1:8, times = 2))),
    site = factor(paste0("s", rep(1:4, each = 2, times = 2)))
  )
}

test_that("x_at_y works on a nested design with no missing levels", {
  df = get_nested_data()

  expect_equal(df %$% x_at_y(site, plot), factor(c("s1", "s1", "s2", "s2", "s3", "s3", "s4", "s4")))
  expect_equal(df %$% x_at_y(as.numeric(site), as.numeric(plot)), c(1, 1, 2, 2, 3, 3, 4, 4))
  expect_equal(df %$% x_at_y(site, plot)[plot], df$site)
})

test_that("x_at_y works on a nested design with missing levels", {
  df = get_nested_data()[-c(1, 9, 6, 14), ]  #drop rows for p1, p6

  expect_equal(df %$% x_at_y(site, plot), factor(c(NA, "s1", "s2", "s2", "s3", NA, "s4", "s4")))
  expect_equal(df %$% x_at_y(as.numeric(site), as.numeric(plot)), c(NA, 1, 2, 2, 3, NA, 4, 4))
  expect_equal(df %$% x_at_y(site, plot)[plot], df$site)
})

test_that("x_at_y works even if the data frame is not sorted by y", {
  df = get_nested_data()
  rev_df = map_df(df, rev)

  expect_equal(df %$% x_at_y(site, plot), rev_df %$% x_at_y(site, plot))
})

test_that("x_at_y does not work when there is a non-unique x for a given y", {
  df = get_nested_data()
  df[14, "site"] = "s2"  # yields non-unique site for plot p6

  expect_error(df %$% x_at_y(site, plot),
   "`plot` does not appear to be nested in `site`: there are multiple values of `site` for at least one value of `plot`"
  )
})

test_that("x_at_y does not work with an integer y having min(y) < 1", {
  df = get_nested_data()
  df$plot = as.numeric(df$plot)
  df[1, "plot"] = 0

  expect_error(df %$% x_at_y(site, plot), "All values of `plot` must be >= 1. Got min\\(`plot`\\) == 0")
})

test_that("x_at_y fails for non-numeric / non-factor y", {
  df = get_nested_data()
  df$plot = as.character(df$plot)

  expect_error(df %$% x_at_y(site, plot),
    "Cannot generate a lookup table for non-numeric / non-factor variable: `plot` is of type \"character\""
  )
})
