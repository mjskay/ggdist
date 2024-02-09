# Tests for curve_interval
#
# Author: mjskay
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})


test_that("curve_interval works with lineribbon", {
  skip_if_no_vdiffr()
  skip_if_not_installed("posterior")


  k = 11 # number of curves
  n = 101
  curve_df = tibble(
    .draw = 1:k,
    mean = seq(-5,5, length.out = k),
    x = list(seq(-15,15,length.out = n))
  ) %>%
    unnest(x) %>%
    mutate(y = dnorm(x, mean, 3)/max(dnorm(x, mean, 3))) %>%
    group_by(x)


  vdiffr::expect_doppelganger("curve_interval with mhd",
    curve_df %>%
      curve_interval(y, .width = c(.5, .8), .interval = "mhd") %>%
      ggplot(aes(x = x, y = y)) +
      geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
      geom_line(aes(group = .draw), alpha = 0.15, data = curve_df) +
      scale_fill_brewer()
  )

  # conditioning works
  set.seed(1234)
  b_draw = sample.int(n)
  ab_curve_df = bind_rows(
    mutate(curve_df, group = "a"),
    mutate(curve_df, group = "b", .draw = b_draw[.draw])
  ) %>%
    arrange(.draw)

  vdiffr::expect_doppelganger("conditional curve_interval with mhd",
    ab_curve_df %>%
      group_by(group) %>%
      curve_interval(y, .along = x, .width = c(.5, .8), .interval = "mhd") %>%
      ggplot(aes(x = x, y = y)) +
      geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
      geom_line(aes(group = .draw), alpha = 0.15, data = curve_df) +
      scale_fill_brewer() +
      facet_wrap(~ group)
  )

  # joint works
  # TODO: fix
  # vdiffr::expect_doppelganger("joint curve_interval with mhd",
  #   ab_curve_df %>%
  #     arrange(.draw) %>%  # TODO: this should not be needed
  #     curve_interval(y, .along = c(group, x), .width = c(.5, .8), .interval = "mhd") %>%
  #     ggplot(aes(x = x, y = y)) +
  #     geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
  #     geom_line(aes(group = .draw), alpha = 0.15, data = curve_df) +
  #     scale_fill_brewer() +
  #     facet_wrap(~ group)
  # )

  skip_if_not_installed("fda")
  vdiffr::expect_doppelganger("curve_interval with bd-mbd",
    curve_df %>%
      curve_interval(y, .width = c(.5, .8), .interval = "bd-mbd") %>%
      ggplot(aes(x = x, y = y)) +
      geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
      geom_line(aes(group = .draw), alpha = 0.15, data = curve_df) +
      scale_fill_brewer()
  )

})


# basic cases: 95%, 0%, 100%, > 100% ---------------------------------------------

test_that("basic cases on single interval work", {
  df = data.frame(value = ppoints(1000))

  ref = tibble(
    value = 0.4995,
    .lower = NA,
    .upper = NA,
    .actual_width = NA,
    .width = NA,
    .point = "mhd",
    .interval = "mhd"
  )

  expect_equal(
    curve_interval(df, .width = .95),
    mutate(ref, .lower = .0255, .upper = .9745, .actual_width = .95, .width = .95)
  )
  expect_equal(
    curve_interval(df, .width = 0),
    mutate(ref, .lower = 0.4995, .upper = .5005, .actual_width = 0.002, .width = 0)
  )
  expect_equal(
    curve_interval(df, .width = 1),
    mutate(ref, .lower = 0.0005, .upper = .9995, .actual_width = 1, .width = 1)
  )
  expect_equal(
    curve_interval(df, .width = 1.1),
    mutate(ref, .lower = 0.0005, .upper = .9995, .actual_width = 1, .width = 1.1)
  )
})

test_that("basic cases on single curve work", {
  df = data.frame(x = 1:3, y = rep(ppoints(1000), each = 3) + 1:3)

  ref = tibble(
    x = rep(1:3, 3),
    y = rep(0.4995 + 1:3, 3),
    .lower = rep(c(.0255, 0.4995, 0.0005), each = 3) + rep(1:3, 3),
    .upper = rep(c(.9745, 0.5005, 0.9995), each = 3) + rep(1:3, 3),
    .actual_width = rep(c(.95, 0.002, 1), each = 3),
    .width = rep(c(.95, 0, 1), each = 3),
    .point = "mhd",
    .interval = "mhd"
  )

  # data frame of draws
  expect_equal(curve_interval(df, .along = x, .width = c(.95, 0, 1)), ref)
  expect_equal(curve_interval(group_by(df, x), .width = c(.95, 0, 1)), ref)

  skip_if_not_installed("posterior")
  # data frame of rvars
  y_rvar = rep(posterior::rvar(ppoints(1000)), 3) + 1:3
  df = data.frame(x = 1:3, y = y_rvar)
  expect_equal(curve_interval(df, .along = x, .width = c(.95, 0, 1)), ref)
  expect_equal(curve_interval(group_by(df, x), .width = c(.95, 0, 1)), ref)

  # rvar
  expect_equal(curve_interval(y_rvar, .width = c(.95, 0, 1)), select(ref, -x, .value = y))

  # matrix
  expect_equal(curve_interval(posterior::draws_of(y_rvar), .width = c(.95, 0, 1)), select(ref, -x, .value = y))
})

test_that("basic cases on multiple variables", {
  df = data.frame(
    x = 1:3,
    y1 = rep(ppoints(1000), each = 3) + 1:3,
    y2 = rep(rev(ppoints(1000)), each = 3) + 2:4
  )

  ref = tibble(
    x = rep(1:3, 3),
    y1 = rep(0.4995 + 1:3, 3),
    y1.lower = rep(c(.0255, 0.4995, 0.0005), each = 3) + rep(1:3, 3),
    y1.upper = rep(c(.9745, 0.5005, 0.9995), each = 3) + rep(1:3, 3),
    .actual_width = rep(c(.95, 0.002, 1), each = 3),
    .width = rep(c(.95, 0, 1), each = 3),
    y2 = rep(0.5005 + 2:4, 3),
    y2.lower = y1.lower + 1,
    y2.upper = y1.upper + 1,
    .point = "mhd",
    .interval = "mhd"
  )

  expect_equal(curve_interval(df, .along = x, .width = c(.95, 0, 1)), ref)
  expect_equal(curve_interval(group_by(df, x), y1, y2, .width = c(.95, 0, 1)), ref)
})


# errors ------------------------------------------------------------------

test_that("error is thrown when no columns found to summarize", {
  df = data.frame(value = ppoints(10))
  expect_error(curve_interval(df, .exclude = "value"), "No columns found to calculate point and interval summaries for")
})

test_that("error is thrown with groups of different sizes", {
  df = data.frame(value = ppoints(9), group = c("a", "a", "b"))
  expect_error(curve_interval(df, .along = group), "Must have the same number of values in each group")
})

test_that("curve_interval(<rvar>) and curve_interval(<matrix>) do not support along", {
  expect_error(
    curve_interval(matrix(1:4, nrow = 2), .along = "x"),
    'does\\s+not\\s+support\\s+the\\s+[^a-zA-Z]*\\.along[^a-zA-Z]*\\s+argument'
  )

  skip_if_not_installed("posterior")
  expect_error(
    curve_interval(posterior::rvar(), .along = "x"),
    'does\\s+not\\s+support\\s+the\\s+[^a-zA-Z]*\\.along[^a-zA-Z]*\\s+argument'
  )
})
