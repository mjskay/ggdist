# Tests for colour_ramp / fill_ramp scales
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)
library(purrr)
library(distributional)

context("scale_colour_ramp")


test_that("basic fill_ramp works", {
  skip_if_no_vdiffr()



  vdiffr::expect_doppelganger("basic fill_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(fill_ramp = stat(x)), n = 20, fill = "red")
  )

  vdiffr::expect_doppelganger("basic fill_ramp from red",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(fill_ramp = stat(x)), fill = "blue", n = 20) +
      scale_fill_ramp_continuous(from = "red")
  )

  vdiffr::expect_doppelganger("basic discrete fill_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(fill_ramp = stat(cut(x, c(-Inf, 0.25, 0.75, Inf)))), fill = "blue", n = 20) +
      scale_fill_ramp_discrete(from = "red")
  )
})


test_that("basic color_ramp works", {
  skip_if_no_vdiffr()



  vdiffr::expect_doppelganger("basic colour_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(colour_ramp = stat(x)), n = 20, color = "red", size = 5) +
      scale_colour_ramp_continuous(from = "blue")
  )

  vdiffr::expect_doppelganger("basic color_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(color_ramp = stat(x)), n = 20, color = "red", size = 5) +
      scale_color_ramp_continuous(from = "blue")
  )

  vdiffr::expect_doppelganger("basic discrete colour_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(colour_ramp = stat(cut(x, c(-Inf, 0.25, 0.75, Inf)))), n = 20, color = "red", size = 5) +
      scale_colour_ramp_discrete(from = "blue")
  )

  vdiffr::expect_doppelganger("basic discrete color_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(color_ramp = stat(cut(x, c(-Inf, 0.25, 0.75, Inf)))), n = 20, color = "red", size = 5) +
      scale_color_ramp_discrete(from = "blue")
  )
})


test_that("color_ramp works with stat_interval", {
  skip_if_no_vdiffr()



  dist_df = tribble(
    ~group, ~subgroup, ~mean, ~sd,
    "a",          "h",     5,   1,
    "b",          "h",     7,   1.5,
    "c",          "h",     8,   1,
    "c",          "i",     9,   1,
    "c",          "j",     7,   1
  )

  vdiffr::expect_doppelganger("color_ramp with interval and subgroups",
    dist_df %>%
      ggplot(aes(x = group, dist = dist_normal(mean, sd), color = subgroup)) +
      stat_dist_interval(aes(color_ramp = stat(level)), position = "dodge")
  )
})


test_that("fill_ramp works with stat_slab and NAs", {
  skip_if_no_vdiffr()



  dist_df = tribble(
    ~group, ~subgroup, ~mean, ~sd,
    "a",          "h",     5,   1,
    "b",          "h",     7,   1.5,
    "c",          "h",     8,   1,
    "c",          "i",     9,   1,
    "c",          "j",     7,   1
  )

  vdiffr::expect_doppelganger("fill_ramp with slab and NAs",
    dist_df %>%
      ggplot(aes(y = group, dist = dist_normal(mean, sd))) +
      stat_dist_halfeye(
        aes(
          fill = subgroup,
          fill_ramp = stat(cut_cdf_qi(
            cdf,
            .width = c(.5, .8, .95),
            labels = scales::percent_format()
          ))
        ),
        position = "dodge",
        n = 50
      ) +
      scale_fill_ramp_discrete(range = c(1, 0.2), na.translate = FALSE) +
      labs(fill_ramp = "level")
  )
})


test_that("fill_ramp works on lineribbons", {
  skip_if_no_vdiffr()



  set.seed(1234)
  n = 1000

  df = tibble(
    .draw = 1:n,
    intercept = rnorm(n, 3, 1),
    slope = rnorm(n, 1, 0.25),
    x = list(-4:5),
    y = map2(intercept, slope, ~ .x + .y * -4:5)
  ) %>%
    unnest(c(x, y))

  df_2groups = rbind(
    mutate(df, g = "a"),
    mutate(df, g = "b", y = (y - 2) * 0.5)
  )

  vdiffr::expect_doppelganger("fill_ramp with lineribbon",
    df_2groups %>%
      ggplot(aes(x = x, y = y, fill = g)) +
      stat_lineribbon(aes(fill_ramp = stat(level)))
  )
})
