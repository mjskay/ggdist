# Tests for colour_ramp / fill_ramp scales
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)
library(distributional)




test_that("basic fill_ramp works", {
  skip_if_no_vdiffr()



  vdiffr::expect_doppelganger("basic fill_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(fill_ramp = after_stat(x)), n = 20, fill = "red")
  )

  vdiffr::expect_doppelganger("basic fill_ramp from red",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(fill_ramp = after_stat(x)), fill = "blue", n = 20) +
      scale_fill_ramp_continuous(from = "red")
  )

  vdiffr::expect_doppelganger("basic discrete fill_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(fill_ramp = after_stat(cut(x, c(-Inf, 0.25, 0.75, Inf)))), fill = "blue", n = 20) +
      scale_fill_ramp_discrete(from = "red")
  )
})


test_that("basic color_ramp works", {
  skip_if_no_vdiffr()



  vdiffr::expect_doppelganger("basic colour_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(colour_ramp = after_stat(x)), n = 20, color = "red", size = 5) +
      scale_colour_ramp_continuous(from = "blue")
  )

  vdiffr::expect_doppelganger("basic color_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(color_ramp = after_stat(x)), n = 20, color = "red", size = 5) +
      scale_color_ramp_continuous(from = "blue")
  )

  vdiffr::expect_doppelganger("basic discrete colour_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(colour_ramp = after_stat(cut(x, c(-Inf, 0.25, 0.75, Inf)))), n = 20, color = "red", size = 5) +
      scale_colour_ramp_discrete(from = "blue")
  )

  vdiffr::expect_doppelganger("basic discrete color_ramp",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(color_ramp = after_stat(cut(x, c(-Inf, 0.25, 0.75, Inf)))), n = 20, color = "red", size = 5) +
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
      stat_dist_interval(aes(color_ramp = after_stat(level)), position = "dodge")
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
          fill_ramp = after_stat(level)
        ),
        position = "dodgejust",
        n = 50
      ) +
      scale_fill_ramp_discrete(na.translate = FALSE) +
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
    y = .mapply(function(x, y) x + y * -4:5, list(intercept, slope), NULL)
  ) %>%
    unnest(c(x, y))

  df_2groups = rbind(
    mutate(df, g = "a"),
    mutate(df, g = "b", y = (y - 2) * 0.5)
  )

  vdiffr::expect_doppelganger("fill_ramp with lineribbon",
    df_2groups %>%
      ggplot(aes(x = x, y = y, fill = g)) +
      stat_lineribbon(aes(fill_ramp = after_stat(level)))
  )
})


# partial_colour_ramp type ------------------------------------------------------

test_that("partial_colour_ramp formatting works", {
  expect_equal(vec_ptype_full(partial_colour_ramp()), "partial_colour_ramp")
  expect_equal(vec_ptype_abbr(partial_colour_ramp()), "rmp")
  expect_equal(format(partial_colour_ramp()), character())
  expect_equal(format(partial_colour_ramp(1:2/2, c("red", "blue"))), c("[0.5 from red]", "[1 from blue]"))
})

test_that("partial_colour_ramp casting works", {
  expect_equal(as_partial_colour_ramp(2), partial_colour_ramp(2))
  expect_equal(vec_cast(partial_colour_ramp(2), double()), 2.0)
  expect_equal(vec_cast(partial_colour_ramp(2L), integer()), 2L)
  expect_equal(vec_cast(2.0, partial_colour_ramp()), partial_colour_ramp(2))
  expect_equal(vec_cast(2L, partial_colour_ramp()), partial_colour_ramp(2))

  expect_equal(c(partial_colour_ramp(1), partial_colour_ramp(2)), partial_colour_ramp(c(1, 2)))
  expect_equal(c(partial_colour_ramp(1), 2), partial_colour_ramp(c(1, 2)))
  expect_equal(c(partial_colour_ramp(1), 2L), partial_colour_ramp(c(1, 2)))

  expect_equal(vec_c(2, partial_colour_ramp(1)), partial_colour_ramp(c(2, 1)))
  expect_equal(vec_c(2L, partial_colour_ramp(1)), partial_colour_ramp(c(2, 1)))

  expect_error(partial_colour_ramp(1) + character())
})
