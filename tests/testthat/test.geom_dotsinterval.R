# Tests for dots geoms and stats
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)
library(distributional)

context("geom_dotsinterval")

test_that("vanilla dots geoms and stats work", {
  skip_if_no_vdiffr()


  set.seed(1234)
  p = tribble(
    ~dist,  ~x,
    "norm", rnorm(20),
    "t",    rt(20, 3)
  ) %>%
    unnest(x) %>%
    ggplot()

  vdiffr::expect_doppelganger("vanilla geom_dots",
    p + geom_dots(aes(x = dist, y = x))
  )

  vdiffr::expect_doppelganger("vanilla geom_dotsh",
    p + geom_dots(aes(y = dist, x = x))
  )

  vdiffr::expect_doppelganger("stat_dotsh with a group with 1 dot",
    p + stat_dots(aes(y = dist, x = x, color = x > 2))
  )

  vdiffr::expect_doppelganger("stat_dotsh with a group with 2 dots",
    p + stat_dots(aes(y = dist, x = x, color = x > 1))
  )

  set.seed(1234)
  p = tribble(
    ~dist,  ~x,
    "norm", rnorm(100),
    "t",    rt(100, 3)
  ) %>%
    unnest(x) %>%
    ggplot()

  vdiffr::expect_doppelganger("vanilla stat_dotsinterval",
    p + stat_dotsinterval(aes(x = dist, y = x), quantiles = 20)
  )

  vdiffr::expect_doppelganger("vanilla stat_dotsintervalh",
    p + stat_dotsinterval(aes(y = dist, x = x), quantiles = 20)
  )

})

test_that("stat_dist_dots[interval] works", {
  skip_if_no_vdiffr()


  p = tribble(
    ~dist,  ~args,
    "norm", list(0, 1),
    "t",    list(3)
  ) %>%
    ggplot(aes(dist = dist, args = args))

  vdiffr::expect_doppelganger("vanilla stat_dist_dots",
    p + stat_dist_dots(aes(x = dist), n = 20)
  )

  vdiffr::expect_doppelganger("vanilla stat_dist_dotsh",
    p + stat_dist_dots(aes(y = dist), n = 20)
  )

  vdiffr::expect_doppelganger("vanilla stat_dist_dotsinterval",
    p + stat_dist_dotsinterval(aes(x = dist), n = 20)
  )

  vdiffr::expect_doppelganger("vanilla stat_dist_dotsintervalh",
    p + stat_dist_dotsinterval(aes(y = dist), n = 20)
  )

})

test_that("stat_dist_dots works on NA data", {
  skip_if_no_vdiffr()


  p = data.frame(
    x = c("norm", NA, "norm"),
    y = c("a","b", NA)
  ) %>%
    ggplot(aes(dist = x, y = y))

  expect_warning(vdiffr::expect_doppelganger("stat_dist_dots with na.rm = FALSE",
    p + stat_dist_dots(na.rm = FALSE)
  ), "Removed 1 rows containing non-finite values")

  vdiffr::expect_doppelganger("stat_dist_dots with na.rm = TRUE",
    p + stat_dist_dots(na.rm = TRUE)
  )

})

test_that("stat_dist_dots works on distributional objects", {
  skip_if_no_vdiffr()


  p = data.frame(
    x = dist_normal(0:1, 1:2),
    y = c("a","b")
  ) %>%
    ggplot(aes(dist = x, y = y))

  vdiffr::expect_doppelganger("stat_dist_dots with dist_normal",
    p + stat_dist_dots()
  )

})

test_that("geom_dots bin_width can be specified in unit()s", {
  skip_if_no_vdiffr()


  # these dots should be the same size (10% of facet height)
  vdiffr::expect_doppelganger("geom_dots with unit() binwidth",
    mtcars %>%
      ggplot(aes(y = mpg)) +
      geom_dots(binwidth = unit(0.1, "native")) +
      facet_grid(~ am, scales = "free")
  )
})

test_that("dotplot layouts work", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("weave top",
    mtcars %>%
      ggplot(aes(x = mpg)) +
      geom_dots(layout = "weave", side = "top")
  )

  vdiffr::expect_doppelganger("weave bottom",
    mtcars %>%
      ggplot(aes(x = mpg)) +
      geom_dots(layout = "weave", side = "bottom")
  )

  vdiffr::expect_doppelganger("weave both",
    mtcars %>%
      ggplot(aes(x = mpg)) +
      geom_dots(layout = "weave", side = "both")
  )

  vdiffr::expect_doppelganger("swarm top",
    mtcars %>%
      ggplot(aes(x = mpg)) +
      geom_dots(layout = "swarm")
  )

  vdiffr::expect_doppelganger("swarm bottom",
    mtcars %>%
      ggplot(aes(x = mpg)) +
      geom_dots(layout = "swarm", side = "bottom")
  )

  vdiffr::expect_doppelganger("swarm both",
    mtcars %>%
      ggplot(aes(x = mpg)) +
      geom_dots(layout = "swarm", side = "both")
  )

  vdiffr::expect_doppelganger("swarm vertical",
    mtcars %>%
      ggplot(aes(y = mpg)) +
      geom_dots(layout = "swarm")
  )

})

