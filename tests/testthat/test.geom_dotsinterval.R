# Tests for dots geoms and stats
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)
library(distributional)



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
    ~dist,  ~x, ~datatype,
    "norm", rnorm(20), "slab",
    "t",    rt(20, 3), "slab"
  ) %>%
    unnest(x) %>%
    bind_rows(tribble(
      ~ dist,  ~x, ~datatype, ~lower, ~upper,
      "norm", 0, "interval", -1, 1,
      "t", 0, "interval", -2, 2
    )) %>%
    ggplot()

  vdiffr::expect_doppelganger("vanilla geom_dotsinterval",
    p + geom_dotsinterval(aes(y = dist, x = x, xmin = lower, xmax = upper, datatype = datatype))
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

test_that("geom_dots binwidth can be specified in unit()s", {
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

test_that("na.rm is propagated to quantile dotplot", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("na.rm with quantile arg",
    data.frame(x = qnorm(ppoints(100), 1)) %>%
      ggplot(aes(x, y = 0)) +
      stat_dots(na.rm = TRUE, quantiles = 20) +
      scale_x_continuous(limits = c(0,4))
  )
})

test_that("geom_dots works on discrete distributions", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("one integer bin",
    data.frame(x = rep(1L, 10)) %>%
      ggplot(aes(x = x, y = 0)) +
      stat_dots(orientation = "horizontal")
  )

  vdiffr::expect_doppelganger("three integer bins",
    data.frame(x = c(rep(1L, 10), rep(2L, 12), rep(3L, 5))) %>%
      ggplot(aes(x = x, y = 0)) +
      stat_dots(orientation = "horizontal")
  )

  vdiffr::expect_doppelganger("one character bin",
    data.frame(x = rep("a", 10)) %>%
      ggplot(aes(x = x, y = 0)) +
      stat_dots(orientation = "horizontal")
  )

  vdiffr::expect_doppelganger("three character bins",
    data.frame(x = c(rep("a", 10), rep("b", 12), rep("c", 5))) %>%
      ggplot(aes(x = x, y = 0)) +
      stat_dots(orientation = "horizontal")
  )

})

test_that("geom_dots works with NA in non-data axis", {
  skip_if_no_vdiffr()

  p = mtcars %>%
    ggplot(aes(x = mpg, y = factor(cyl))) +
    scale_y_discrete(limits = c("4", "6"))

  # without na.rm this should work but also throw a warning
  expect_warning(vdiffr::expect_doppelganger("NA on y axis",
    p + geom_dots(na.rm = FALSE)
  ))

  # with na.rm this should not throw a warning
  vdiffr::expect_doppelganger("removed NA on y axis",
    p + geom_dots(na.rm = TRUE)
  )
})

test_that("geom_dots allows constraints on binwidth", {
  skip_if_no_vdiffr()

  p = data.frame(x = ppoints(20)) %>%
    ggplot(aes(x = x, y = 0L))

  # max width of 1/40th of the viewport should approx space
  # this data with about 1 dot of space in between each dot
  vdiffr::expect_doppelganger("max binwidth",
    p + geom_dots(binwidth = unit(c(0, 1/40), "npc"))
  )

  # min width of 1/4th of the viewport should give us four giant bins
  vdiffr::expect_doppelganger("min binwidth",
    p + geom_dots(binwidth = unit(c(1/4, Inf), "npc"))
  )

})

test_that("geom_dots correctly adjusts dot size for stroke size", {
  skip_if_no_vdiffr()

  p = data.frame(x = ppoints(40)) %>%
    ggplot(aes(x = x))

  vdiffr::expect_doppelganger("size = 1 and 3",
    p +
      geom_dots(aes(y = "a"), binwidth = 1/20, size = 1, color = "black") +
      geom_dots(aes(y = "b"), binwidth = 1/20, size = 3, color = "black")
  )

})

test_that("side, justification, and scale can vary", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("varying side and just",
    mtcars %>%
      ggplot(aes(x = mpg, y = cyl,
        side = case_when(cyl == 4 ~ "top", cyl == 6 ~ "both", cyl == 8 ~ "bottom"),
        justification = case_when(cyl == 4 ~ 0, cyl == 6 ~ 0.5, cyl == 8 ~ 1)
        )) +
      stat_dotsinterval(orientation = "horizontal")
  )

  vdiffr::expect_doppelganger("varying scale, side, just",
    tibble(
      x = c(0, rep(1, 9), 0),
      group = c(rep("a", 4), rep("b", 7)),
      scale = c(rep(1/3, 4), rep(2/3, 7)),
      side = c(rep("top", 4), rep("bottom", 7)),
      justification = c(rep(0, 4), rep(1, 7))
    ) %>%
      ggplot(aes(x = x, y = group, scale = scale, side = side, justification = justification, color = group)) +
      stat_dots()
  )
})
