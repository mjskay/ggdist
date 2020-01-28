# Tests for dots geoms and stats
#
# Author: mjskay
###############################################################################

library(dplyr)

context("geom_dotsinterval")

test_that("vanilla dots geoms and stats work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

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
    p + geom_dotsh(aes(y = dist, x = x))
  )

  vdiffr::expect_doppelganger("stat_dotsh with a group with 1 dot",
    p + stat_dotsh(aes(y = dist, x = x, color = x > 2))
  )

  vdiffr::expect_doppelganger("stat_dotsh with a group with 2 dots",
    p + stat_dotsh(aes(y = dist, x = x, color = x > 1))
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
    p + stat_dotsintervalh(aes(y = dist, x = x), quantiles = 20)
  )

})

test_that("stat_dist_dots[interval] works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  set.seed(1234)
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
    p + stat_dist_dotsh(aes(y = dist), n = 20)
  )

  vdiffr::expect_doppelganger("vanilla stat_dist_dotsinterval",
    p + stat_dist_dotsinterval(aes(x = dist), n = 20)
  )

  vdiffr::expect_doppelganger("vanilla stat_dist_dotsintervalh",
    p + stat_dist_dotsintervalh(aes(y = dist), n = 20)
  )

})


