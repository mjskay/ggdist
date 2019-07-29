# Tests for analytical distribution plots
#
# Author: mjskay
###############################################################################

library(dplyr)

context("stat_dist_slabinterval")

test_that("distribution eye plots work with the args aesthetic", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  p = tribble(
    ~dist, ~args,
    "norm", list(0, 1),
    "beta", list(5, 5)
  ) %>%
    ggplot(aes(dist = dist, args = args))

  vdiffr::expect_doppelganger("vertical half-eye using args",
    p + stat_dist_halfeye(aes(x = dist)))

  vdiffr::expect_doppelganger("horizontal half-eye using args",
    p + stat_dist_halfeyeh(aes(y = dist)))
})

test_that("multiple fill gradient halfeye works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  p = data.frame(dist = "norm", mean = 0, sd = 1) %>%
    ggplot(aes(y = 1, dist = dist, arg1 = mean, arg2 = sd, slab_color = stat(x > 0), fill = stat(f))) +
    stat_dist_halfeyeh(n = 10)
  vdiffr::expect_doppelganger("gradient fill/color halfeye", p)
})

test_that("gradientinterval works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  p = tribble(
    ~dist, ~args,
    "norm", list(0, 1),
    "t", list(3)
  ) %>%
    ggplot(aes(dist = dist, args = args, fill = dist)) +
    scale_alpha_continuous(range = c(0,1))

  vdiffr::expect_doppelganger("gradientinterval with two groups",
    p + stat_dist_gradientinterval(aes(x = dist), n = 20, p_limits = c(0.01, 0.99)))
  vdiffr::expect_doppelganger("gradientintervalh with two groups",
    p + stat_dist_gradientintervalh(aes(y = dist), n = 20, p_limits = c(0.01, 0.99)))
})
