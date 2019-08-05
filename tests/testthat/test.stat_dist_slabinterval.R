# Tests for analytical distribution plots
#
# Author: mjskay
###############################################################################

library(dplyr)

context("stat_dist_")

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
    p + stat_dist_halfeye(aes(x = dist))
  )

  vdiffr::expect_doppelganger("horizontal half-eye using args",
    p + stat_dist_halfeyeh(aes(y = dist)))
})

test_that("stat fill aesthetic on halfeye works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  vdiffr::expect_doppelganger("gradient fill/color halfeye",
    data.frame(dist = "norm", mean = 0, sd = 1) %>%
      ggplot(aes(y = 1, dist = dist, arg1 = mean, arg2 = sd, slab_color = stat(x > 0), fill = stat(f))) +
      stat_dist_halfeyeh(n = 10)
  )
})

test_that("stat_dist_gradientinterval works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  p = tribble(
    ~dist, ~args,
    "norm", list(0, 1),
    "t", list(3)
  ) %>%
    ggplot(aes(dist = dist, args = args, fill = dist)) +
    scale_slab_alpha_continuous(range = c(0,1))

  vdiffr::expect_doppelganger("dist_gradientinterval with two groups",
    p + stat_dist_gradientinterval(aes(x = dist), n = 20, p_limits = c(0.01, 0.99))
  )
  vdiffr::expect_doppelganger("dist_gradientintervalh with two groups",
    p + stat_dist_gradientintervalh(aes(y = dist), n = 20, p_limits = c(0.01, 0.99))
  )
})

test_that("stat_dist_pointinterval, interval, and slab work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  p = tribble(
    ~dist, ~args,
    "norm", list(0, 1),
    "t", list(3)
  ) %>%
    ggplot(aes(dist = dist, args = args)) +
    scale_color_brewer()

  vdiffr::expect_doppelganger("dist_pointinterval with two groups",
    p + stat_dist_pointinterval(aes(x = dist))
  )
  vdiffr::expect_doppelganger("dist_pointintervalh with two groups",
    p + stat_dist_pointintervalh(aes(y = dist))
  )

  vdiffr::expect_doppelganger("dist_interval with two groups",
    p + stat_dist_interval(aes(x = dist))
  )
  vdiffr::expect_doppelganger("dist_intervalh with two groups",
    p + stat_dist_intervalh(aes(y = dist))
  )

  vdiffr::expect_doppelganger("dist_slab with two groups",
    p + stat_dist_slab(aes(x = dist))
  )
  vdiffr::expect_doppelganger("dist_slabh with two groups",
    p + stat_dist_slabh(aes(y = dist))
  )
})

test_that("density transformation works", {
  expect_equal(transform_pdf(dnorm, 1:5, scales::exp_trans()), dlnorm(1:5))
  expect_equal(transform_pdf(dlnorm, -2:2, scales::log_trans()), dnorm(-2:2))
})

test_that("scale transformation works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  # this setup should yield a 95% interval from a little above 1e-3 to a little below 1e+5
  p_log = data.frame(dist = "lnorm") %>%
    ggplot(aes(y = 1, dist = dist, arg1 = log(10), arg2 = 2*log(10))) +
    scale_x_log10(breaks = 10^seq(-5,7, by = 2))

  vdiffr::expect_doppelganger("dist_halfeyeh log scale transform",
    p_log + stat_dist_halfeyeh()
  )

  vdiffr::expect_doppelganger("dist_ccdfintervalh log scale transform",
    p_log + stat_dist_ccdfintervalh()
  )


  p_rev = data.frame(dist = "lnorm") %>%
    ggplot(aes(y = 1, dist = dist, arg1 = 1, arg2 = 0.5)) +
    scale_x_reverse()

  vdiffr::expect_doppelganger("dist_halfeyeh reverse scale transform",
    p_rev + stat_dist_halfeyeh()
  )

  vdiffr::expect_doppelganger("dist_ccdfintervalh reverse scale transform",
    p_rev + stat_dist_ccdfintervalh()
  )
})
