# Tests for sample distribution plots
#
# Author: mjskay
###############################################################################

library(dplyr)

context("stat_sample_slabinterval")

test_that("gradientinterval works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  set.seed(1234)
  p = tribble(
    ~dist, ~x,
    "norm", rnorm(100),
    "t", rt(100, 3)
  ) %>%
    unnest() %>%
    ggplot() +
    scale_slab_alpha_continuous(range = c(0,1))

  vdiffr::expect_doppelganger("gradientinterval with two groups",
    p + stat_gradientinterval(aes(x = dist, y = x), n = 20))
  vdiffr::expect_doppelganger("gradientintervalh with two groups",
    p + stat_gradientintervalh(aes(y = dist, x = x), n = 20))
})

test_that("histinterval works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  set.seed(1234)
  p = tribble(
    ~dist, ~x,
    "norm", rnorm(100),
    "t", rt(100, 3)
  ) %>%
    unnest() %>%
    ggplot()

  vdiffr::expect_doppelganger("histinterval with outline",
    p + stat_histinterval(aes(x = dist, y = x), slab_color = "black"))
  vdiffr::expect_doppelganger("histintervalh with outline",
    p + stat_histintervalh(aes(y = dist, x = x), slab_color = "black"))
})

test_that("scale transformation works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  vdiffr::expect_doppelganger("scale transformation works",
    data.frame(x = qlnorm(ppoints(100))) %>% ggplot(aes(y = "a", x = x)) + stat_halfeyeh() + scale_x_log10()
  )
})
