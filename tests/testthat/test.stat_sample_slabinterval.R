# Tests for sample distribution plots
#
# Author: mjskay
###############################################################################

library(dplyr)

context("stat_sample_")

test_that("vanilla stat_slabinterval works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  set.seed(1234)
  p = tribble(
    ~dist,  ~x,
    "norm", rnorm(100),
    "t",    rt(100, 3)
  ) %>%
    unnest(x) %>%
    ggplot() +
    scale_slab_alpha_continuous(range = c(0,1))

  vdiffr::expect_doppelganger("vanilla stat_slabinterval",
    p + stat_slabinterval(aes(x = dist, y = x), n = 20, slab_function = sample_slab_function)
  )
})

test_that("gradientinterval works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  set.seed(1234)
  p = tribble(
    ~dist,  ~x,
    "norm", rnorm(100),
    "t",    rt(100, 3)
  ) %>%
    unnest(x) %>%
    ggplot() +
    scale_slab_alpha_continuous(range = c(0,1))

  vdiffr::expect_doppelganger("gradientinterval with two groups",
    p + stat_gradientinterval(aes(x = dist, y = x), n = 20)
  )
  vdiffr::expect_doppelganger("gradientintervalh with two groups",
    p + stat_gradientintervalh(aes(y = dist, x = x), n = 20)
  )
})

test_that("histinterval and slab work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  set.seed(1234)
  p = tribble(
    ~dist,  ~x,
    "norm", rnorm(100),
    "t",    rt(100, 3)
  ) %>%
    unnest(x) %>%
    ggplot()

  vdiffr::expect_doppelganger("histinterval with outline",
    p + stat_histinterval(aes(x = dist, y = x), slab_color = "black")
  )
  vdiffr::expect_doppelganger("histintervalh with outline",
    p + stat_histintervalh(aes(y = dist, x = x), slab_color = "black")
  )

  vdiffr::expect_doppelganger("slab with outline",
    p + stat_slab(aes(x = dist, y = x), n = 20, slab_color = "black")
  )
  vdiffr::expect_doppelganger("slabh with outline",
    p + stat_slabh(aes(y = dist, x = x), n = 20, slab_color = "black")
  )

})

test_that("scale transformation works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")


  p_log = data.frame(x = qlnorm(ppoints(200), log(10), 2 * log(10))) %>%
    ggplot(aes(y = "a", x = x)) +
    scale_x_log10(breaks = 10^seq(-5,7, by = 2))

  vdiffr::expect_doppelganger("halfeyeh log scale transform",
    p_log + stat_halfeyeh(point_interval = mode_hdci, n = 50)
  )

  vdiffr::expect_doppelganger("ccdfintervalh log scale transform",
    p_log + stat_ccdfintervalh(point_interval = mean_hdi, n = 50)
  )

  vdiffr::expect_doppelganger("histintervalh log scale transform",
    p_log + stat_histintervalh(point_interval = median_qi, n = 50)
  )

})
