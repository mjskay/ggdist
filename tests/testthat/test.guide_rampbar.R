# Tests for guide_rampbar
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)
library(purrr)
library(distributional)



test_that("rampbar works with fill_ramp", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("fill_ramp without `to`",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(fill_ramp = stat(x)), fill = "blue", n = 20) +
      scale_fill_ramp_continuous(from = "red", guide = guide_rampbar())
  )

  vdiffr::expect_doppelganger("fill_ramp with `to`",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(fill_ramp = stat(x)), fill = "blue", n = 20) +
      scale_fill_ramp_continuous(from = "red", guide = guide_rampbar(to = "blue"))
  )
})


test_that("rampbar works with color_ramp", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("color_ramp without `to`",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(color_ramp = stat(x)), n = 20, color = "red", size = 5) +
      scale_color_ramp_continuous(from = "blue", guide = guide_rampbar())
  )

  vdiffr::expect_doppelganger("color_ramp with `to`",
    tibble(d = dist_uniform(0, 1)) %>%
      ggplot(aes(y = 0, dist = d)) +
      stat_dist_slab(aes(color_ramp = stat(x)), n = 20, color = "red", size = 5) +
      scale_color_ramp_continuous(from = "blue", guide = guide_rampbar(to = "red"))
  )
})
