# Tests for spike plots
#
# Author: mjskay
###############################################################################

suppressPackageStartupMessages({
  library(distributional)
})


test_that("spike works", {
  skip_if_no_vdiffr()

  p = ggplot(data.frame(x = dist_normal()), aes(xdist = x)) +
    stat_slabinterval() +
    stat_spike() +
    stat_spike(at = c(-1.5, 1.5), color = "red") +
    stat_spike(at = qi, color = "green") +
    scale_thickness_shared()

  vdiffr::expect_doppelganger("spike works",
    p
  )
})
