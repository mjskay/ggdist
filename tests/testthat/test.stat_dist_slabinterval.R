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
