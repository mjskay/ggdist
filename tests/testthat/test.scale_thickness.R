# Tests for thickness scales
#
# Author: mjskay
###############################################################################

library(dplyr)
library(distributional)



test_that("basic scale_thickness_shared works", {
  skip_if_no_vdiffr()


  prior_post = data.frame(
    prior = dist_normal(0, 1),
    posterior = dist_normal(0, 0.5)
  )

  p = prior_post %>%
    ggplot() +
    stat_halfeye(aes(xdist = posterior), n = 10) +
    stat_slab(aes(xdist = prior), fill = NA, color = "#e41a1c", n = 10)

  vdiffr::expect_doppelganger("basic scale_thickness_identity",
    p + scale_thickness_identity()
  )

  vdiffr::expect_doppelganger("basic scale_thickness_shared",
    p + scale_thickness_shared()
  )
})
