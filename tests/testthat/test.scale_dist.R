# Tests for scale_x_dist and scale_y_dist
#
# Author: mjskay
###############################################################################

library(dplyr)
library(distributional)


# scale_x_dist ------------------------------------------------------------

test_that("basic scale_XXX_dist", {
  skip_if_no_vdiffr()


  df = data.frame(var = c(1,2), dist = dist_normal(0:1,1))

  vdiffr::expect_doppelganger("scale_y_dist",
    df %>%
      ggplot(aes(x = var, y = dist)) +
      stat_dist_halfeye(n = 15) +
      scale_y_dist(limits = c(-7, 7))
  )

  vdiffr::expect_doppelganger("scale_x_dist",
    df %>%
      ggplot(aes(x = dist, y = var)) +
      stat_dist_halfeye(n = 15) +
      scale_x_dist(limits = c(-7, 7))
  )

})
