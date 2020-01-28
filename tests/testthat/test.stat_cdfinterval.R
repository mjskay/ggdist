# Tests for CDF bar plots
#
# Author: mjskay
###############################################################################

library(dplyr)

context("stat_cdfinterval")

test_that("dodged ccdf barplots work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  set.seed(123)
  df = data.frame(y = rnorm(500, 1), x = "a", g = c("g1")) %>%
    rbind(data.frame(y = rnorm(900), x = "b", g = c("g1", "g2", "g3")))

  p = ggplot(df, aes(x = x, y = y))

  vdiffr::expect_doppelganger("vertical CCDF bar plot with dodging (3 groups)",
    p + stat_ccdfinterval(aes(fill = g), position = "dodge", n = 20)
  )

})
