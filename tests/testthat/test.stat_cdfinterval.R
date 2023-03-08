# Tests for CDF bar plots
#
# Author: mjskay
###############################################################################

library(dplyr)



test_that("dodged ccdf barplots work", {
  skip_if_no_vdiffr()


  df = data.frame(y = 1:5, x = "a", g = c("g1")) %>%
    rbind(data.frame(y = rep(1:5, each = 3) + 1:3, x = "b", g = c("g1", "g2", "g3")))

  p = ggplot(df, aes(x = x, y = y))

  vdiffr::expect_doppelganger("vertical CCDF bar plot with dodging (3 groups)",
    p + stat_ccdfinterval(aes(fill = g), position = "dodge", n = 10)
  )

})
