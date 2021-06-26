# Tests for curve_interval
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)




test_that("curve_interval works with lineribbon", {
  skip_if_no_vdiffr()


  k = 11 # number of curves
  n = 101
  curve_df = tibble(
    .draw = 1:k,
    mean = seq(-5,5, length.out = k),
    x = list(seq(-15,15,length.out = n)),
  ) %>%
    unnest(x) %>%
    mutate(y = dnorm(x, mean, 3)/max(dnorm(x, mean, 3))) %>%
    group_by(x)


  vdiffr::expect_doppelganger("curve_interval with mhd",
    curve_df %>%
      curve_interval(y, .width = c(.5, .8), .interval = "mhd") %>%
      ggplot(aes(x = x, y = y)) +
      geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
      geom_line(aes(group = .draw), alpha = 0.15, data = curve_df) +
      scale_fill_brewer()
  )

  skip_if_not_installed("fda")
  vdiffr::expect_doppelganger("curve_interval with bd-mbd",
    curve_df %>%
      curve_interval(y, .width = c(.5, .8), .interval = "bd-mbd") %>%
      ggplot(aes(x = x, y = y)) +
      geom_lineribbon(aes(ymin = .lower, ymax = .upper)) +
      geom_line(aes(group = .draw), alpha = 0.15, data = curve_df) +
      scale_fill_brewer()
  )

})
