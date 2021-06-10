# Tests for interval geoms
#
# Author: mjskay
###############################################################################

library(dplyr)
library(purrr)
library(tidyr)

context("geom_interval")

# use a subset of RankCorr so tests are faster
data(RankCorr_u_tau, package = "ggdist")
RankCorr_u_tau = RankCorr_u_tau %>%
  filter(i %in% 1:3, .iteration %in% 1:50) %>%
  group_by(i)

test_that("horizontal grouped intervals work", {
  skip_if_no_vdiffr()


  vdiffr::expect_doppelganger("grouped intervals (h)",
    RankCorr_u_tau %>%
      mean_qi(.width = c(.5, .75, .90)) %>%
      ggplot(aes(y = i, x = u_tau, xmin = .lower, xmax = .upper)) +
      geom_interval() +
      scale_color_brewer()
  )

  vdiffr::expect_doppelganger("grouped intervals (h, stat)",
    RankCorr_u_tau %>%
      ggplot(aes(y = factor(i), x = u_tau)) +
      stat_interval(.width = c(.5, .75, .90)) +
      scale_color_brewer()
  )

  vdiffr::expect_doppelganger("grouped intervals (h, stat, mode_hdi)",
    RankCorr_u_tau %>%
      ggplot(aes(y = factor(i), x = u_tau)) +
      stat_interval(.width = c(.5, .75, .90), point_interval = mode_hdi) +
      scale_color_brewer()
  )

  reverse_plot = RankCorr_u_tau %>%
    mean_qi(.width = c(.90, .75, .5)) %>%
    ggplot(aes(y = i, x = u_tau, xmin = .lower, xmax = .upper)) +
    geom_interval() +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals (h, reverse order)", reverse_plot)

  vdiffr::expect_doppelganger("grouped intervals (h, stat, reverse order)",
    RankCorr_u_tau %>%
      ggplot(aes(y = factor(i), x = u_tau)) +
      stat_interval(.width = c(.90, .75, .5)) +
      scale_color_brewer()
  )
})

test_that("grouped intervals work", {
  skip_if_no_vdiffr()


  forward_plot = RankCorr_u_tau %>%
    mean_qi(.width = c(.5, .75, .90)) %>%
    ggplot(aes(x = i, y = u_tau, ymin = .lower, ymax = .upper)) +
    geom_interval() +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals", forward_plot)

  stat_forward_plot = RankCorr_u_tau %>%
    ggplot(aes(x = i, y = u_tau)) +
    stat_interval(.width = c(.5, .75, .90)) +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals (stat)", stat_forward_plot)

})

test_that("multimodal intervals work with stat_interval", {
  skip_if_no_vdiffr()


  set.seed(1234)
  df = data.frame(x = c(rnorm(300), rnorm(300, 5)) + c(0,.5), g = c("a","b"))

  vdiffr::expect_doppelganger("multimodal intervals (h, stat, dodged)",
    df %>%
      ggplot(aes(x = x, y = "a", group = g)) +
      stat_interval(point_interval = mean_hdi, position = "dodge") +
      scale_color_brewer()
  )

  stat_interval_plot_multimodal_dodged = df %>%
    ggplot(aes(y = x, x = "a", group = g)) +
    stat_interval(point_interval = mean_hdi, position = "dodge") +
    scale_color_brewer()

  vdiffr::expect_doppelganger("multimodal intervals (stat, dodged)", stat_interval_plot_multimodal_dodged)
})
