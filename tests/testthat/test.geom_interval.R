# Tests for interval geoms
#
# Author: mjskay
###############################################################################

library(dplyr)
library(purrr)
library(tidyr)

context("geom_interval")

# use a subset of RankCorr so tests are faster
data(RankCorr, package = "tidybayes")
RankCorr_s = RankCorr[[1]][1:100,]
RankCorr_u_tau = RankCorr_s %>%
  spread_draws(u_tau[i]) %>%
  filter(i %in% 1:3)

test_that("horizontal grouped intervals work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  forward_plot = RankCorr_u_tau %>%
    mean_qi(.width = c(.5, .75, .90)) %>%
    ggplot(aes(y = i, x = u_tau)) +
    geom_intervalh() +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals (h)", forward_plot)

  stat_forward_plot = RankCorr_u_tau %>%
    ggplot(aes(y = i, x = u_tau)) +
    stat_intervalh(.width = c(.5, .75, .90)) +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals (h, stat)", stat_forward_plot)

  stat_forward_plot_mode_hdi = RankCorr_u_tau %>%
    ggplot(aes(y = i, x = u_tau)) +
    stat_intervalh(.width = c(.5, .75, .90), point_interval = mode_hdi) +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals (h, stat, mode_hdi)", stat_forward_plot_mode_hdi)

  reverse_plot = RankCorr_u_tau %>%
    mean_qi(.width = c(.90, .75, .5)) %>%
    ggplot(aes(y = i, x = u_tau)) +
    geom_intervalh() +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals (h, reverse order)", reverse_plot)

  stat_reverse_plot = RankCorr_u_tau %>%
    ggplot(aes(y = i, x = u_tau)) +
    stat_intervalh(.width = c(.90, .75, .5)) +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals (h, stat, reverse order)", stat_reverse_plot)
})

test_that("grouped intervals work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  forward_plot = RankCorr_u_tau %>%
    mean_qi(.width = c(.5, .75, .90)) %>%
    ggplot(aes(x = i, y = u_tau)) +
    geom_interval() +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals", forward_plot)

  stat_forward_plot = RankCorr_u_tau %>%
    ggplot(aes(x = i, y = u_tau)) +
    stat_interval(.width = c(.5, .75, .90)) +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals (stat)", stat_forward_plot)

  stat_forward_plot_mode_hdi = RankCorr_u_tau %>%
    ggplot(aes(x = i, y = u_tau)) +
    stat_interval(.width = c(.5, .75, .90), point_interval = mode_hdi) +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals (stat, mode_hdi)", stat_forward_plot_mode_hdi)

  reverse_plot = RankCorr_u_tau %>%
    mean_qi(.width = c(.90, .75, .5)) %>%
    ggplot(aes(x = i, y = u_tau)) +
    geom_interval() +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals (reverse order)", reverse_plot)

  stat_reverse_plot = RankCorr_u_tau %>%
    ggplot(aes(x = i, y = u_tau)) +
    stat_interval(.width = c(.90, .75, .5)) +
    scale_color_brewer()

  vdiffr::expect_doppelganger("grouped intervals (stat, reverse order)", stat_reverse_plot)
})
