# Tests for interval geoms
#
# Author: mjskay
###############################################################################

library(dplyr)
library(purrr)
library(tidyr)
library(vdiffr)

context("geom_interval")

test_that("horizontal grouped intervals work", {
  data(RankCorr, package = "tidybayes")

  forward_plot = RankCorr %>%
    spread_samples(u_tau[i]) %>%
    mean_qi(.prob = c(.5, .8, .95, .99)) %>%
    ggplot(aes(y = i, x = u_tau)) +
    geom_intervalh() +
    scale_color_brewer()

  expect_doppelganger("grouped intervals (h)", forward_plot)


  stat_forward_plot = RankCorr %>%
    spread_samples(u_tau[i]) %>%
    ggplot(aes(y = i, x = u_tau)) +
    stat_intervalh(.prob = c(.5, .8, .95, .99)) +
    scale_color_brewer()

  expect_doppelganger("grouped intervals (h, stat)", stat_forward_plot)

  reverse_plot = RankCorr %>%
    spread_samples(u_tau[i]) %>%
    mean_qi(.prob = c(.99, .95, .8, .5)) %>%
    ggplot(aes(y = i, x = u_tau)) +
    geom_intervalh() +
    scale_color_brewer()

  expect_doppelganger("grouped intervals (h, reverse order)", reverse_plot)

  stat_reverse_plot = RankCorr %>%
    spread_samples(u_tau[i]) %>%
    ggplot(aes(y = i, x = u_tau)) +
    stat_intervalh(.prob = c(.99, .95, .8, .5)) +
    scale_color_brewer()

  expect_doppelganger("grouped intervals (h, stat, reverse order)", stat_reverse_plot)
})

test_that("grouped intervals work", {
  data(RankCorr, package = "tidybayes")

  forward_plot = RankCorr %>%
    spread_samples(u_tau[i]) %>%
    mean_qi(.prob = c(.5, .8, .95, .99)) %>%
    ggplot(aes(x = i, y = u_tau)) +
    geom_interval() +
    scale_color_brewer()

  expect_doppelganger("grouped intervals", forward_plot)


  stat_forward_plot = RankCorr %>%
    spread_samples(u_tau[i]) %>%
    ggplot(aes(x = i, y = u_tau)) +
    stat_interval(.prob = c(.5, .8, .95, .99)) +
    scale_color_brewer()

  expect_doppelganger("grouped intervals (stat)", stat_forward_plot)

  reverse_plot = RankCorr %>%
    spread_samples(u_tau[i]) %>%
    mean_qi(.prob = c(.99, .95, .8, .5)) %>%
    ggplot(aes(x = i, y = u_tau)) +
    geom_interval() +
    scale_color_brewer()

  expect_doppelganger("grouped intervals (reverse order)", reverse_plot)

  stat_reverse_plot = RankCorr %>%
    spread_samples(u_tau[i]) %>%
    ggplot(aes(x = i, y = u_tau)) +
    stat_interval(.prob = c(.99, .95, .8, .5)) +
    scale_color_brewer()

  expect_doppelganger("grouped intervals (stat, reverse order)", stat_reverse_plot)
})
