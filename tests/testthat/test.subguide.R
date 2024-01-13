# Tests for sub-guides
#
# Author: mjskay
###############################################################################

library(dplyr)
library(distributional)


# slabinterval ---------------------------------------------------------

test_that("slab subguide works with dodging", {
  skip_if_no_vdiffr()

  df = data.frame(
    x = c(dist_gamma(1:3,1:3), dist_normal(2:3,0.75)),
    group = c("a","a","a","b","b"),
    subgroup = c("d","e","f","d","e")
  )

  p = df %>%
    ggplot(aes(xdist = x, y = group, fill = subgroup)) +
    scale_x_continuous(expand = expansion(add = 1))

  vdiffr::expect_doppelganger("slab subguide with dodging and group normalization",
    p + stat_slabinterval(
      subguide = subguide_axis(label_side = "outside", title = "den", theme = theme_test()),
      position = "dodgejust",
      height = 0.9,
      scale = 0.8,
      normalize = "groups",
      n = 11
    )
  )
})

test_that("slab subguide positioning works", {
  skip_if_no_vdiffr()

  df = data.frame(x = dist_normal(0,1))

  p = df %>%
    ggplot(aes(xdist = x)) +
    scale_x_continuous(expand = expansion(add = 1))

  sg = subguide_axis(title = "test", label_side = "inside", theme = theme_test())
  vdiffr::expect_doppelganger("slab subguide with inside labels",
    p +
      stat_slabinterval(aes(y = "1"), subguide = sg(position = 1), n = 5) +
      stat_slabinterval(aes(y = "0.5"), subguide = sg(position = 0.5), n = 5) +
      stat_slabinterval(aes(y = "0"), subguide = sg(position = 0), n = 5) +
      stat_slabinterval(aes(y = "left"), subguide = sg(position = "left"), n = 5) +
      stat_slabinterval(aes(y = "right"), subguide = sg(position = "right"), n = 5) +
      stat_slabinterval(aes(y = "right, just = 1"), subguide = sg(position = "right", just = 1), n = 5) +
      theme_test() +
      theme(plot.margin = margin(5.5,50,5.5,5.5))
  )

  sg = subguide_axis(title = "test", label_side = "outside", theme = theme_test())
  vdiffr::expect_doppelganger("slab subguide with outside labels",
    p +
      stat_slabinterval(aes(y = "1"), subguide = sg(position = 1), n = 5) +
      stat_slabinterval(aes(y = "0.5"), subguide = sg(position = 0.5), n = 5) +
      stat_slabinterval(aes(y = "0"), subguide = sg(position = 0), n = 5) +
      stat_slabinterval(aes(y = "left"), subguide = sg(position = "left"), n = 5) +
      stat_slabinterval(aes(y = "right"), subguide = sg(position = "right"), n = 5) +
      stat_slabinterval(aes(y = "right, just = 1"), subguide = sg(position = "right", just = 1), n = 5) +
      theme_test() +
      theme(plot.margin = margin(5.5,50,5.5,5.5))
  )
})

test_that("slab subguide works with side and justification", {
  skip_if_no_vdiffr()

  df = data.frame(x = dist_normal(0,1))

  p = df %>%
    ggplot(aes(xdist = x)) +
    scale_x_continuous(expand = expansion(add = 1))

  sg = subguide_axis(title = "test", theme = theme_test())
  vdiffr::expect_doppelganger("slab subguide with side",
    p +
      stat_slabinterval(aes(y = "1 bottom"), subguide = sg, side = "bottom", n = 5) +
      stat_slabinterval(aes(y = "2 both"), subguide = sg, side = "both", n = 5) +
      stat_slabinterval(aes(y = "3 top"), subguide = sg, side = "top", n = 5)
  )

  p = df %>%
    ggplot(aes(ydist = x)) +
    scale_y_continuous(expand = expansion(add = 1))

  vdiffr::expect_doppelganger("slab subguide with side vertical",
    p +
      stat_slabinterval(aes(x = "1 left, just 0.5"), subguide = sg, side = "left", justification = 0.5, n = 5) +
      stat_slabinterval(aes(x = "2 both, just 0"), subguide = sg, side = "both", justification = 0, n = 5) +
      stat_slabinterval(aes(x = "3 right"), subguide = sg, side = "right", n = 5)
  )
})

test_that("incompatible slab subguides are detected", {
  df = data.frame(
    x = dist_normal(1:2, 1:2)
  )

  p = ggplot(df) +
    stat_slab(aes(xdist = x), normalize = "groups", subguide = "axis")

  expect_error(layer_grob(p), class = "ggdist_incompatible_subguides")
})


# dots ---------------------------------------------------------

test_that("dots subguide works with dodging", {
  skip_if_no_vdiffr()

  df = data.frame(
    x = c(dist_gamma(1:3,1:3)),
    group = c("a","a","b"),
    subgroup = c("d","e","d")
  )

  p = df %>%
    ggplot(aes(xdist = x, y = group, fill = subgroup)) +
    scale_x_continuous(expand = expansion(add = 0.25))

  vdiffr::expect_doppelganger("dots subguide with dodging",
    p + stat_dotsinterval(
      subguide = subguide_count(label_side = "outside", title = "num", theme = theme_test()),
      position = "dodgejust",
      slab_color = NA,
      height = 0.9,
      scale = 0.8,
      quantiles = 50
    )
  )
})

test_that("dots subguide works with side and justification", {
  skip_if_no_vdiffr()

  df = data.frame(x = dist_exponential(1))

  p = df %>%
    ggplot(aes(xdist = x)) +
    scale_x_continuous(expand = expansion(add = 0.3))

  sg = subguide_count(title = "num", label_side = "left")
  vdiffr::expect_doppelganger("dots subguide with side",
    p +
      stat_dotsinterval(aes(y = "1 bottom"), subguide = sg, side = "bottom", quantiles = 50, stackratio = 1.25) +
      stat_dotsinterval(aes(y = "2 both"), subguide = sg, side = "both", quantiles = 50, stackratio = 1.25) +
      stat_dotsinterval(aes(y = "3 top"), subguide = sg, side = "top", quantiles = 50, stackratio = 1.25)
  )

  p = df %>%
    ggplot(aes(ydist = x)) +
    scale_y_continuous(expand = expansion(add = 0.5))

  vdiffr::expect_doppelganger("dots subguide with side vertical",
    p +
      stat_dotsinterval(aes(x = "1 left, just 0.5"), subguide = sg, side = "left", quantiles = 10, justification = 0.5, stackratio = 0.75) +
      stat_dotsinterval(aes(x = "2 both, just 0"), subguide = sg, side = "both", quantiles = 10, justification = 0, stackratio = 0.75) +
      stat_dotsinterval(aes(x = "3 right"), subguide = sg, side = "right", quantiles = 10, stackratio = 0.75)
  )
})
