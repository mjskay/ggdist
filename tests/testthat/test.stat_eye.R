# Tests for eye plots
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)

context("stat_eye")

test_that("one-parameter eye plots work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  set.seed(123)
  df = data.frame(x = rnorm(1000), y = 1)

  # using geom_[half]eyeh here to test deprecated aliases
  p = ggplot(df, aes(x = x, y = y))
  vdiffr::expect_doppelganger("one-parameter horizontal eye", p + geom_eyeh(n = 20))
  vdiffr::expect_doppelganger("one-parameter horizontal half-eye", p + geom_halfeyeh(n = 20))

  # using geom_eye here to test deprecated alias (there never was a geom_halfeye())
  p = ggplot(df, aes(x = y, y = x))
  vdiffr::expect_doppelganger("one-parameter vertical eye", p + geom_eye(n = 20))
  vdiffr::expect_doppelganger("one-parameter vertical halfeye", p + stat_halfeye(n = 20))

  p = ggplot(df, aes(x = x, y = y))
  vdiffr::expect_doppelganger("one-parameter horizontal eye (mode_hdi)", p + stat_eyeh(point_interval = mode_hdi, n = 20))

})


test_that("two-parameter eye plots work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  set.seed(123)
  df = data.frame(x = rnorm(1000), y = "a", y_int = 1) %>%
    rbind(data.frame(x = rnorm(1000, 1), y = "b", y_int = 2))

  p = ggplot(df, aes(x = x, y = y))
  vdiffr::expect_doppelganger("two-parameter (factor) horizontal half-eye", p + stat_halfeyeh(scale = 0.5, n = 20))
  vdiffr::expect_doppelganger("two-parameter (factor) horizontal eye (fill)", p + stat_eyeh(aes(fill = y), scale = 0.5, n = 20))

  p = ggplot(df, aes(x = y, y = x))
  vdiffr::expect_doppelganger("two-parameter (factor) vertical eye (fill)", p + stat_eye(aes(fill = y), scale = 0.5, n = 20))

  p = ggplot(df, aes(x = x, y = y_int))
  vdiffr::expect_doppelganger("two-parameter (numeric) horizontal eye", p + stat_eyeh(fatten_point = 3, n = 20))
  vdiffr::expect_doppelganger("two-parameter (numeric) horizontal half-eye (fill)", p + stat_halfeyeh(aes(fill = y_int), fatten_point = 3, n = 20, show.legend = c(size = FALSE)))

  p = ggplot(df, aes(x = y_int, y = x))
  vdiffr::expect_doppelganger("two-parameter (numeric) vertical eye", p + stat_eye(fatten_point = 3, n = 20))

})


test_that("dodged eye plots work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  set.seed(123)
  df = data.frame(y = rnorm(500, 1), x = "a", g = c("g1")) %>%
    rbind(data.frame(y = rnorm(900), x = "b", g = c("g1", "g2", "g3")))

  p = ggplot(df, aes(x = x, y = y))

  vdiffr::expect_doppelganger("vertical halfeye with dodging (3 groups, right)",
    p + stat_halfeye(aes(fill = g), position = "dodge", n = 20))
  vdiffr::expect_doppelganger("vertical halfeye with dodging (3 groups, left)",
    p + stat_halfeye(aes(fill = g), position = "dodge", side = "bottom", n = 20))
  vdiffr::expect_doppelganger("vertical eye with dodging (3 groups, left)",
    p + stat_eye(aes(fill = g), position = "dodge", n = 20))

  vdiffr::expect_doppelganger("vertical eye with dodging (3 groups, just = 0)",
    p + stat_eye(aes(fill = g), position = "dodge", justification = 0, n = 20))

  vdiffr::expect_doppelganger("vertical eye with dodging (3 groups, just = 1, top, scale = 0.5)",
    p + stat_halfeye(aes(fill = g), position = "dodge", justification = 1, side = "top", scale = 0.5, n = 20))

  vdiffr::expect_doppelganger("vertical eye with dodging (3 groups, just = 0.5, top, scale = 0.5)",
    p + stat_halfeye(aes(fill = g), position = "dodge", justification = 0.5, side = "top", scale = 0.5, n = 20))

  vdiffr::expect_doppelganger("vertical halfeye with dodging (3 groups, just = 0, top, scale = 1.5)",
    p + stat_halfeye(aes(fill = g), position = "dodge", side = "top", scale = 1.5, n = 20))

  vdiffr::expect_doppelganger("vertical halfeye with dodging (3 groups, just = 0, both, scale = 1.5)",
    p + stat_halfeye(aes(fill = g), position = "dodge", side = "both", scale = 1.5, n = 20))

  vdiffr::expect_doppelganger("halfeyeh with dodge (3 groups, just = 0, both, scale = 4, rev y)",
    p + stat_halfeyeh(aes(x = y, y = forcats::fct_rev(x), fill = g), position = ggstance::position_dodgev(), side = "top", scale = 4, slab_color = "black", n = 20)
  )

  vdiffr::expect_doppelganger("vertical halfeye with dodging (3 groups, just = 0, bottom, scale = 1.5)",
    p + stat_halfeye(aes(fill = g), position = "dodge", side = "bottom", scale = 1.5, n = 20))

})
