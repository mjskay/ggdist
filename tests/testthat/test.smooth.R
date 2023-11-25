# Tests for dotplot smoothers
#
# Author: mjskay
###############################################################################


test_that("smooths on scalars work", {
  smooths = list(smooth_bounded, smooth_unbounded, smooth_discrete, smooth_bar, smooth_none)
  for (smooth in smooths) {
    expect_equal(!!(smooth)(numeric()), numeric())
    expect_equal(!!(smooth)(1.1), 1.1)
    expect_equal(!!(smooth)()(1.1), 1.1)
  }
})

test_that("smooth_bar works", {
  skip_if_no_vdiffr()

  df = data.frame(
    x = factor(c(rep(1:5, times = 5:1 * 11), 6, 6, 7)),
    g = c("a","b")
  )

  vdiffr::expect_doppelganger("smooth_bar with order",
    df %>%
      ggplot(aes(x, fill = g, group = NA, order = g)) +
      geom_dots(smooth = "bar")
  )
})

test_that("smooth_discrete works", {
  skip_if_no_vdiffr()

  df = data.frame(
    x = factor(c(rep(1:5, times = 5:1 * 11), 6, 6, 7)),
    g = c("a","b")
  )

  vdiffr::expect_doppelganger("smooth_discrete gaussian",
    df %>%
      ggplot(aes(x, fill = g, group = NA, order = g)) +
      geom_dots(smooth = "discrete")
  )

  vdiffr::expect_doppelganger("smooth_discrete ep mirrored",
    df %>%
      ggplot(aes(x, fill = g, group = NA, order = g)) +
      geom_dots(smooth = smooth_discrete(kernel = "ep"), side = "both")
  )
})
