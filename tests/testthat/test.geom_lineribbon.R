# Tests for line ribbons
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)



make_line_data = function(offset = 0, seed = 123, g = "a") {
  set.seed(seed)
  tibble(
      x = seq(0, 1, length.out = 15),
      g = g
    ) %>%
    group_by_all() %>%
    do(tibble(
      y = rnorm(500, mean = .$x + offset)
    ))
}

test_that("one-group stat_lineribbons work", {
  skip_if_no_vdiffr()


  df = make_line_data()

  p = ggplot(df, aes(x = x, y = y))

  vdiffr::expect_doppelganger("one-group stat_lineribbon",
    p + stat_lineribbon(.width = c(.50, .75, .90)) + scale_fill_brewer()
  )
  vdiffr::expect_doppelganger("one-group stat (reverse order, mean_qi)",
    p + stat_lineribbon(.width = c(.90, .75, .50), point_interval = mean_qi) + scale_fill_brewer()
  )

  vdiffr::expect_doppelganger("one-group stat_lineribbon (horizontal)",
    p + stat_lineribbon(aes(x = y, y = x), orientation = "horizontal", .width = c(.50, .75, .90)) + scale_fill_brewer()
  )

})

test_that("one-group geom_lineribbons work", {
  skip_if_no_vdiffr()


  df = make_line_data()

  vdiffr::expect_doppelganger("one-group geom_lineribbon",
    df %>%
      group_by(x, g) %>%
      mode_qi(y, .width = c(.50, .75, .90)) %>%
      ggplot(aes(x = x, y = y, ymin = .lower, ymax = .upper)) +
      geom_lineribbon() +
      scale_fill_brewer()
  )
  vdiffr::expect_doppelganger("one-group geom_lineribbon (reverse order)",
    df %>%
      group_by(x, g) %>%
      mean_hdi(y, .width = c(.50, .75, .90)) %>%
      ggplot(aes(x = x, y = y, ymin = .lower, ymax = .upper)) +
      geom_lineribbon() +
      scale_fill_brewer()
  )
  vdiffr::expect_doppelganger("one-group geom_lineribbon (manual aes)",
    df %>%
      group_by(x, g) %>%
      mean_qi(y, .width = c(.50, .75, .90)) %>%
      ggplot(aes(x = x, y = y, ymin = .lower - 10, ymax = .upper)) +
      geom_lineribbon() +
      scale_fill_brewer()
  )
  vdiffr::expect_doppelganger("one-group geom_lineribbon (horizontal)",
    df %>%
      group_by(x, g) %>%
      mode_qi(y, .width = c(.50, .75, .90)) %>%
      rename(l = .lower, u = .upper) %>%
      ggplot(aes(x = y, y = x, xmin = l, xmax = u)) +
      geom_lineribbon() +
      scale_fill_brewer()
  )
})

test_that("geom_lineribbons unspecified fill and color work", {
  skip_if_no_vdiffr()

  p = tibble(x = 1:3, lower = 0:2, upper = 2:4) %>%
    ggplot(aes(x = x, y = x, ymin = lower, ymax = upper))

  vdiffr::expect_doppelganger("lineribbon without .width",
    p + geom_lineribbon()
  )

  vdiffr::expect_doppelganger("lineribbon fill = NA",
    p + geom_lineribbon(fill = NA)
  )

  vdiffr::expect_doppelganger("lineribbon color = NA",
    p + geom_lineribbon(color = NA)
  )

})

test_that("two-group stat_lineribbons work", {
  skip_if_no_vdiffr()


  df = bind_rows(
    make_line_data(),
    make_line_data(offset = 4, seed = 234, g = "b")
  ) %>%
    arrange(x)

  p = ggplot(df, aes(x = x, y = y))

  vdiffr::expect_doppelganger("two-group stat_lineribbons grouped by group",
    p +
    stat_lineribbon(aes(group = g), .width = c(.50, .75, .90)) + scale_fill_brewer()
  )
  vdiffr::expect_doppelganger("two-group stats grouped by color and linetype",
    p +
    stat_lineribbon(aes(color = g, linetype = g), .width = c(.50, .75, .90)) + scale_fill_brewer() +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2), linetype = guide_legend(order = 2))
  )
  vdiffr::expect_doppelganger("two-group stats grouped by fill with ramp",
    p +
      stat_lineribbon(aes(fill = g, fill_ramp = stat(level)), .width = c(.50, .75, .90)) +
      # testing range = c(0,1) here because it ensures both groups ramp all the
      # way to the same color ("gray85"), which will cause problems if the ribbons
      # are not properly grouped by the original (un-ramped) color as well.
      scale_fill_ramp_discrete(from = "gray85", range = c(0, 1))
  )

})

test_that("stat_dist_lineribbon works", {
  skip_if_no_vdiffr()


  p = tibble(
    x = 1:10,
    sd = seq(1, 3, length.out = 10),
    g = rep(c("a","b"), 5)
  ) %>%
    ggplot(aes(x = x, dist = "norm", arg1 = x, arg2 = sd))

  vdiffr::expect_doppelganger("basic stat_dist_lineribbon",
    p + stat_dist_lineribbon() + scale_fill_brewer()
  )

  vdiffr::expect_doppelganger("two group stat_dist_lineribbon",
    p + stat_dist_lineribbon(aes(arg1 = x + ifelse(g == "a", 6, 0), fill = g, color = g), alpha = 1/4)
  )
})

test_that("the stepped lineribbons work", {
  skip_if_no_vdiffr()


  p = data.frame(x = 1:5) %>%
    ggplot(aes(x = x, dist = "norm", arg1 = x))

  vdiffr::expect_doppelganger("lineribbon with step = TRUE works",
    p + stat_dist_lineribbon(step = TRUE) + geom_point(aes(y = x), size = 3)
  )

  vdiffr::expect_doppelganger("lineribbon with step = hv works",
    p + stat_dist_lineribbon(step = "hv") + geom_point(aes(y = x), size = 3)
  )

  vdiffr::expect_doppelganger("lineribbon with step = vh works",
    p + stat_dist_lineribbon(step = "vh") + geom_point(aes(y = x), size = 3)
  )

  vdiffr::expect_doppelganger("lineribbon with step = mid works",
    p + stat_dist_lineribbon(step = "mid") + geom_point(aes(y = x), size = 3)
  )

})



# stat_ribbon -------------------------------------------------------------

test_that("stat_ribbon works", {
  skip_if_no_vdiffr()


  df = make_line_data()

  p = ggplot(df, aes(x = x, y = y))

  vdiffr::expect_doppelganger("one-group stat_ribbon",
    p + stat_ribbon(.width = c(.50, .75, .90)) + scale_fill_brewer()
  )
})



# geom_lineribbon without line --------------------------------------------

test_that("geom_lineribbon without line works", {
  skip_if_no_vdiffr()

  df = tibble(
    x = 1:10,
    y = (1:10)^2/2,
    lower = y - 10/(10:1),
    upper = y + 10/(10:1)
  )

  vdiffr::expect_doppelganger("geom_lineribbon without line",
    df %>%
      ggplot(aes(x = x, ymin = lower, ymax = upper)) +
      geom_lineribbon()
  )

  vdiffr::expect_doppelganger("geom_lineribbon without line, vertical",
    df %>%
      ggplot(aes(y = x, xmin = lower, xmax = upper)) +
      geom_lineribbon()
  )

})



