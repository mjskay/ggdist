# Tests for line ribbons
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)

context("geom_lineribbon")

make_line_data = function(offset = 0, seed = 123, g = "a") {
  set.seed(seed)
  tibble(
      x = seq(0, 1, length.out = 25),
      g = g
    ) %>%
    group_by_all() %>%
    do(tibble(
      y = rnorm(500, mean = .$x + offset)
    ))
}

test_that("one-group stat_lineribbons work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  df = make_line_data()

  p = ggplot(df, aes(x = x, y = y))

  vdiffr::expect_doppelganger("one-group stat_lineribbon", p + stat_lineribbon(.width = c(.50, .75, .90)) + scale_fill_brewer())
  vdiffr::expect_doppelganger("one-group stat_lineribbon (reverse order)", p + stat_lineribbon(.width = c(.90, .75, .50)) + scale_fill_brewer())

  vdiffr::expect_doppelganger("one-group stat_lineribbon (mean_qi)",
    p + stat_lineribbon(.width = c(.50, .75, .90), point_interval = mean_qi) + scale_fill_brewer())
})

test_that("one-group geom_lineribbons work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  df = make_line_data()

  vdiffr::expect_doppelganger("one-group geom_lineribbon", df %>%
      group_by(x, g) %>%
      mode_qi(y, .width = c(.50, .75, .90)) %>%
      ggplot(aes(x = x, y = y)) +
      geom_lineribbon() +
      scale_fill_brewer()
    )
  vdiffr::expect_doppelganger("one-group geom_lineribbon (reverse order)", df %>%
      group_by(x, g) %>%
      mean_hdi(y, .width = c(.50, .75, .90)) %>%
      ggplot(aes(x = x, y = y)) +
      geom_lineribbon() +
      scale_fill_brewer()
  )
})

test_that("two-group stat_lineribbons work", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  df = bind_rows(
    make_line_data(),
    make_line_data(offset = 4, seed = 234, g = "b")
  ) %>%
    arrange(x)

  p = ggplot(df, aes(x = x, y = y))

  vdiffr::expect_doppelganger("two-group stat_lineribbons grouped by group", p +
      stat_lineribbon(aes(group = g), .width = c(.50, .75, .90)) + scale_fill_brewer())
  vdiffr::expect_doppelganger("two-group stat_lineribbons grouped by linetype",
    p +
    stat_lineribbon(aes(linetype = g), .width = c(.50, .75, .90)) + scale_fill_brewer() +
    guides(fill = guide_legend(order = 1), linetype = guide_legend(order = 2))
  )
  vdiffr::expect_doppelganger("two-group stat_lineribbons grouped by color",
    p +
    stat_lineribbon(aes(color = g), .width = c(.50, .75, .90)) + scale_fill_brewer() +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2))
  )
  vdiffr::expect_doppelganger("two-group stat_lineribbons grouped by color and linetype",
    p +
    stat_lineribbon(aes(color = g, linetype = g), .width = c(.50, .75, .90)) + scale_fill_brewer() +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2), linetype = guide_legend(order = 2))
  )
})
