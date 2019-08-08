# Tests for geom_slabinterval
#
# Author: mjskay
###############################################################################

library(dplyr)

context("geom_slabinterval")


# group_slab_data_by_colour -------------------------------------------------

test_that("group_slab_data_by works", {

  df = data.frame(
    x = 1:8,
    ymin = 2:9,
    ymax = 3:10,
    fill = c("a","a","a","b","b","b","b","a"),
    alpha = 1
  )

  ref = data.frame(
    x =    c(1:3, 3.5, 3.5, 3:1,   3.5, 4:7, 7.5, 7.5, 7:4, 3.5,    7.5, 8, 8, 7.5),
    ymin = c(1:3, 3.5, 3.5, 3:1,   3.5, 4:7, 7.5, 7.5, 7:4, 3.5,    7.5, 8, 8, 7.5) + 1,
    ymax = c(1:3, 3.5, 3.5, 3:1,   3.5, 4:7, 7.5, 7.5, 7:4, 3.5,    7.5, 8, 8, 7.5) + 2,
    fill = c(rep("a", 8), rep("b", 12), rep("a", 4)),
    alpha = 1,
    group = c(rep(0, 8), rep(1, 12), rep(2,4)),
    y    = c(3:5, 5.5, 4.5, 4:2,   5.5, 6:9, 9.5, 8.5, 8:5, 4.5,    9.5, 10, 9, 8.5)
  )

  grouped_slab_data = arrange(group_slab_data_by(df, side = "both"), group)
  expect_equal(grouped_slab_data, ref)

  df$fill = "a"
  expect_equal(group_slab_data_by(df, side = "top"), mutate(df, y = ymax))

})


test_that("group_slab works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  p = tibble(
    x = seq(-4,4, length.out = 50),
    d = dnorm(x)
  ) %>%
    ggplot(aes(thickness = d))

  vdiffr::expect_doppelganger("geom_slab one group",
    p + geom_slab(aes(x = 1, y = x))
  )

  vdiffr::expect_doppelganger("geom_slabh one group",
    p + geom_slabh(aes(x = x, y = 1))
  )

})


