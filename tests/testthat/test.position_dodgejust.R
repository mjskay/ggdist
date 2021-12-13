# Tests for position_dodgejust
#
# Author: mjskay
###############################################################################

library(dplyr)
library(distributional)



test_that("position_dodgejust works", {
  skip_if_no_vdiffr()

  dist_df = tribble(
    ~group, ~subgroup, ~mean, ~sd,
    1,          "h",     5,   1,
    2,          "h",     7,   1.5,
    3,          "h",     8,   1,
    3,          "i",     9,   1,
    3,          "j",     7,   1
  )

  just_test_plot = function(just = 0, side = "right") {
    dist_df %>%
      ggplot(aes(
        x = factor(group), dist = dist_normal(mean, sd),
        fill = interaction(group, subgroup)
      )) +
      stat_dist_halfeye(
        position = "dodgejust",
        n = 10,
        side = side
      ) +
      geom_rect(
        aes(xmin = group - just, xmax = group + 1 - just, ymin = 2, ymax = 13, color = interaction(group, subgroup)),
        position = "dodgejust",
        alpha = 0.1
      ) +
      geom_point(
        aes(x = group, y = 7.5, color = interaction(group, subgroup)),
        position = position_dodgejust(width = 1, justification = just),
        shape = 1,
        size = 4,
        stroke = 1.5
      ) +
      scale_fill_brewer(palette = "Set2") +
      scale_color_brewer(palette = "Dark2")
  }


  vdiffr::expect_doppelganger("right halfeye w point and rect",
    just_test_plot(just = 0, side = "right")
  )
  vdiffr::expect_doppelganger("left halfeye w point and rect",
    just_test_plot(just = 1, side = "left")
  )
  vdiffr::expect_doppelganger("both halfeye w point and rect",
    just_test_plot(just = 0.5, side = "both")
  )
})


# zero-width geoms --------------------------------------------------------

test_that("position_dodgejust works on zero-width geoms", {
  skip_if_no_vdiffr()


  expect_warning(vdiffr::expect_doppelganger("zero-width dodgejust",
    data.frame(x = c(1,1), y = "a", group = c("a","b")) %>%
      ggplot(aes(x = x, y = y, color = group, shape = group)) +
      geom_point(position = position_dodgejust(), size = 4) +
      scale_shape_manual(values = c(0, 3))
  ), "Width not defined. Set with `position_dodgejust\\(width = \\?\\)`")
})


# missing y/ymax --------------------------------------------------------------

test_that("position_dodgejust warns on missing y/ymax", {
  expect_error(ggplot_build(
    data.frame(xmin = 1) %>%
      ggplot(aes(xmin = xmin)) +
      geom_blank(position = position_dodgejust(width = 1))
  ), "Neither y nor ymax defined")
})


# overlapping intervals ---------------------------------------------------

test_that("position_dodgejust warns on overlapping intervals", {
  expect_warning(ggplot_build(
    data.frame(y = 1:2, x = factor("a")) %>%
      ggplot(aes(x = x, y = y,
        xmin = 0.5 - 0.1 * y,
        xmax = 1.5 - 0.1 * y,
        color = factor(y)
      )) +
      geom_point(
        position = position_dodgejust()
      )
  ), "position_dodgejust requires non-overlapping x intervals")

  expect_warning(ggplot_build(
    data.frame(y = 1:2, x = factor("a")) %>%
      ggplot(aes(y = x, x = y,
        ymin = 0.5 - 0.1 * y,
        ymax = 1.5 - 0.1 * y,
        color = factor(y)
      )) +
      geom_point(
        position = position_dodgejust()
      )
  ), "position_dodgejust requires non-overlapping y intervals")
})
