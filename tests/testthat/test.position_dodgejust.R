# Tests for position_dodgejust
#
# Author: mjskay
###############################################################################

library(dplyr)




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


  vdiffr::expect_doppelganger("halfeye w point and rect refs",
    dist_df %>%
      ggplot(aes(
        x = factor(group), dist = dist_normal(mean, sd),
        fill = interaction(group, subgroup)
      )) +
      stat_dist_halfeye(
        position = "dodgejust"
      ) +
      geom_rect(
        aes(xmin = group, xmax = group + 1, ymin = 2, ymax = 13, color = interaction(group, subgroup)),
        position = "dodgejust",
        alpha = 0.1
      ) +
      geom_point(
        aes(x = group, y = 7.5, color = interaction(group, subgroup)),
        position = position_dodgejust(width = 1, justification = 0),
        shape = 1,
        size = 4,
        stroke = 1.5
      ) +
      scale_fill_brewer(palette = "Set2") +
      scale_color_brewer(palette = "Dark2")
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
