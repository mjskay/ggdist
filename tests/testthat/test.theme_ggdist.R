# Tests for themes
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)



# test_that("theme helper functions work", {
#   skip("skipping theme tests, can check manually")
#
#   skip_if_no_vdiffr()
#
#
#   p = data.frame(
#     x = 1:2,
#     y = 0,
#     g = c("aaa","bbb")
#   ) %>%
#     ggplot(aes(x, y)) +
#     geom_point() +
#     axis_titles_bottom_left()
#
#   vdiffr::expect_doppelganger("facet titles on right", {
#     p + facet_grid(g ~ .) + facet_title_horizontal()
#   })
#
#   vdiffr::expect_doppelganger("facet titles on left", {
#     p + facet_grid(g ~ ., switch = "y") + facet_title_horizontal()
#   })
#
# })
