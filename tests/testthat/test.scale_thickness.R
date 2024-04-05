# Tests for thickness scales
#
# Author: mjskay
###############################################################################

library(dplyr)
library(distributional)


# scale_thickness ---------------------------------------------------------

test_that("basic scale_thickness_shared works", {
  skip_if_no_vdiffr()


  prior_post = data.frame(
    prior = dist_normal(0, 1),
    posterior = dist_normal(0, 0.5)
  )

  p = prior_post %>%
    ggplot() +
    stat_halfeye(aes(xdist = posterior), n = 10) +
    stat_slab(aes(xdist = prior), fill = NA, color = "#e41a1c", n = 10)

  vdiffr::expect_doppelganger("basic scale_thickness_identity",
    p + scale_thickness_identity()
  )

  vdiffr::expect_doppelganger("basic scale_thickness_shared",
    p + scale_thickness_shared()
  )
})

test_that("infinite thickness is squished", {
  p = ggplot() +
    geom_slab(aes(x = 1:3, thickness = thickness(c(0, Inf, -Inf)))) +
    coord_cartesian(expand = FALSE)

  geom_grob = layer_grob(p)[[1]]
  slab_grob = geom_grob$children[[1]]
  expect_equal(slab_grob$x, unit(c(0, 0.5, 1, 1, 0.5, 0), "native"))
  # Inf should be squished to 0.9 and -Inf to 0
  expect_equal(slab_grob$y, unit(c(0, 0.9, 0, 0, 0, 0), "native"))
})


# scale_type --------------------------------------------------------------

test_that("scale_type works", {
  expect_equal(scale_type(thickness(1)), "continuous")
})
