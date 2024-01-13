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


# scale_type --------------------------------------------------------------

test_that("scale_type works", {
  expect_equal(scale_type(thickness(1)), "continuous")
})


# thickness type ----------------------------------------------------------

test_that("thickness formatting works", {
  expect_equal(vec_ptype_full(thickness()), "thickness")
  expect_equal(vec_ptype_abbr(thickness()), "thk")
  expect_equal(format(thickness()), character())
  expect_equal(format(thickness(1:2, 3, 4)), c("1thk [3,4]", "2thk [3,4]"))
})

test_that("thickness casting works", {
  expect_equal(vec_cast(thickness(2), double()), 2.0)
  expect_equal(vec_cast(thickness(2L), integer()), 2L)
  expect_equal(vec_cast(2.0, thickness()), thickness(2))
  expect_equal(vec_cast(2L, thickness()), thickness(2))

  expect_equal(c(thickness(1), thickness(2)), thickness(c(1, 2)))
  expect_equal(c(thickness(1), 2), thickness(c(1, 2)))
  expect_equal(c(thickness(1), 2L), thickness(c(1, 2)))

  expect_equal(vec_c(2, thickness(1)), thickness(c(2, 1)))
  expect_equal(vec_c(2L, thickness(1)), thickness(c(2, 1)))

  expect_error(thickness(1) + character())

  expect_equal(thickness(1) + thickness(2), thickness(3))
  expect_equal(thickness(1) - thickness(2), thickness(-1))
  expect_equal(thickness(1) / thickness(2), 0.5)
  expect_error(thickness(1) * thickness(2))

  expect_equal(thickness(2) * 3, thickness(6))
  expect_equal(thickness(1) / 2, thickness(0.5))
  expect_error(thickness(1) - 2)
  expect_error(thickness(1) + 2)

  expect_equal(2 * thickness(3), thickness(6))
  expect_error(1 / thickness(2))
  expect_error(1 - thickness(2))
  expect_error(1 + thickness(2))
})

test_that("thickness compatibility testing works", {
  expect_equal(thickness(1) + thickness(2), thickness(3))
  expect_equal(thickness(1,1,NA) + thickness(2,NA,2), thickness(3,1,2))
  expect_equal(thickness(1,1,2) + thickness(2,1,2), thickness(3,1,2))

  expect_error(thickness(1,1,2) + thickness(2,0,2), class = "ggdist_incompatible_thickness_bounds")
  expect_error(thickness(1,1,0) + thickness(2,1,2), class = "ggdist_incompatible_thickness_bounds")
})
