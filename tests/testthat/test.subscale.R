# Tests for sub-scales
#
# Author: mjskay
###############################################################################


# subscale_thickness ---------------------------------------------------------

test_that("thickness subscale works", {
  expect_equal(
    subscale_thickness(c(0, NA, Inf, -Inf)),
    thickness(c(0, NA_real_, 1, 0), NA_real_, NA_real_)
  )

  expect_equal(
    subscale_thickness(1:10),
    thickness(1:10/10, 0, 10)
  )

  expect_equal(
    subscale_thickness(c(12, 19), limits = NULL, expand = expansion(mult = 0, add = c(2, 1))),
    thickness(c(0.2, 0.9), 10, 20)
  )

  expect_equal(
    subscale_thickness(c(12, 29), limits = c(0, 10)),
    thickness(c(1.2, 2.9), 0, 10)
  )

  expect_equal(
    subscale_thickness(c(1, 11), limits = NULL, expand = expansion(mult = c(0.2, 0.3))),
    thickness(c(2/15, 12/15), -1, 14)
  )
})

test_that("identity subscale works", {
  expect_equal(
    subscale_identity(c(0, NA, Inf, -Inf)),
    thickness(c(0, NA_real_, 1, 0), NA_real_, NA_real_)
  )

  expect_equal(
    subscale_identity(c(-3:3, NA_real_, Inf, -Inf)),
    thickness(c(-3:3, NA_real_, 3, -3), NA_real_, NA_real_)
  )
})
