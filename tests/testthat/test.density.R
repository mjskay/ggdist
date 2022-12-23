# Tests for density estimators
#
# Author: mjskay
###############################################################################


# drop the portion of a density() object that depends on the input function
# name (to make comparison across functions easier)
drop_names = function(x) {
  x[!names(x) %in% "call"]
}


test_that("density_auto works", {
  x = 1:10

  expect_equal(density_auto(adjust = 0.5)(x), density_auto(x, adjust = 0.5))
  expect_equal(drop_names(density_auto(x, trim = TRUE)), drop_names(density_bounded(x)))
  expect_equal(drop_names(density_auto(x, trim = FALSE)), drop_names(density_unbounded(x)))
})

test_that("density_bounded works", {
  x = 1:10

  expect_equal(density_bounded(adjust = 0.5)(x), density_bounded(x, adjust = 0.5))
  # density for Uniform(1, 10) is 1/9, estimator should be close-ish to that
  expect_true(all(abs(density_bounded(x)$y - 1/9) < .013))
})

test_that("density_unbounded works", {
  x = 1:10

  expect_equal(density_unbounded(adjust = 0.5)(x), density_unbounded(x, adjust = 0.5))
  expect_equal(drop_names(density_unbounded(x)), drop_names(density(x)))
})
