# Tests for density estimators
#
# Author: mjskay
###############################################################################


# drop the portion of a density() object that depends on the input function
# call expression (to make comparison across functions easier)
drop_call = function(x) {
  x[!names(x) %in% "call"]
}


test_that("density_auto works", {
  x = 1:10

  expect_equal(density_auto(adjust = 0.5)(x), density_auto(x, adjust = 0.5))
  expect_equal(drop_call(density_auto(x, trim = TRUE)), drop_call(density_bounded(x)))
  expect_equal(drop_call(density_auto(x, trim = FALSE)), drop_call(density_unbounded(x)))
})

test_that("density_bounded works", {
  x = 1:1000

  expect_equal(density_bounded(adjust = 0.5)(x), density_bounded(x, adjust = 0.5))
  # density for Uniform(1, 1000) is 1/999, estimator should be close-ish to that
  expect_true(all(abs(density_bounded(x)$y - 1/999) < .0001))
  expect_true(all(abs(density_bounded(x, bounder = "cooke")$y - 1/999) < .0001))
  expect_true(all(abs(density_bounded(x, bounder = "range")$y - 1/999) < .0001))
  expect_true(all(abs(density_bounded(x, bounds = c(1,1000))$y - 1/999) < .0001))

  expect_error(density_bounded(1:2, n = 0), "`n` of at least 1")
  expect_error(density_bounded(1:4, bounds = c(1,2)), "`x` must be inside `bounds`")
})

test_that("density_unbounded works", {
  x = 1:10

  expect_equal(density_unbounded(adjust = 0.5)(x), density_unbounded(x, adjust = 0.5))

  ref = density(x)
  ref$cdf = ecdf(x)(ref$x)
  expect_equal(drop_call(density_unbounded(x)), drop_call(ref))

  expect_error(density_unbounded(1:2, n = 0), "`n` of at least 1")
})

test_that("density_bounded(range_only = TRUE) works", {
  expect_equal(
    density_bounded(range_only = TRUE, trim = TRUE, bounder = bounder_cooke())(1:2),
    list(x = 1:2, y = c(NA_real_, NA_real_))
  )

  expect_equal(
    density_bounded(range_only = TRUE, trim = FALSE, bounder = bounder_cooke())(1:2),
    list(x = c(0.75, 2.25), y = c(NA_real_, NA_real_))
  )
})
