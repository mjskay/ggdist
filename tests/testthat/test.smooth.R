# Tests for dotplot smoothers
#
# Author: mjskay
###############################################################################


test_that("smooths on scalars work", {
  smooths = list(smooth_bounded, smooth_unbounded, smooth_density, smooth_discrete, smooth_bar, smooth_none)
  for (smooth in smooths) {
    expect_equal(!!(smooth)(numeric()), numeric())
    expect_equal(!!(smooth)(1.1), 1.1)
    expect_equal(!!(smooth)()(1.1), 1.1)
  }
})

test_that("smooth_bar works", {
  x = rep(1:4, times = 4:1)

  ld = layer_data(ggplot() + geom_dots(aes(x), smooth = "bar"))
  ref_x = c(
    (ppoints(4, a = 0.5) - 0.5) * 0.7 + 1,
    (ppoints(3, a = 0.5) - 0.5) * 0.7 + 2,
    (ppoints(2, a = 0.5) - 0.5) * 0.7 + 3,
    4
  )
  expect_equal(ld$x, ref_x)
})

test_that("smooth_discrete works", {
  x = rep(1:4, times = 4:1)

  ld = layer_data(ggplot() + geom_dots(aes(x), smooth = "discrete"))
  ref_x = c(
    0.763716218161551, 0.923059095766473, 1.08240197337139, 1.24174485097632,
    1.78694892327806, 1.99967302101091, 2.21239711874376,
    2.83777166431773, 3.15725820230644,
    3.99606829126277
  )
  expect_equal(ld$x, ref_x)

  ld = layer_data(ggplot() + geom_dots(aes(x), smooth = smooth_discrete(kernel = "ep")))
  ref_x = c(
    0.819795622358026, 0.947290763498179, 1.05728864327811, 1.18481187856615,
    1.84182750392288, 1.99962521884686, 2.15743685691928,
    2.88380665858889, 3.11164258282046,
    3.99657421511161
  )
  expect_equal(ld$x, ref_x)
})

test_that("smooth_density works", {
  x = 1:10

  ld = layer_data(ggplot() + geom_dots(aes(x), smooth = "density"))
  ref_x = c(
    1.42505862034351, 2.25017148914754, 3.11945257183347, 4.04366746501064,
    5.01016675945187, 5.98983324054813, 6.95633253498936, 7.88054742816653,
    8.74982851085246, 9.5749413796565
  )
  expect_equal(ld$x, ref_x)
})
