# Tests for dotplot smoothers
#
# Author: mjskay
###############################################################################


test_that("smooths on scalars work", {
  smooths = list(smooth_bounded, smooth_unbounded, smooth_discrete, smooth_bar, smooth_none)
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

test_that("smooth_bounded works", {
  x = 1:10

  ld = layer_data(ggplot() + geom_dots(aes(x), smooth = "bounded"))
  ref_x = c(
    1.00658549280008, 2.00323115445067, 3.00120376176176, 4.00033203047236,
    5.00005763903296, 5.99994236096704, 6.99966796952764, 7.99879623823824,
    8.99676884554933, 9.99341450719992
  )
  expect_equal(ld$x, ref_x)
})
