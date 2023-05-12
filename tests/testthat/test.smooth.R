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
  ref_x = c(0.760112695656659, 0.920256356040445, 1.08040001642423, 1.24054367680802,  1.78654853188863, 2.00007341240034, 2.21359829291206, 2.83977362126489,  3.16006094203247, 3.99967181376766)
  expect_equal(ld$x, ref_x, tolerance = 0.001)

  ld = layer_data(ggplot() + geom_dots(aes(x), smooth = smooth_discrete(kernel = "ep")))
  ref_x = c(0.816364320617873, 0.944625373470495, 1.05539072151476, 1.18366670884155,  1.84144365484769, 2.00000507797274, 2.15857922335003, 2.88568947241406,  3.11432520840337, 3.99999280566278)
  expect_equal(ld$x, ref_x, tolerance = 0.001)
})

test_that("smooth_bounded works", {
  x = 1:10

  ld = layer_data(ggplot() + geom_dots(aes(x), smooth = "bounded"))
  ref_x = c(0.997794316080475, 1.99639357461413, 2.99631978598422, 3.99740164666058,  4.99908084276668, 6.00091915723332, 7.00259835333942, 8.00368021401578,  9.00360642538587, 10.0022056839195)
  expect_equal(ld$x, ref_x, tolerance = 0.001)
})
