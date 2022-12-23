# Tests for automatic partial functions
#
# Author: mjskay
###############################################################################


test_that("partial function printing works", {
  add1 = function(x, ...) {
    if (missing(x)) return(partial_self("add1"))
    x + 1
  }

  expect_output(print(add1()), "<partial_function>:.*add1\\(\\)")
  expect_output(print(add1(a = 2)), "<partial_function>:.*add1\\(a = 2\\)")
  expect_output(print(add1(a = 2)(a = 3, b = 4)), "<partial_function>:.*add1\\(a = 3, b = 4\\)")
  expect_equal(add1(a = 2)(a = 3, b = 4)(1), 2)
})
