# Tests for automatic partial functions
#
# Author: mjskay
###############################################################################


test_that("partial function printing works", {
  add1 = function(x, ...) {
    if (missing(x)) return(partial_self("add1"))
    x + 1
  }

  add1_auto = auto_partial(name = "add1", function(x, ...) {
    x + 1
  })

  expect_output(print(add1()), "<partial_function>:.*add1\\(\\)")
  expect_output(print(add1_auto()), "<partial_function>:.*add1\\(\\)")
  expect_output(print(add1(a = 2)), "<partial_function>:.*add1\\(a = 2\\)")
  expect_output(print(add1_auto(a = 2)), "<partial_function>:.*add1\\(a = 2\\)")
  expect_output(print(add1(a = 2)(a = 3, b = 4)), "<partial_function>:.*add1\\(a = 3, b = 4\\)")
  expect_output(print(add1_auto(a = 2)(a = 3, b = 4)), "<partial_function>:.*add1\\(a = 3, b = 4\\)")
  expect_equal(add1(a = 2)(a = 3, b = 4)(1), 2)
  expect_equal(add1_auto(a = 2)(a = 3, b = 4)(1), 2)
})

test_that("function bodies without braces work", {
  add1_auto = auto_partial(name = "add1", function(x, ...) {
    x + 1
  })
  add1_auto_nobrace = auto_partial(name = "add1", function(x, ...) x + 1)

  expect_identical(body(add1_auto), body(add1_auto_nobrace))
  expect_equal(add1_auto(3), 4)
  expect_equal(add1_auto_nobrace(3), 4)
})

test_that("functions without arguments work", {
  expect_equal(auto_partial(function() 5)(), 5)
})

test_that("functions inside the ggdist namespace do not inline partial_self", {
  f = function(x) x + 1
  environment(f) = asNamespace("ggdist")
  expect_identical(body(auto_partial(f, name = "f")), quote({
    if (missing(x))
      return(partial_self("f"))
    x + 1
  }))
})
