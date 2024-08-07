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
  add1_auto_nobrace = auto_partial(name = "add1", function(x, ...) x + 1)
  expect_equal(add1_auto_nobrace(3), 4)
})

test_that("functions without arguments work", {
  expect_equal(auto_partial(function() 5)(), 5)
})

test_that("functions inside the ggdist namespace do not inline partial_self", {
  f = function(x) {
    x + 1
  }
  environment(f) = asNamespace("ggdist")
  expect_match(deparse0(body(auto_partial(f, name = "f"))), 'partial_self("f", waivable = TRUE)', fixed = TRUE)
})

test_that("wrapper functions work", {
  f = auto_partial(name = "f", function(x, y = 1, z = 2) {
    y + z
  })
  g = function(..., y = 2) f(..., y = y)

  expect_output(print(g(y = 2)), "<partial_function>:.*f\\(y = y\\)")
  expect_equal(g(1), f(1, y = 2))
  expect_equal(g(1, y = 3, z = 4), f(1, y = 3, z = 4))
})

test_that("original function names are preserved in match.call after multiple partial applications", {
  foo = auto_partial(function(x) as.call(lapply(match.call(), get_expr)))

  expect_equal(foo()()(1), quote(foo(x = 1)))
})

test_that("waivers work", {
  foo = auto_partial(function(x, a = 2) c(x, a))

  expect_equal(foo(a = waiver())(1), c(1, 2))
  expect_equal(foo(1, a = waiver()), c(1, 2))
  expect_equal(foo(a = waiver())(x = 1), c(1, 2))
  expect_equal(foo(a = waiver())(a = 4)(x = 1), c(1, 4))
  expect_equal(foo(a = 4)(a = waiver())(x = 1), c(1, 4))

  foo = auto_partial(function(x, y, a = 3, b = 4) c(x, y, a, b))

  expect_equal(foo(a = waiver(), b = 5)(1)(y = -2, b = waiver()), c(1, -2, 3, 5))
})
