# Tests for automatic partial functions
#
# Author: mjskay
###############################################################################


test_that("partial function printing works", {
  add1 = function(x, ...) {
    x + 1
  }

  add1_auto = auto_partial(add1)

  expect_output(print(add1_auto()), "<auto_partial.*add1\\(\\)")
  expect_output(print(add1_auto(a = 2)), "<auto_partial.*add1\\(a = 2\\)")
  expect_output(print(add1_auto(a = 2)(a = 3, b = 4)), "<auto_partial.*add1\\(a = 3, b = 4\\)")
  expect_equal(add1_auto(a = 2)(a = 3, b = 4)(1), 2)
})

test_that("function bodies without braces work", {
  add1_auto_nobrace = auto_partial(function(x, ...) x + 1)
  expect_equal(add1_auto_nobrace(3), 4)
})

test_that("functions without arguments work", {
  expect_equal(auto_partial(function() 5)(), 5)
})

test_that("wrapper functions work", {
  f = function(x, y = 1, z = 2) {
    y + z
  }
  f = auto_partial(f)
  g = function(..., y = 2) f(..., y = y)

  expect_output(print(g(y = 2)), "<auto_partial.*f\\(y = y\\)")
  expect_equal(g(1), f(1, y = 2))
  expect_equal(g(1, y = 3, z = 4), f(1, y = 3, z = 4))
})

test_that("dots args are not prematurely evaluated", {
  f = function(x, y, z) substitute(y)
  f = auto_partial(f)
  g = function(...) {
    gz = 3
    f(z = gz, ...)
  }
  h = function(...) {
    hy = 2
    g(y = {stop("should not be evaluated"); hy}, ...)
  }
  h(x = 1)

  expect_silent(h(x = 1))
  expect_equal(h(x = 1), quote({stop("should not be evaluated"); hy}))
})

test_that("dots args are forwarded correctly", {
  f = function(x, y, z) list(x, y, z)
  f = auto_partial(f)
  g = function(...) {
    gz = 3
    f(z = gz, ...)
  }
  h = function(...) {
    hy = 2
    g(y = {print("evaluated"); hy}, ...)
  }

  expect_silent(h())
  expect_s3_class(h(), "autopartial_function")
  expect_output(
    expect_equal(h(x = 1), list(1, 2, 3)),
    "evaluated"
  )
})

test_that("waivers are detected correctly", {
  f = function(x = 1, y = 2, z = 3) list(x, y, z)
  f = auto_partial(f)
  g = function(...) {
    gz = waiver()
    f(z = gz, ...)
  }
  h = function(...) {
    g(y = waiver(), ...)
  }

  expect_equal(g(), list(1, 2, 3))
  expect_equal(h(x = waiver()), list(1, 2, 3))
})

test_that("original function names are preserved in match.call after multiple partial applications", {
  foo = function(x) match.call()
  foo = auto_partial(foo)

  expect_equal(foo()()(1), quote(foo(x = 1)))
})


# waivers -----------------------------------------------------------------

test_that("is_waiver works", {
  x = waiver()

  expect_true(is_waiver(x))
  expect_true(is_waiver(waiver()))

  expect_true(is_waiver(new_promise(quote(x))[[1]]))
  expect_true(is_waiver(new_promise(quote(waiver()))[[1]]))

  f = function(x) promise_list(x)
  g = function(y) f(y)
  h = compiler::cmpfun(function(z) g(z))
  expect_true(is_waiver(h(x)[[1]]))
  expect_true(is_waiver(h(waiver())[[1]]))
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


# promises ----------------------------------------------------------------

test_that("promise expressions are not retrieved as byte code", {
  f = function(...) {
    lapply(promise_list(...), promise_expr_)
  }
  f = auto_partial(f)
  g = compiler::cmpfun(function(...) {
    gx = 5
    f(x = gx, ...)
  })
  expect_equal(g(), list(x = quote(gx)))
})
