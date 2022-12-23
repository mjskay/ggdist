# Tests for probability expressions
#
# Author: mjskay
###############################################################################

test_that("CDFs and CCDFs from Pr_() work", {
  expect_equal(Pr_(X < x), quote(after_stat(cdf)))
  expect_equal(Pr_(xdist <= x), quote(after_stat(cdf)))
  expect_equal(Pr_(y > ydist), quote(after_stat(cdf)))
  expect_equal(Pr_(y >= Y), quote(after_stat(cdf)))
  expect_equal(Pr_(dist < value), quote(after_stat(cdf)))

  expect_equal(Pr_(X > x), quote(after_stat(1 - cdf)))
  expect_equal(Pr_(xdist >= x), quote(after_stat(1 - cdf)))
  expect_equal(Pr_(y < ydist), quote(after_stat(1 - cdf)))
  expect_equal(Pr_(y <= Y), quote(after_stat(1 - cdf)))
  expect_equal(Pr_(dist > value), quote(after_stat(1 - cdf)))

  expect_error(Pr_(x < x))
  expect_error(Pr_(x < y))
  expect_error(Pr_(x < dist))
  expect_error(Pr_(x < value))
})

test_that("Pr_(... %in% ...) works", {
  expect_equal(Pr_(x %in% interval), quote(after_stat(.width)))
  expect_equal(Pr_(y %in% interval), quote(after_stat(.width)))
  expect_equal(Pr_(xdist %in% interval), quote(after_stat(.width)))
  expect_equal(Pr_(ydist %in% interval), quote(after_stat(.width)))
  expect_equal(Pr_(X %in% interval), quote(after_stat(.width)))
  expect_equal(Pr_(Y %in% interval), quote(after_stat(.width)))
  expect_equal(Pr_(dist %in% interval), quote(after_stat(.width)))
  expect_equal(Pr_(value %in% interval), quote(after_stat(.width)))
})

test_that("p_(...) works", {
  expect_equal(p_(x), quote(after_stat(pdf)))
  expect_equal(p_(y), quote(after_stat(pdf)))
  expect_equal(p_(value), quote(after_stat(pdf)))
})

test_that("incorrect probability expressions are caught", {
  expect_error(Pr_(y %in% dist), "Unrecognized probability expression")
  expect_error(Pr_(X > interval), "Invalid combination")
  expect_error(p_(interval), "Unrecognized probability expression")
  expect_error(Pr_(g), "Unrecognized probability expression")
})
