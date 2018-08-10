# Tests for prototype constructors (as_constructor)
#
# Author: mjskay
###############################################################################

context("as_constructor")


test_that("factor prototypes convert correctly", {
  constructor = tidybayes:::as_constructor.factor(factor(c("a", "b", "c")))
  expect_equal(constructor(NULL), factor(levels = c("a", "b", "c")))
  expect_equal(constructor(2), factor("b", levels = c("a", "b", "c")))
  expect_equal(constructor(c(3, 2, 1)), factor(c("c", "b", "a"), levels = c("a", "b", "c")))
  expect_equal(constructor(c(2, 2, 3, 1, 1, 2, 3)), factor(c("b", "b", "c", "a", "a", "b", "c")))
})

test_that("character prototypes convert correctly", {
  constructor = tidybayes:::as_constructor.character(c("a", "b", "c"))
  expect_equal(constructor(NULL), character(0))
  expect_equal(constructor(2), "b")
  expect_equal(constructor(c(3, 2, 1)), c("c", "b", "a"))
  expect_equal(constructor(c(2, 2, 3, 1, 1, 2, 3)), c("b", "b", "c", "a", "a", "b", "c"))
})

test_that("ordered factor prototypes convert correctly", {
  constructor = tidybayes:::as_constructor.factor(ordered(c("a", "b", "c")))
  expect_equal(constructor(NULL), ordered(NULL, levels = c("a", "b", "c")))
  expect_equal(constructor(2), ordered("b", levels = c("a", "b", "c")))
  expect_equal(constructor(c(3, 2, 1)), ordered(c("c", "b", "a"), levels = c("a", "b", "c")))
  expect_equal(constructor(c(2, 2, 3, 1, 1, 2, 3)), ordered(c("b", "b", "c", "a", "a", "b", "c")))
})

test_that("logical prototypes convert correctly", {
  constructor = tidybayes:::as_constructor.logical(TRUE)
  expect_equal(constructor(0), FALSE)
  expect_equal(constructor(1), TRUE)
  expect_equal(constructor(c(0, 1, 2)), c(FALSE, TRUE, TRUE))
})
