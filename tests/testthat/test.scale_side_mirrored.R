# Tests for scale_side_mirrored
#
# Author: mjskay
###############################################################################



test_that("basic scale_side_mirrored works", {
  trained_on = function(x, ...) {
    s = scale_side_mirrored(...)
    s$train(x)
    s
  }

  expect_equal(trained_on("a")$map(c("a","b",NA)), c("topright",NA,NA))
  expect_equal(trained_on(c("a","b"))$map(c("a","b",NA)), c("topright","bottomleft",NA))
  expect_equal(trained_on(c("a","b","c"))$map(c("a","b","c",NA)), c("topright","both","bottomleft",NA))
  expect_equal(trained_on(c("a","b","c"), start = "bottom")$map(c("a","b","c",NA)), c("bottom","both","top",NA))

  expect_error(
    trained_on(c("a","b","c"), start = "both")$map(c("a","b","c")),
    "not a valid side"
  )
  expect_error(
    trained_on(c("a","b","c","d"))$map("a"),
    "cannot be used with more than 3 levels"
  )
})
