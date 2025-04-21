# Tests for interval_widths
#
# Author: mjskay
###############################################################################


test_that("interval_widths works", {
  expect_equal(interval_widths(0), numeric())
  expect_equal(interval_widths(1), 0.9)
  expect_equal(interval_widths(2), (pnorm(c(qnorm(0.975)/2, qnorm(0.975))) - 0.5) * 2)

  expect_equal(pretty_widths(0), numeric())
  expect_equal(pretty_widths(1), 0.95)
  expect_equal(pretty_widths(2), c(0.65, 0.95))
  expect_equal(pretty_widths(3), c(0.50, 0.80, 0.95))
  expect_length(pretty_widths(100), 100)
})
