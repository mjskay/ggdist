# Tests for distribution functions
#
# Author: mjskay
###############################################################################

library(distributional)



# distributional objects --------------------------------------------------

test_that("distribution functions work on distributional objects", {
  x = dist_normal(1,2)
  expect_equal(distr_pdf(x)(-2:2), dnorm(-2:2, 1, 2))
  expect_equal(distr_cdf(x)(-2:2), pnorm(-2:2, 1, 2))
  expect_equal(distr_quantile(x)(ppoints(5)), qnorm(ppoints(5), 1, 2))

  # with subsetting
  expect_equal(distr_pdf(x[[1]])(-2:2), dnorm(-2:2, 1, 2))
  expect_equal(distr_cdf(x[[1]])(-2:2), pnorm(-2:2, 1, 2))
  expect_equal(distr_quantile(x[[1]])(ppoints(5)), qnorm(ppoints(5), 1, 2))
})
