# Tests for parameters
#
# Author: mjskay
###############################################################################

context("get_variables")


test_that("basic variable extraction works", {
  data(RankCorr, package = "tidybayes")

  ref = dimnames(RankCorr)[[2]]

  expect_equal(get_variables(RankCorr), ref)
})
