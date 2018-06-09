# Tests for parameters
#
# Author: mjskay
###############################################################################

context("parameters")


test_that("basic parameters extraction works", {
  data(RankCorr, package = "tidybayes")

  ref = dimnames(RankCorr)[[2]]

  expect_equal(parameters(RankCorr), ref)
})
