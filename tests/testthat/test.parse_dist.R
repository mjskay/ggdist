# Tests for parse_dist
#
# Author: mjskay
###############################################################################

library(dplyr)

context("parse_dist")

test_that("parse_dist works on vectors", {
  # using as.data.frame here because comparison of tibbles with
  # list columns directly doesn't seem to work...
  expect_equal(
    as.data.frame(parse_dist(c("Normal(0,1)", "log-normal(2,3)", "CHI square(4)", "bad", "bad2(1"))),
    as.data.frame(tibble(.dist = c("norm", "lnorm", "chisq", NA, NA), .args = list(list(0,1), list(2,3), list(4), NA, NA)))
  )
})

test_that("parse_dist works on data frames", {
  # using as.data.frame here because comparison of tibbles with
  # list columns directly doesn't seem to work...
  dists = factor(c("Normal(0,1)", "log-normal(2,3)", "Student's t(3,0,1)"))
  expect_equal(
    parse_dist(data.frame(p = dists), p),
    as.data.frame(tibble(p = dists, .dist = c("norm", "lnorm", "student_t"), .args = list(list(0,1), list(2,3), list(3,0,1))))
  )
})
