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

test_that("parse_dist + marginalize_lkjcorr produces correct results", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")

  vdiffr::expect_doppelganger("LKJ marginalization works",
    data.frame(prior = "lkjcorr(3)") %>%
      parse_dist(prior) %>%
      marginalize_lkjcorr(K = 2) %>%
      ggplot(aes(y = prior, dist = .dist, args = .args)) +
      stat_dist_halfeyeh(n = 20) +
      xlim(-1, 1) +
      xlab("Marginal correlation for LKJ(3) prior on 2x2 correlation matrix")
  )
})

test_that("unsupported objects throw error with parse_dist", {
  expect_error(
    parse_dist(list()),
    'Objects of type "list" are not currently supported by `parse_dist`.'
  )
})


# check_dist_name ---------------------------------------------------------

test_that("check_dist ignores unknown distributions", {
  expect_warning(
    expect_equal(check_dist_name(c("norm","foo","bar","t")), c("norm",NA,NA,"t")),
    "The following distribution names were not recognized and were ignored:.*foo, bar"
  )
})
