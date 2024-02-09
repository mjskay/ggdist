# Tests for parse_dist
#
# Author: mjskay
###############################################################################

suppressWarnings(suppressPackageStartupMessages({
  library(dplyr)
  library(distributional)
}))


test_that("parse_dist works on vectors", {
  dist_env = rlang::current_env()
  ref = as.data.frame(tibble(
    .dist = c("norm", "lnorm", "chisq", NA, NA),
    .args = list(list(0,1), list(2,3), list(4), NA, NA),
    .dist_obj = c(
      dist_wrap("norm", 0, 1, package = "stats"),
      dist_wrap("lnorm", 2, 3, package = "stats"),
      dist_wrap("chisq", 4, package = "stats"),
      NA, NA
    )
  ))

  expect_equal(
    as.data.frame(parse_dist(c("Normal(0,1)", "log-normal(2,3)", "CHI square(4)", "bad", "bad2(1"), package = "stats")),
    ref
  )
})

test_that("parse_dist works on data frames", {
  dist_env = rlang::current_env()
  dists = factor(c("Normal(0,1)", "log-normal(2,3)", "Student's t(3,0,1)"))
  ref = as.data.frame(tibble(
    p = dists,
    lb = c(NA_real_, NA_real_, 0),
    ub = c(NA_real_, NA_real_, NA_real_),
    .dist = c("norm", "lnorm", "student_t"),
    .args = list(list(0,1), list(2,3), list(3,0,1)),
    .dist_obj = c(
      dist_wrap("norm", 0, 1, package = dist_env),
      dist_wrap("lnorm", 2, 3, package = dist_env),
      dist_truncated(dist_wrap("student_t", 3, 0, 1, package = dist_env), 0, Inf)
    )
  ))

  df = data.frame(
    p = dists,
    lb = c(NA_real_, NA_real_, 0),
    ub = c(NA_real_, NA_real_, NA_real_)
  )

  expect_equal(parse_dist(df, p), ref)
  expect_equal(parse_dist(df, p, package = dist_env), ref)
})

test_that("parse_dist works on brmsprior objects", {
  dist_env = rlang::current_env()
  dists = factor(c("Normal(0,1)", "log-normal(2,3)", "Student's t(3,0,1)"))
  ref = as.data.frame(tibble(
    prior = dists,
    .dist = c("norm", "lnorm", "student_t"),
    .args = list(list(0,1), list(2,3), list(3,0,1)),
    .dist_obj = c(
      dist_wrap("norm", 0, 1, package = dist_env),
      dist_wrap("lnorm", 2, 3, package = dist_env),
      dist_wrap("student_t", 3, 0, 1, package = dist_env)
    )
  ))

  brmsprior = data.frame(prior = dists)
  class(brmsprior) = c("brmsprior", "data.frame")
  expect_equal(parse_dist(brmsprior), ref)
})

test_that("parse_dist + marginalize_lkjcorr produces correct results", {
  skip_if_no_vdiffr()


  vdiffr::expect_doppelganger("LKJ marginalization works",
    tibble(prior = "lkjcorr(3)") %>%
      parse_dist(prior) %>%
      marginalize_lkjcorr(K = 2) %>%
      ggplot(aes(y = prior, dist = .dist, args = .args)) +
      stat_dist_halfeye(n = 20) +
      xlim(-1, 1) +
      xlab("Marginal correlation for LKJ(3) prior on 2x2 correlation matrix")
  )
})

test_that("unsupported objects throw error with parse_dist", {
  expect_error(
    parse_dist(list()),
    class = "ggdist_unsupported_type"
  )
})


# check_dist_name ---------------------------------------------------------

test_that("check_dist ignores unknown distributions", {
  expect_warning(
    expect_equal(check_dist_name(c("norm","foo","bar","t")), c("norm",NA,NA,"t")),
    class = "ggdist_unsupported_distribution_name"
  )
})
