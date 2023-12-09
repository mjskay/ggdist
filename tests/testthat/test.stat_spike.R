# Tests for spike plots
#
# Author: mjskay
###############################################################################

suppressPackageStartupMessages({
  library(distributional)
})


test_that("spike works", {
  skip_if_no_vdiffr()

  p = ggplot(data.frame(x = dist_normal()), aes(xdist = x)) +
    stat_slabinterval() +
    stat_spike() +
    stat_spike(at = c(-1.5, 1.5), color = "red") +
    stat_spike(at = qi, color = "green") +
    scale_thickness_shared()

  vdiffr::expect_doppelganger("spike works",
    p
  )
})

test_that("constant distributions work", {
  p = ggplot(data.frame(x = dist_normal(1, 0)), aes(xdist = x))

  test_data = function(plot) as.list(layer_data(plot)[,c("thickness","pdf","cdf","x","ymin","ymax")])
  expect_snapshot_value(test_data(p + stat_spike()), style = "deparse", cran = TRUE)
  expect_snapshot_value(test_data(p + stat_spike(at = 0)), style = "deparse", cran = TRUE)
})

test_that("at param works", {
  expect_equal(check_at(NULL), list())
  expect_equal(check_at(character()), list())
  expect_equal(check_at(numeric()), list())
  expect_equal(check_at(list()), list())

  expect_equal(check_at(1), list("1" = 1))
  expect_equal(check_at(1:3), list("1" = 1, "2" = 2, "3" = 3))
  expect_equal(check_at(list(1:3)), list("1" = 1, "2" = 2, "3" = 3))
  expect_equal(check_at(list(a = 1:3)), list("a" = 1, "a" = 2, "a" = 3))
  expect_equal(check_at(c("a" = 1, "b" = 2, "c" = 3)), list("a" = 1, "b" = 2, "c" = 3))
  expect_equal(check_at(list(c("a" = 1, "b" = 2, "c" = 3))), list("a" = 1, "b" = 2, "c" = 3))
  expect_equal(check_at(list(x = c("a" = 1, "b" = 2, "c" = 3))), list("x.a" = 1, "x.b" = 2, "x.c" = 3))

  expect_equal(check_at(list(mean, median)), list("<fn1>" = mean, "<fn2>" = median))
  expect_equal(check_at(list(a = mean, b = median)), list("a" = mean, "b" = median))
  expect_equal(check_at(list(a = mean, median)), list("a" = mean, "<fn1>" = median))

  expect_equal(check_at(list("mean", "median")), list(mean = mean, median = median))
  expect_equal(check_at(c("mean", "median")), list(mean = mean, median = median))
  expect_equal(check_at(c(a = "mean", b = "median")), list(a = mean, b = median))
  expect_equal(check_at(list(c(a = "mean", b = "median"))), list(a = mean, b = median))
  expect_equal(check_at(list(c("mean", "median"))), list(mean = mean, median = median))
  expect_equal(check_at(list(a = c("mean", "median"))), list(a = mean, a = median))
  expect_equal(check_at(list(a = c(x = "mean", "median"))), list(a.x = mean, a2 = median))
  expect_equal(check_at(list(a = c(x = "mean", y = "median"))), list(a.x = mean, a.y = median))

  expect_error(check_at(TRUE), class = "ggdist_param_at_invalid")
  expect_error(check_at(list(TRUE)), class = "ggdist_param_at_invalid")
  expect_error(check_at(list("a", TRUE, FALSE)), class = "ggdist_param_at_invalid")
  expect_error(check_at(list(list())), class = "ggdist_param_at_invalid")
})
