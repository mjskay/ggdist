# Tests for point_interval
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)

context("point_interval")


ff_labels = c("a", "b", "c")

get_draws = function() {
  #observations of tau grouped by the factor ff (with levels ff_labels)
  data(RankCorr, package = "tidybayes")
  rank_corr = RankCorr[[1]]
  plyr::ldply(1:3, function(i) {
    data.frame(
      .chain = as.integer(1),
      .iteration = seq_len(nrow(rank_corr)),
      .draw = seq_len(nrow(rank_corr)),
      ff = ff_labels[i],
      tau = as.vector(rank_corr[, paste0("tau[", i, "]")])
    )
  })
}

test_that("median_qi works on a grouped variable", {
  draws = get_draws()

  ref = draws %>%
    group_by(ff) %>%
    summarise(
      tau.lower = quantile(tau, .025),
      tau.upper = quantile(tau, .975),
      tau = median(tau)
    )

  result.simple = draws %>%
    group_by(ff) %>%
    median_qi(tau)

  result = draws %>%
    group_by(ff) %>%
    median_qi(tau, .simple_names = FALSE)

  expect_equal(result.simple$tau, ref$tau)
  expect_equal(result.simple$.lower, ref$tau.lower)
  expect_equal(result.simple$.upper, ref$tau.upper)
  expect_equal(result$tau, ref$tau)
  expect_equal(result$tau.lower, ref$tau.lower)
  expect_equal(result$tau.upper, ref$tau.upper)
})

test_that("mean_qi works on multiple columns", {
  draws = get_draws() %>%
    group_by(.iteration) %>%
    spread(ff, tau) %>%
    ungroup()

  ref = draws %>%
    summarise(
      a.lower = as.vector(quantile(a, .025)),
      a.upper = as.vector(quantile(a, .975)),
      a = mean(a),
      b.lower = as.vector(quantile(b, .025)),
      b.upper = as.vector(quantile(b, .975)),
      b = mean(b)
    )

  result = draws %>%
    mean_qi(a, b)

  expect_equal(result$a, ref$a)
  expect_equal(result$a.lower, ref$a.lower)
  expect_equal(result$a.upper, ref$a.upper)
  expect_equal(result$b, ref$b)
  expect_equal(result$b.lower, ref$b.lower)
  expect_equal(result$b.upper, ref$b.upper)
})

test_that("mean_qi works on non-95% probs", {
  draws = get_draws()

  ref = draws %>%
    summarise(
      tau.lower = as.vector(quantile(tau, .25)),
      tau.upper = as.vector(quantile(tau, .75)),
      tau = mean(tau)
    )

  result = draws %>%
    mean_qi(tau, .width = .5)

  expect_equal(result$tau, ref$tau)
  expect_equal(result$.lower, ref$tau.lower)
  expect_equal(result$.upper, ref$tau.upper)
})

test_that("mean_qi works on multiple probs with groups", {
  draws = get_draws()

  ref95 = draws %>%
    group_by(ff) %>%
    summarise(
      .lower = as.vector(quantile(tau, .025)),
      .upper = as.vector(quantile(tau, .975)),
      tau = mean(tau),
      .width = .95,
      .point = "mean",
      .interval = "qi"
    ) %>%
    select(ff, tau, .lower, .upper, .width, .point, .interval)

  ref50 = draws %>%
    group_by(ff) %>%
    summarise(
      .lower = as.vector(quantile(tau, .25)),
      .upper = as.vector(quantile(tau, .75)),
      tau = mean(tau),
      .width = .5,
      .point = "mean",
      .interval = "qi"
    ) %>%
    select(ff, tau, .lower, .upper, .width, .point, .interval)

  ref = bind_rows(ref50, ref95)

  result = draws %>%
    group_by(ff) %>%
    mean_qi(tau, .width = c(.5, .95))

  result_list = draws %>%
    group_by(ff) %>%
    summarise_at("tau", list) %>%
    mean_qi(tau, .width = c(.5, .95))

  expect_equal(as.data.frame(result), as.data.frame(ref))
  expect_equal(as.data.frame(result_list), as.data.frame(ref))
})

test_that("mean_qi works on multiple probs with multiple vars", {
  draws = get_draws() %>%
    mutate(tau2 = tau * 2)

  ref95 = draws %>%
    group_by(ff) %>%
    summarise(
      tau.lower = as.vector(quantile(tau, .025)),
      tau.upper = as.vector(quantile(tau, .975)),
      tau = mean(tau),
      tau2.lower = as.vector(quantile(tau2, .025)),
      tau2.upper = as.vector(quantile(tau2, .975)),
      tau2 = mean(tau2),
      .width = .95,
      .point = "mean",
      .interval = "qi"
    ) %>%
    select(ff, tau, tau.lower, tau.upper, tau2, tau2.lower, tau2.upper, .width, .point, .interval)

  ref50 = draws %>%
    group_by(ff) %>%
    summarise(
      tau.lower = as.vector(quantile(tau, .25)),
      tau.upper = as.vector(quantile(tau, .75)),
      tau = mean(tau),
      tau2.lower = as.vector(quantile(tau2, .25)),
      tau2.upper = as.vector(quantile(tau2, .75)),
      tau2 = mean(tau2),
      .width = .50,
      .point = "mean",
      .interval = "qi"
    ) %>%
    select(ff, tau, tau.lower, tau.upper, tau2, tau2.lower, tau2.upper, .width, .point, .interval)

  ref = bind_rows(ref50, ref95)

  result = draws %>%
    group_by(ff) %>%
    mean_qi(tau, tau2, .width = c(.5, .95))

  result_list = draws %>%
    group_by(ff) %>%
    summarise_at(c("tau", "tau2"), list) %>%
    mean_qi(tau, tau2, .width = c(.5, .95))

  expect_equal(as.data.frame(result), as.data.frame(ref))
  expect_equal(as.data.frame(result_list), as.data.frame(ref))
})

test_that("mean_qi correctly identifies the desired columns when ... is empty", {
  testdf = tibble(
    .chain = 1:1000,
    .iteration = 1:1000,
    .draw = 1:1000,
    .row = 1:1000,
    .x = c(qnorm(ppoints(500)), qnorm(ppoints(500), 1)),
    y = c(qnorm(ppoints(500), 2), qnorm(ppoints(500), 3)),
    g = c(rep("a", 500), rep("b", 500))
  ) %>%
    group_by(g)

  expect_equal(mean_qi(testdf, .x, y), mean_qi(testdf))
})

test_that("multiple-response intervals work", {
  set.seed(1234)
  dd = tibble(
    x = c(rnorm(1000), rnorm(1000, mean = 5))
  )

  ref = dd %>%
    summarise(
      .lower = list(hdi(x, .width = .5)[, 1]),
      .upper = list(hdi(x, .width = .5)[, 2]),
      x = Mode(x),
      .width = .5,
      .point = "mode",
      .interval = "hdi"
    ) %>%
    unnest()

  expect_equal(mode_hdi(dd, x, .width = .5), ref)
})

test_that("point_interval errors if there are no columns to summarise", {
  expect_error(median_hdi(data.frame()),
    "No columns found to calculate point and interval summaries for\\.")
})

test_that("point_interval works on vectors", {
  set.seed(1234)
  x = rnorm(100, mean = 5)

  ref = data.frame(
    y = mean(x),
    ymin = as.vector(quantile(x, probs = .025)),
    ymax = as.vector(quantile(x, probs = .975)),
    .width = .95,
    .point = "mean",
    .interval = "qi",
    stringsAsFactors = FALSE
  )

  expect_equal(mean_qi(x), ref)
  expect_equal(mean_qih(x), rename(ref, x = y, xmin = ymin, xmax = ymax))
})

test_that("various point summaries and intervals give correct numbers", {
  expect_equal(
    median_hdci(c(0:6, 1:5, 2:4, 2), .width = .6),
    data.frame(y = 3, ymin = 2, ymax = 4, .width = 0.6, .point = "median", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_qi(c(0:6, 1:5, 2:4, 2), .width = .6),
    data.frame(y = 2.9375, ymin = 2, ymax = 4, .width = 0.6, .point = "mean", .interval = "qi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdi(c(0:6, 1:5, 2:4, 2), .width = .6, .simple_names = TRUE),
    data.frame(.value = 2, .lower = 2, .upper = 4, .width = 0.6, .point = "mode", .interval = "hdi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdci(c(0:6, 1:5, 2:4, 2), .width = .6, .simple_names = TRUE),
    data.frame(.value = 2, .lower = 2, .upper = 4, .width = 0.6, .point = "mode", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_hdci(c(0:6, 1:5, 2:4, 2), .width = .6),
    data.frame(y = 2.9375, ymin = 2, ymax = 4, .width = 0.6, .point = "mean", .interval = "hdci", stringsAsFactors = FALSE)
  )

})

test_that("attempting to use hdi with multiple multimodal columns simultaneously fails", {
  expect_error(
    mode_hdi(data.frame(x = c(1:5, 1, 5), y = c(1:5, 1, 5)), .width = .2),
    "You are summarizing a multimodal distribution using a method that returns multiple intervals"
  )
})

test_that("NAs are handled correctly in point_interval", {
  expect_equal(
    median_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = TRUE),
    data.frame(y = 3, ymin = 2, ymax = 4, .width = 0.6, .point = "median", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_qi(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = TRUE),
    data.frame(y = 2.9375, ymin = 2, ymax = 4, .width = 0.6, .point = "mean", .interval = "qi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdi(c(0:6, 1:5, 2:4, 2, NA), .width = .6, .simple_names = TRUE, na.rm = TRUE),
    data.frame(.value = 2, .lower = 2, .upper = 4, .width = 0.6, .point = "mode", .interval = "hdi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, .simple_names = TRUE, na.rm = TRUE),
    data.frame(.value = 2, .lower = 2, .upper = 4, .width = 0.6, .point = "mode", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = TRUE),
    data.frame(y = 2.9375, ymin = 2, ymax = 4, .width = 0.6, .point = "mean", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    median_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = FALSE),
    data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_, .width = 0.6, .point = "median", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_qi(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = FALSE),
    data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_, .width = 0.6, .point = "mean", .interval = "qi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdi(c(0:6, 1:5, 2:4, 2, NA), .width = .6, .simple_names = TRUE, na.rm = FALSE),
    data.frame(.value = NA_real_, .lower = NA_real_, .upper = NA_real_, .width = 0.6, .point = "mode", .interval = "hdi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, .simple_names = TRUE, na.rm = FALSE),
    data.frame(.value = NA_real_, .lower = NA_real_, .upper = NA_real_, .width = 0.6, .point = "mode", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = FALSE),
    data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_, .width = 0.6, .point = "mean", .interval = "hdci", stringsAsFactors = FALSE)
  )
})
