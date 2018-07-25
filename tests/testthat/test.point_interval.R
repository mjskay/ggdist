# Tests for point_interval
#
# Author: mjskay
###############################################################################

import::from(plyr, ldply, llply, .)
import::from(dplyr, `%>%`, group_by, summarise, ungroup, rename)
library(tidyr)

context("point_interval")


ff_labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")

get_samples = function() {
  #observations of tau grouped by the factor ff (with levels ff_labels)
  data(RankCorr, package = "tidybayes")
  ldply(1:18, function(i) {
    data.frame(
      .chain = 1,
      .iteration = seq_len(nrow(RankCorr)),
      ff = ff_labels[i],
      tau = RankCorr[, paste0("tau[", i, "]")]
    )
  })
}

test_that("median_qi works on a grouped variable", {
  samples = get_samples()

  ref = samples %>%
    group_by(ff) %>%
    summarise(
      tau.lower = quantile(tau, .025),
      tau.upper = quantile(tau, .975),
      tau = median(tau)
    )

  result.broom = samples %>%
    group_by(ff) %>%
    median_qi(tau)

  result = samples %>%
    group_by(ff) %>%
    median_qi(tau, .broom = FALSE)

  expect_equal(result.broom$tau, ref$tau)
  expect_equal(result.broom$.lower, ref$tau.lower)
  expect_equal(result.broom$.upper, ref$tau.upper)
  expect_equal(result$tau, ref$tau)
  expect_equal(result$tau.lower, ref$tau.lower)
  expect_equal(result$tau.upper, ref$tau.upper)
})

test_that("mean_qi works on multiple columns", {
  samples = get_samples() %>%
    group_by(.iteration) %>%
    spread(ff, tau) %>%
    ungroup()

  ref = samples %>%
    summarise(
      a.lower = as.vector(quantile(a, .025)),
      a.upper = as.vector(quantile(a, .975)),
      a = mean(a),
      b.lower = as.vector(quantile(b, .025)),
      b.upper = as.vector(quantile(b, .975)),
      b = mean(b)
    )

  result = samples %>%
    mean_qi(a, b)

  expect_equal(result$a, ref$a)
  expect_equal(result$a.lower, ref$a.lower)
  expect_equal(result$a.upper, ref$a.upper)
  expect_equal(result$b, ref$b)
  expect_equal(result$b.lower, ref$b.lower)
  expect_equal(result$b.upper, ref$b.upper)
})

test_that("mean_qi works on non-95% probs", {
  samples = get_samples()

  ref = samples %>%
    summarise(
      tau.lower = as.vector(quantile(tau, .25)),
      tau.upper = as.vector(quantile(tau, .75)),
      tau = mean(tau)
    )

  result = samples %>%
    mean_qi(tau, .width = .5)

  expect_equal(result$tau, ref$tau)
  expect_equal(result$.lower, ref$tau.lower)
  expect_equal(result$.upper, ref$tau.upper)
})

test_that("mean_qi works on multiple probs with groups", {
  samples = get_samples()

  ref95 = samples %>%
    group_by(ff) %>%
    summarise(
      .lower = as.vector(quantile(tau, .025)),
      .upper = as.vector(quantile(tau, .975)),
      tau = mean(tau),
      .width = .95
    ) %>%
    select(ff, tau, .lower, .upper, .width)

  ref50 = samples %>%
    group_by(ff) %>%
    summarise(
      .lower = as.vector(quantile(tau, .25)),
      .upper = as.vector(quantile(tau, .75)),
      tau = mean(tau),
      .width = .5
    ) %>%
    select(ff, tau, .lower, .upper, .width)

  ref = bind_rows(ref50, ref95)

  result = samples %>%
    group_by(ff) %>%
    mean_qi(tau, .width = c(.5, .95))

  result_list = samples %>%
    group_by(ff) %>%
    summarise_at("tau", list) %>%
    mean_qi(tau, .width = c(.5, .95))

  expect_equal(as.data.frame(result), as.data.frame(ref))
  expect_equal(as.data.frame(result_list), as.data.frame(ref))
})

test_that("mean_qi works on multiple probs with multiple vars", {
  samples = get_samples() %>%
    mutate(tau2 = tau * 2)

  ref95 = samples %>%
    group_by(ff) %>%
    summarise(
      tau.lower = as.vector(quantile(tau, .025)),
      tau.upper = as.vector(quantile(tau, .975)),
      tau = mean(tau),
      tau2.lower = as.vector(quantile(tau2, .025)),
      tau2.upper = as.vector(quantile(tau2, .975)),
      tau2 = mean(tau2),
      .width = .95
    ) %>%
    select(ff, tau, tau.lower, tau.upper, tau2, tau2.lower, tau2.upper, .width)

  ref50 = samples %>%
    group_by(ff) %>%
    summarise(
      tau.lower = as.vector(quantile(tau, .25)),
      tau.upper = as.vector(quantile(tau, .75)),
      tau = mean(tau),
      tau2.lower = as.vector(quantile(tau2, .25)),
      tau2.upper = as.vector(quantile(tau2, .75)),
      tau2 = mean(tau2),
      .width = .50
    ) %>%
    select(ff, tau, tau.lower, tau.upper, tau2, tau2.lower, tau2.upper, .width)

  ref = bind_rows(ref50, ref95)

  result = samples %>%
    group_by(ff) %>%
    mean_qi(tau, tau2, .width = c(.5, .95))

  result_list = samples %>%
    group_by(ff) %>%
    summarise_at(c("tau", "tau2"), list) %>%
    mean_qi(tau, tau2, .width = c(.5, .95))

  expect_equal(as.data.frame(result), as.data.frame(ref))
  expect_equal(as.data.frame(result_list), as.data.frame(ref))
})

test_that("mean_qi correctly identifies the desired columns when ... is empty", {
  testdf = data_frame(
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
  dd = data_frame(
    x = c(rnorm(1000), rnorm(1000, mean = 5))
  )

  ref = dd %>%
    summarise(
      .lower = list(hdi(x, .width = .5)[, 1]),
      .upper = list(hdi(x, .width = .5)[, 2]),
      x = mean(x),
      .width = .5
    ) %>%
    unnest()

  expect_equal(mean_hdi(dd, x, .width = .5), ref)
})
