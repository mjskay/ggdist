# Tests for compare_levels
#
# Author: mjskay
###############################################################################

import::from(plyr, ldply, llply, .)
import::from(dplyr, `%>%`)
import::from(tibble, as_tibble)
library(tidyr)

context("compare_levels")


ff_labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")

get_draws = function() {
  #observations of tau grouped by the factor ff (with levels ff_labels)
  data(RankCorr, package = "tidybayes")
  ldply(1:18, function(i) {
    data.frame(
      .chain = as.integer(1),
      .iteration = seq_len(nrow(RankCorr)),
      .draw = seq_len(nrow(RankCorr)),
      ff = ff_labels[i],
      tau = RankCorr[, paste0("tau[", i, "]")]
    )
  })
}

test_that("pairwise level comparison works", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = ldply(combn(levels(draws$ff), 2, simplify = FALSE), function(levels.) {
    draws_wide$ff = factor(paste(levels.[[2]], "-", levels.[[1]]))
    draws_wide$tau = draws_wide[[levels.[[2]]]] - draws_wide[[levels.[[1]]]]
    draws_wide
  }) %>%
    select(-one_of(ff_labels)) %>%
    as_tibble()

  expect_equal(compare_levels(draws, tau, by = ff, comparison = pairwise), ref)
  expect_equal(group_vars(compare_levels(draws, tau, by = ff, comparison = pairwise)), "ff")
  expect_equal(compare_levels(draws, tau, by = ff, comparison = "pairwise"), ref)
})

test_that("ordered level comparison works", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = ldply(llply(2:18, function(i) c(ff_labels[[i]], ff_labels[[i - 1]])), function(levels.) {
    draws_wide$ff = factor(paste(levels.[[1]], "-", levels.[[2]]))
    draws_wide$tau = draws_wide[[levels.[[1]]]] - draws_wide[[levels.[[2]]]]
    draws_wide
  }) %>%
    select(-one_of(ff_labels)) %>%
    as_tibble()

  expect_equal(compare_levels(draws, tau, by = ff, comparison = ordered), ref)
  expect_equal(compare_levels(draws, tau, by = ff, comparison = "ordered"), ref)
})

test_that("control level comparison works", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = ldply(llply(2:18, function(i) c(ff_labels[[i]], ff_labels[[1]])), function(levels.) {
    draws_wide$ff = factor(paste(levels.[[1]], "-", levels.[[2]]))
    draws_wide$tau = draws_wide[[levels.[[1]]]] - draws_wide[[levels.[[2]]]]
    draws_wide
  }) %>%
    select(-one_of(ff_labels)) %>%
    as_tibble()

  expect_equal(compare_levels(draws, tau, by = ff, comparison = control), ref)
})

test_that("default level comparison selects the correct comparison depending on if `by` is ordered", {
  draws = get_draws()

  expect_equal(compare_levels(draws, tau, by = ff, comparison = default),
    compare_levels(draws, tau, by = ff, comparison = pairwise))

  draws$ff = ordered(draws$ff)

  expect_equal(compare_levels(draws, tau, by = ff, comparison = default),
    compare_levels(draws, tau, by = ff, comparison = ordered))
})

test_that("named functions are supported and named with their own name", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = ldply(llply(2:18, function(i) c(ff_labels[[i]], ff_labels[[1]])), function(levels.) {
    draws_wide$ff = factor(paste(levels.[[1]], "+", levels.[[2]]))
    draws_wide$tau = draws_wide[[levels.[[1]]]] + draws_wide[[levels.[[2]]]]
    draws_wide
  }) %>%
    select(-one_of(ff_labels)) %>%
    as_tibble()

  expect_equal(compare_levels(draws, tau, by = ff, fun = `+`, comparison = control), ref)
})

test_that("anonymous functions are supported and named with `:`", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = ldply(llply(2:18, function(i) c(ff_labels[[i]], ff_labels[[1]])), function(levels.) {
    draws_wide$ff = factor(paste(levels.[[1]], ":", levels.[[2]]))
    draws_wide$tau = draws_wide[[levels.[[1]]]] + draws_wide[[levels.[[2]]]]
    draws_wide
  }) %>%
    select(-one_of(ff_labels)) %>%
    as_tibble()

  expect_equal(compare_levels(draws, tau, by = ff, fun = function(x, y) x + y, comparison = control), ref)
})

test_that("custom comparisons of lists of character vectors are supported", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = ldply(list(c("a", "b"), c("c", "f")), function(levels.) {
    draws_wide$ff = factor(paste(levels.[[1]], "-", levels.[[2]]))
    draws_wide$tau = draws_wide[[levels.[[1]]]] - draws_wide[[levels.[[2]]]]
    draws_wide
  }) %>%
    select(-one_of(ff_labels)) %>%
    as_tibble()

  expect_equal(compare_levels(draws, tau, by = ff, comparison = list(c("a", "b"), c("c", "f"))), ref)
})

test_that("custom comparisons of lists of unevaluated expressions are supported", {
  draws = get_draws()

  draws_wide = spread(draws, ff, tau)
  ref = ldply(.(a + b, exp(c - f)), function(levels.) {
    draws_wide$ff = factor(deparse0(levels.))
    draws_wide$tau = eval(levels., draws_wide)
    draws_wide
  }) %>%
    select(-one_of(ff_labels)) %>%
    as_tibble()

  expect_equal(compare_levels(draws, tau, by = ff, comparison = plyr::.(a + b, exp(c - f))), ref)
})

test_that("comparisons of subsets of levels of factors are supported", {
  draws = get_draws() %>%
    filter(ff %in% c("a",
      "d", "g"))

  draws_wide = spread(draws, ff, tau)
  ref = ldply(combn(levels(factor(draws$ff)), 2, simplify = FALSE), function(levels.) {
    draws_wide$ff = factor(paste(levels.[[2]], "-", levels.[[1]]))
    draws_wide$tau = draws_wide[[levels.[[2]]]] - draws_wide[[levels.[[1]]]]
    draws_wide
  }) %>%
    select(-one_of(c("a", "d", "g"))) %>%
    as_tibble()

  expect_equal(compare_levels(draws, tau, by = ff, comparison = pairwise), ref)
})

test_that("extraneous columns are dropped before comparison", {
  draws = get_draws()

  draws_extra = draws %>%
    mutate(sd = 1 / sqrt(tau))  #use something that won't act as a clean index

  expect_equal(
    compare_levels(draws, tau, by = ff, comparison = pairwise),
    compare_levels(draws_extra, tau, by = ff, comparison = pairwise)
  )
})
