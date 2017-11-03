# Tests for spread_samples
#
# Author: mjskay
###############################################################################

library(testthat)
import::from(plyr, ldply, .)  #TODO: drop remaining ldplys from this file
import::from(dplyr, `%>%`, inner_join, data_frame)
library(tidyr)
library(tidybayes)

context("spread_samples")

test_that("spread_samples works on a simple parameter with no indices", {
  data(RankCorr, package = "tidybayes")

  ref = data_frame(
    .chain = as.integer(1),
    .iteration = seq_len(nrow(RankCorr)),
    typical_r = RankCorr[, "typical_r"]
  )
  expect_equal(spread_samples(RankCorr, typical_r), ref)
})


test_that("spread_samples works on a parameter with one unnamed index", {
  data(RankCorr, package = "tidybayes")

  ref = ldply(1:18, function(i) {
    data.frame(
      .chain = as.integer(1),
      .iteration = seq_len(nrow(RankCorr)),
      i = i,
      tau = RankCorr[, paste0("tau[", i, "]")]
    )
  })

  expect_equal(spread_samples(RankCorr, tau[i]) %>% arrange(i), ref)
})

test_that("spread_samples works on a parameter with one named index", {
  i_labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")
  data(RankCorr, package = "tidybayes")
  RankCorr = recover_types(RankCorr, list(i = factor(i_labels)))

  ref = ldply(1:18, function(i) {
    data.frame(
      .chain = as.integer(1),
      .iteration = seq_len(nrow(RankCorr)),
      i = i_labels[i],
      tau = RankCorr[, paste0("tau[", i, "]")]
    )
  })

  expect_equal(spread_samples(RankCorr, tau[i]) %>% arrange(i), ref)
})

test_that("spread_samples works on a parameter with one anonymous wide index", {
  data(RankCorr, package = "tidybayes")

  ref = data.frame(
    .chain = as.integer(1),
    .iteration = seq_len(nrow(RankCorr))
  )
  for (i in 1:18) {
    refcol = data.frame(RankCorr[, paste0("tau[", i, "]")])
    names(refcol) = paste0("tau.", i)
    ref = cbind(ref, refcol)
  }

  expect_equal(spread_samples(RankCorr, tau[..]), ref)
})


test_that("spread_samples works on a parameter with one named wide index", {
  i_labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")
  data(RankCorr, package = "tidybayes")
  RankCorr = recover_types(RankCorr, list(i = factor(i_labels)))

  ref = data.frame(
    .chain = as.integer(1),
    .iteration = seq_len(nrow(RankCorr))
  )
  for (i in 1:18) {
    refcol = data.frame(RankCorr[, paste0("tau[", i, "]")])
    names(refcol) = i_labels[i]
    ref = cbind(ref, refcol)
  }

  expect_equal(spread_samples(RankCorr, tau[i] | i), ref)
})


test_that("spread_samples works on a parameter with two named indices", {
  i_labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")
  j_labels = c("A", "B", "C", "D")
  data(RankCorr, package = "tidybayes")
  RankCorr = recover_types(RankCorr,
    list(i = factor(i_labels), j = factor(j_labels)))


  ref = ldply(1:4, function(j) {
    ldply(1:18, function(i) {
      data.frame(
        .chain = as.integer(1),
        .iteration = seq_len(nrow(RankCorr)),
        i = i_labels[i],
        j = j_labels[j],
        b = RankCorr[, paste0("b[", i, ",", j, "]")]
      )
    })
  })

  expect_equal(spread_samples(RankCorr, b[i, j]) %>% arrange(j, i), ref)
})


test_that("spread_samples works on a parameter with two named indices, one that is wide", {
  i_labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")
  j_labels = c("A", "B", "C", "D")
  data(RankCorr, package = "tidybayes")
  RankCorr = recover_types(RankCorr,
    list(i = factor(i_labels), j = factor(j_labels)))


  ref = ldply(1:4, function(j) {
    ldply(1:18, function(i) {
      data.frame(
        .chain = as.integer(1),
        .iteration = seq_len(nrow(RankCorr)),
        i = i_labels[i],
        j = j_labels[j],
        b = RankCorr[, paste0("b[", i, ",", j, "]")]
      )
    })
  }) %>%
    spread(j, b)

  expect_equal(spread_samples(RankCorr, b[i, j] | j) %>% arrange(.iteration), ref)
})

test_that("spread_samples works on a parameter with one named index and one wide anonymous index", {
  i_labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")
  data(RankCorr, package = "tidybayes")
  RankCorr = recover_types(RankCorr,
    list(i = factor(i_labels)))


  ref = ldply(1:4, function(j) {
    ldply(1:18, function(i) {
      data.frame(
        .chain = as.integer(1),
        .iteration = seq_len(nrow(RankCorr)),
        i = i_labels[i],
        j = paste0("b.", j),
        b = RankCorr[, paste0("b[", i, ",", j, "]")]
      )
    })
  }) %>%
    spread(j, b)

  expect_equal(spread_samples(RankCorr, b[i, ..]) %>% arrange(.iteration), ref)
})

test_that("spread_samples does not allow extraction of two variables simultaneously with a wide index", {
  data(RankCorr, package = "tidybayes")

  error_message = "Cannot extract samples of multiple variables in wide format."
  expect_error(spread_samples(RankCorr, c(tau, typical_mu)[..]), error_message)
  expect_error(spread_samples(RankCorr, c(tau, typical_mu)[i] | i), error_message)
})

test_that("spread_samples correctly extracts multiple variables simultaneously", {
  i_labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")
  data(RankCorr, package = "tidybayes")
  RankCorr = recover_types(RankCorr,
    list(i = factor(i_labels)))

  expect_equal(spread_samples(RankCorr, c(tau, typical_mu)[i]),
    spread_samples(RankCorr, tau[i]) %>%
      inner_join(spread_samples(RankCorr, typical_mu[i]), by = c(".chain", ".iteration", "i"))
  )
  expect_equal(spread_samples(RankCorr, c(tau, typical_mu, u_tau)[i]),
    spread_samples(RankCorr, tau[i]) %>%
      inner_join(spread_samples(RankCorr, typical_mu[i]), by = c(".chain", ".iteration", "i")) %>%
      inner_join(spread_samples(RankCorr, u_tau[i]), by = c(".chain", ".iteration", "i"))
  )
  expect_equal(spread_samples(RankCorr, cbind(tau)[i]),
    spread_samples(RankCorr, c(tau)[i]))
  expect_equal(spread_samples(RankCorr, cbind(tau, typical_mu)[i]),
    spread_samples(RankCorr, c(tau, typical_mu)[i]))
  expect_equal(spread_samples(RankCorr, cbind(tau, typical_mu, u_tau)[i]),
    spread_samples(RankCorr, c(tau, typical_mu, u_tau)[i]))
})

test_that("spread_samples correctly extracts multiple variables simultaneously when those variables have no indices", {
  data(RankCorr, package = "tidybayes")
  dimnames(RankCorr)[[2]][[1]] <- "tr2"

  ref1 = spread_samples(RankCorr, typical_r)
  expect_equal(spread_samples(RankCorr, c(typical_r)), ref1)

  ref2 = spread_samples(RankCorr, tr2) %>%
    inner_join(spread_samples(RankCorr, typical_r), by = c(".chain", ".iteration"))
  expect_equal(spread_samples(RankCorr, c(tr2, typical_r)), ref2)
})

test_that("spread_samples multispec syntax joins results correctly", {
  data(RankCorr, package = "tidybayes")

  ref = spread_samples(RankCorr, typical_r) %>%
    inner_join(spread_samples(RankCorr, tau[i]), by = c(".chain", ".iteration")) %>%
    inner_join(spread_samples(RankCorr, b[i, v]), by = c(".chain", ".iteration", "i"))

  expect_equal(spread_samples(RankCorr, typical_r, tau[i], b[i, v]), ref)
})

test_that("spread_samples multispec with different indices retains grouping information with all indices", {
  data(RankCorr, package = "tidybayes")

  groups_ = RankCorr %>%
    spread_samples(typical_r, tau[i], b[i, j]) %>%
    groups() %>%
    as.character()

  expect_equal(groups_, c("i", "j"))
})

test_that("groups from spread_samples retain factor level names", {
  i_labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r")
  data(RankCorr, package = "tidybayes")
  RankCorr = recover_types(RankCorr, list(i = factor(i_labels)))

  samples = RankCorr %>% spread_samples(tau[i])

  expect_equivalent(attr(samples, "labels")$i, factor(i_labels))
})

test_that("empty indices are dropped", {
  data(RankCorr, package = "tidybayes")

  ref = RankCorr %>%
    spread_samples(tau[i]) %>%
    ungroup() %>%
    select(-i)

  expect_equal(spread_samples(RankCorr, tau[]), ref)

  ref2 = RankCorr %>%
    spread_samples(b[i, j]) %>%
    group_by(j) %>%
    select(-i)

  expect_equal(spread_samples(RankCorr, b[, j]), ref2)

  ref3 = RankCorr %>%
    spread_samples(b[i, j]) %>%
    group_by(i) %>%
    select(-j)

  expect_equal(spread_samples(RankCorr, b[i, ]), ref3)

  ref4 = RankCorr %>%
    spread_samples(b[i, j]) %>%
    ungroup() %>%
    select(-i, -j)

  expect_equal(spread_samples(RankCorr, b[, ]), ref4)
})

test_that("indices with existing names as strings are made wide as strings with `..`", {
  data(RankCorr, package = "tidybayes")
  dimnames(RankCorr)[[2]][1] = "x[a]"
  dimnames(RankCorr)[[2]][2] = "x[b]"

  ref = RankCorr %>%
    spread_samples(x[k]) %>%
    spread(k, x) %>%
    rename(x.a = a, x.b = b)

  expect_equal(spread_samples(RankCorr, x[..]), ref)
})

test_that("regular expressions for parameter names work on non-indexed parameters", {
  data(RankCorr, package = "tidybayes")

  ref = spread_samples(RankCorr, typical_r)

  expect_equal(spread_samples(RankCorr, `typical..`, regex = TRUE), ref)
})

test_that("regular expressions for parameter names work on indexed parameters", {
  data(RankCorr, package = "tidybayes")

  ref = spread_samples(RankCorr, c(tau, u_tau)[i])

  expect_equal(spread_samples(RankCorr, `.*tau`[i], regex = TRUE), ref)
})

test_that("parameter names containing regex special chars work", {
  data(RankCorr, package = "tidybayes")

  dimnames(RankCorr)[[2]][[1]] = "(Intercept("

  ref = RankCorr %>%
    as_sample_tibble() %>%
    select(.chain, .iteration, `(Intercept(`)

  expect_equal(spread_samples(RankCorr, `(Intercept(`), ref)
})
