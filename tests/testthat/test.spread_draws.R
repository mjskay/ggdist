# Tests for spread_draws
#
# Author: mjskay
###############################################################################

library(dplyr)
library(rlang)
library(tidyr)

context("spread_draws")


#set up datasets
data(RankCorr, package = "tidybayes")

# subset of RankCorr (for speed)
RankCorr_s = RankCorr[[1]][1:10,]

# version of RankCorr with i index labeled
i_labels = c("a", "b", "c")
RankCorr_i = recover_types(RankCorr_s, list(i = factor(i_labels)))

# version of RankCorr with i and j dimensions labeled
i_labels = c("a", "b", "c")
j_labels = c("A", "B", "C", "D")
RankCorr_ij = recover_types(RankCorr_s, list(i = factor(i_labels), j = factor(j_labels)))


# tests for helpers ==========================================================

test_that("all_names works on various expressions", {
  expect_equal(all_names(quote(a + b + c[i, j] + 1)), c("a","b","c","i","j"))

  invalid_expr = quote(a + b)
  invalid_expr[[3]] = list() #replace `b` with a list object
  expect_error(all_names(invalid_expr), "Don't know how to handle type `list`")
})


test_that("parse_variable_spec rejects incorrect usage of `|`", {
  expect_error(parse_variable_spec(quo(a | b | c)),
    "Left-hand side of `|` cannot contain `|`")
  expect_error(parse_variable_spec(quo(a | cbind(b, c))),
    "Right-hand side of `|` must be exactly one name")
})



# tests for spread_draws ===================================================

test_that("spread_draws correctly rejects missing variables", {
  data("RankCorr", package = "tidybayes")

  expect_error(spread_draws(RankCorr, c(a, b)),
    "No variables found matching spec: c\\(a,b\\)")
  expect_error(spread_draws(RankCorr, a[b]),
    "No variables found matching spec: c\\(a\\)\\[b\\]")
})


test_that("spread_draws works on a simple variable with no dimensions", {
  ref = data_frame(
    .chain = as.integer(1),
    .iteration = seq_len(nrow(RankCorr_s)),
    .draw = .iteration,
    typical_r = RankCorr_s[, "typical_r"]
  )

  expect_equal(spread_draws(RankCorr_s, typical_r), ref)
})


test_that("spread_draws works on two variables with no dimensions and multiple chains", {
  data(line, package = "coda")

  ref = data_frame(
    .chain = c(rep(1L, nrow(line[[1]])), rep(2L, nrow(line[[2]]))),
    .iteration = c(seq_len(nrow(line[[1]])), seq_len(nrow(line[[2]]))),
    .draw = seq_len(nrow(line[[1]]) + nrow(line[[2]])),
    alpha = c(line[[1]][,"alpha"], line[[2]][,"alpha"]),
    beta = c(line[[1]][,"beta"], line[[2]][,"beta"])
  )

  expect_equal(spread_draws(line, alpha, beta), ref)
})


test_that("spread_draws works on a variable with one unnamed index", {
  ref = plyr::ldply(1:3, function(i) {
    data.frame(
      .chain = as.integer(1),
      .iteration = seq_len(nrow(RankCorr_s)),
      .draw = seq_len(nrow(RankCorr_s)),
      i = i,
      tau = RankCorr_s[, paste0("tau[", i, "]")]
    )
  })

  expect_equal(spread_draws(RankCorr_s, tau[i]) %>% arrange(i), ref)
})

test_that("spread_draws works on a variable with one named index", {
  ref = plyr::ldply(1:3, function(i) {
    data.frame(
      .chain = as.integer(1),
      .iteration = seq_len(nrow(RankCorr_i)),
      .draw = seq_len(nrow(RankCorr_i)),
      i = i_labels[i],
      tau = RankCorr_i[, paste0("tau[", i, "]")]
    )
  })

  expect_equal(spread_draws(RankCorr_i, tau[i]) %>% arrange(i), ref)
})

test_that("spread_draws works on a variable with one anonymous wide index", {
  ref = data.frame(
    .chain = as.integer(1),
    .iteration = seq_len(nrow(RankCorr_s)),
    .draw = seq_len(nrow(RankCorr_s))
  )
  for (i in 1:3) {
    refcol = data.frame(RankCorr_s[, paste0("tau[", i, "]")])
    names(refcol) = paste0("tau.", i)
    ref = cbind(ref, refcol)
  }

  expect_equal(spread_draws(RankCorr_s, tau[..]), ref)
})


test_that("spread_draws works on a variable with one named wide index", {
  ref = data.frame(
    .chain = as.integer(1),
    .iteration = seq_len(nrow(RankCorr_i)),
    .draw = seq_len(nrow(RankCorr_i))
  )
  for (i in 1:3) {
    refcol = data.frame(RankCorr_i[, paste0("tau[", i, "]")])
    names(refcol) = i_labels[i]
    ref = cbind(ref, refcol)
  }

  expect_equal(spread_draws(RankCorr_i, tau[i] | i), ref)
})


test_that("spread_draws works on a variable with two named dimensions", {
  ref = plyr::ldply(1:4, function(j) {
    plyr::ldply(1:3, function(i) {
      data.frame(
        .chain = as.integer(1),
        .iteration = seq_len(nrow(RankCorr_ij)),
        .draw = seq_len(nrow(RankCorr_ij)),
        i = i_labels[i],
        j = j_labels[j],
        b = RankCorr_ij[, paste0("b[", i, ",", j, "]")]
      )
    })
  })

  expect_equal(spread_draws(RankCorr_ij, b[i, j]) %>% arrange(j, i), ref)
})


test_that("spread_draws works on a variable with two named dimensions, one that is wide", {
  ref = plyr::ldply(1:4, function(j) {
    plyr::ldply(1:3, function(i) {
      data.frame(
        .chain = as.integer(1),
        .iteration = seq_len(nrow(RankCorr_ij)),
        .draw = seq_len(nrow(RankCorr_ij)),
        i = i_labels[i],
        j = j_labels[j],
        b = RankCorr_ij[, paste0("b[", i, ",", j, "]")]
      )
    })
  }) %>%
    spread(j, b)

  expect_equal(spread_draws(RankCorr_ij, b[i, j] | j) %>% arrange(.iteration), ref)
})

test_that("spread_draws works on a variable with one named index and one wide anonymous index", {
  ref = plyr::ldply(1:4, function(j) {
    plyr::ldply(1:3, function(i) {
      data.frame(
        .chain = as.integer(1),
        .iteration = seq_len(nrow(RankCorr_i)),
        .draw = seq_len(nrow(RankCorr_i)),
        i = i_labels[i],
        j = paste0("b.", j),
        b = RankCorr_i[, paste0("b[", i, ",", j, "]")]
      )
    })
  }) %>%
    spread(j, b)

  expect_equal(spread_draws(RankCorr_i, b[i, ..]) %>% arrange(.iteration), ref)
})

test_that("spread_draws does not allow extraction of two variables simultaneously with a wide index", {
  error_message = "Cannot extract draws from multiple variables in wide format."
  expect_error(spread_draws(RankCorr_s, c(tau, u_tau)[..]), error_message)
  expect_error(spread_draws(RankCorr_s, c(tau, u_tau)[i] | i), error_message)
})

test_that("spread_draws correctly extracts multiple variables simultaneously", {
  expect_equal(spread_draws(RankCorr_i, c(tau, u_tau)[i]),
    spread_draws(RankCorr_i, tau[i]) %>%
      inner_join(spread_draws(RankCorr_i, u_tau[i]), by = c(".chain", ".iteration", ".draw", "i"))
  )
  expect_equal(spread_draws(RankCorr_i, cbind(tau)[i]),
    spread_draws(RankCorr_i, c(tau)[i]))
  expect_equal(spread_draws(RankCorr_i, cbind(tau, u_tau)[i]),
    spread_draws(RankCorr_i, c(tau, u_tau)[i]))
})

test_that("spread_draws correctly extracts multiple variables simultaneously when those variables have no dimensions", {
  RankCorr_t = RankCorr_s
  dimnames(RankCorr_t)[[2]][[1]] <- "tr2"

  ref1 = spread_draws(RankCorr_t, typical_r)
  expect_equal(spread_draws(RankCorr_t, c(typical_r)), ref1)

  ref2 = spread_draws(RankCorr_t, tr2) %>%
    inner_join(spread_draws(RankCorr_t, typical_r), by = c(".chain", ".iteration", ".draw"))
  expect_equal(spread_draws(RankCorr_t, c(tr2, typical_r)), ref2)
})

test_that("spread_draws multispec syntax joins results correctly", {
  ref = spread_draws(RankCorr_s, typical_r) %>%
    inner_join(spread_draws(RankCorr_s, tau[i]), by = c(".chain", ".iteration", ".draw")) %>%
    inner_join(spread_draws(RankCorr_s, b[i, v]), by = c(".chain", ".iteration", ".draw", "i"))

  expect_equal(spread_draws(RankCorr_s, typical_r, tau[i], b[i, v]), ref)
})

test_that("spread_draws multispec with different dimensions retains grouping information with all dimensions", {
  groups_ = RankCorr_s %>%
    spread_draws(typical_r, tau[i], b[i, j]) %>%
    groups() %>%
    as.character()

  expect_equal(groups_, c("i", "j"))
})

test_that("groups from spread_draws retain factor level names", {
  draws = RankCorr_i %>% spread_draws(tau[i])

  expect_equivalent(attr(draws, "labels")$i, factor(i_labels))
})

test_that("empty dimensions are dropped", {
  ref = RankCorr_s %>%
    spread_draws(tau[i]) %>%
    ungroup() %>%
    select(-i)

  expect_equal(spread_draws(RankCorr_s, tau[]), ref)

  ref2 = RankCorr_s %>%
    spread_draws(b[i, j]) %>%
    group_by(j) %>%
    select(-i)

  expect_equal(spread_draws(RankCorr_s, b[, j]), ref2)

  ref3 = RankCorr_s %>%
    spread_draws(b[i, j]) %>%
    group_by(i) %>%
    select(-j)

  expect_equal(spread_draws(RankCorr_s, b[i, ]), ref3)

  ref4 = RankCorr_s %>%
    spread_draws(b[i, j]) %>%
    ungroup() %>%
    select(-i, -j)

  expect_equal(spread_draws(RankCorr_s, b[, ]), ref4)
})

test_that("dimensions with existing names as strings are made wide as strings with `..`", {
  RankCorr_t = RankCorr_s
  dimnames(RankCorr_t)[[2]][1] = "x[a]"
  dimnames(RankCorr_t)[[2]][2] = "x[b]"

  ref = RankCorr_t %>%
    spread_draws(x[k]) %>%
    spread(k, x) %>%
    rename(x.a = a, x.b = b)

  expect_equal(spread_draws(RankCorr_t, x[..]), ref)
})

test_that("regular expressions for variable names work on non-indexed variables", {
  ref = spread_draws(RankCorr_s, typical_r)

  expect_equal(spread_draws(RankCorr_s, `typical..`, regex = TRUE), ref)
})

test_that("regular expressions for variable names work on indexed variables", {
  ref = spread_draws(RankCorr_s, c(tau, u_tau)[i])

  expect_equal(spread_draws(RankCorr_s, `.*tau`[i], regex = TRUE), ref)
})

test_that("variable names containing regex special chars work", {
  RankCorr_t = RankCorr_s
  dimnames(RankCorr_t)[[2]][[1]] = "(Intercept)"

  ref = RankCorr_t %>%
    tidy_draws() %>%
    select(.chain, .iteration, .draw, `(Intercept)`)

  expect_equal(spread_draws(RankCorr_t, `(Intercept)`), ref)
})
