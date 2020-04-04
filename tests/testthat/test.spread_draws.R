# Tests for spread_draws
#
# Author: mjskay
###############################################################################

library(dplyr)
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
    "No variables found matching spec: a\\[b\\]")
  expect_error(spread_draws(RankCorr, c(a, x)[b]),
    "No variables found matching spec: c\\(a,x\\)\\[b\\]")
})


test_that("spread_draws works on a simple variable with no dimensions", {
  ref = tibble(
    .chain = as.integer(1),
    .iteration = seq_len(nrow(RankCorr_s)),
    .draw = .iteration,
    typical_r = as.vector(RankCorr_s[, "typical_r"])
  )

  expect_equivalent(spread_draws(RankCorr_s, typical_r), ref)
})


test_that("spread_draws works on two variables with no dimensions and multiple chains", {
  data(line, package = "coda")

  ref = tibble(
    .chain = c(rep(1L, nrow(line[[1]])), rep(2L, nrow(line[[2]]))),
    .iteration = c(seq_len(nrow(line[[1]])), seq_len(nrow(line[[2]]))),
    .draw = seq_len(nrow(line[[1]]) + nrow(line[[2]])),
    alpha = as.vector(c(line[[1]][,"alpha"], line[[2]][,"alpha"])),
    beta = as.vector(c(line[[1]][,"beta"], line[[2]][,"beta"]))
  )

  expect_equal(spread_draws(line, alpha, beta), ref)
})


test_that("spread_draws works on a variable with one unnamed index", {
  ref = plyr::ldply(1:3, function(i) {
    data.frame(
      i = i,
      tau = RankCorr_s[, paste0("tau[", i, "]")],
      .chain = as.integer(1),
      .iteration = seq_len(nrow(RankCorr_s)),
      .draw = seq_len(nrow(RankCorr_s))
    )
  }) %>%
    group_by(i)

  expect_equal(spread_draws(RankCorr_s, tau[i]) %>% arrange(i), ref)
})

test_that("spread_draws works on a variable with one named index", {
  ref = plyr::ldply(1:3, function(i) {
    data.frame(
      i = factor(i_labels[i]),
      tau = RankCorr_i[, paste0("tau[", i, "]")],
      .chain = as.integer(1),
      .iteration = seq_len(nrow(RankCorr_i)),
      .draw = seq_len(nrow(RankCorr_i))
    )
  }) %>%
    group_by(i)

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
  ref = as_tibble(ref)

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
  ref = as_tibble(ref)

  expect_equal(spread_draws(RankCorr_i, tau[i] | i), ref)
})


test_that("spread_draws works on a variable with two named dimensions", {
  ref = plyr::ldply(1:4, function(j) {
    plyr::ldply(1:3, function(i) {
      data.frame(
        i = factor(i_labels[i]),
        j = factor(j_labels[j]),
        b = RankCorr_ij[, paste0("b[", i, ",", j, "]")],
        .chain = as.integer(1),
        .iteration = seq_len(nrow(RankCorr_ij)),
        .draw = seq_len(nrow(RankCorr_ij))
      )
    })
  }) %>%
    group_by(i, j)

  expect_equal(spread_draws(RankCorr_ij, b[i, j]) %>% arrange(j, i), ref)
})


test_that("spread_draws works on a variable with two named dimensions, one that is wide", {
  ref = plyr::ldply(1:4, function(j) {
    plyr::ldply(1:3, function(i) {
      data.frame(
        i = factor(i_labels[i]),
        .chain = as.integer(1),
        .iteration = seq_len(nrow(RankCorr_ij)),
        .draw = seq_len(nrow(RankCorr_ij)),
        j = factor(j_labels[j]),
        b = RankCorr_ij[, paste0("b[", i, ",", j, "]")]
      )
    })
  }) %>%
    spread(j, b)

  # grouping attributes are too finicky on this one for an exact comparison
  expect_equivalent(spread_draws(RankCorr_ij, b[i, j] | j) %>% arrange(i, .iteration), ref)
})

test_that("spread_draws works on a variable with one named index and one wide anonymous index", {
  ref = plyr::ldply(1:4, function(j) {
    plyr::ldply(1:3, function(i) {
      data.frame(
        i = factor(i_labels[i]),
        .chain = as.integer(1),
        .iteration = seq_len(nrow(RankCorr_i)),
        .draw = seq_len(nrow(RankCorr_i)),
        j = factor(paste0("b.", j)),
        b = RankCorr_i[, paste0("b[", i, ",", j, "]")]
      )
    })
  }) %>%
    spread(j, b)

  # grouping attributes are too finicky on this one for an exact comparison
  expect_equivalent(spread_draws(RankCorr_i, b[i, ..]) %>% arrange(i, .iteration), ref)
})

test_that("spread_draws does not allow extraction of two variables simultaneously with a wide index", {
  error_message = "Cannot extract draws from multiple variables in wide format."
  expect_error(spread_draws(RankCorr_s, c(tau, u_tau)[..]), error_message)
  expect_error(spread_draws(RankCorr_s, c(tau, u_tau)[i] | i), error_message)
})

test_that("spread_draws correctly extracts multiple variables simultaneously", {
  expect_equal(spread_draws(RankCorr_i, c(tau, u_tau)[i]),
    spread_draws(RankCorr_i, tau[i]) %>%
      inner_join(spread_draws(RankCorr_i, u_tau[i]), by = c(".chain", ".iteration", ".draw", "i")) %>%
          select(i, tau, u_tau, everything())
  )
  expect_equal(spread_draws(RankCorr_i, cbind(tau)[i]),
    spread_draws(RankCorr_i, c(tau)[i]))
  expect_equal(spread_draws(RankCorr_i, cbind(tau, u_tau)[i]),
    spread_draws(RankCorr_i, c(tau, u_tau)[i]))
})

test_that("spread_draws correctly extracts multiple variables simultaneously when those variables have no dimensions", {
  RankCorr_t = RankCorr_s
  dimnames(RankCorr_t)[[2]][[1]] = "tr2"

  ref1 = spread_draws(RankCorr_t, typical_r)
  expect_equal(spread_draws(RankCorr_t, c(typical_r)), ref1)

  ref2 = spread_draws(RankCorr_t, tr2) %>%
    inner_join(spread_draws(RankCorr_t, typical_r), by = c(".chain", ".iteration", ".draw"))
  expect_equal(spread_draws(RankCorr_t, c(tr2, typical_r)), ref2)
})

test_that("spread_draws multispec syntax joins results correctly", {
  ref = spread_draws(RankCorr_s, typical_r) %>%
    inner_join(spread_draws(RankCorr_s, tau[i]), by = c(".chain", ".iteration", ".draw")) %>%
    inner_join(spread_draws(RankCorr_s, b[i, v]), by = c(".chain", ".iteration", ".draw", "i")) %>%
    group_by(i, v)

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

  expect_equivalent(attr(draws, "groups")$i, factor(i_labels))
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


# tests for nested syntax -------------------------------------------------

test_that("nested matrices are correctly extracted", {
  test_draws = tibble(
    .chain = NA, .iteration = NA, .draw = 1,
    `x[1,1]`  = 1,
    `x[1,3]`  = 3,
    `x[1,2]`  = 2,
    `x[1,4]`  = 4,
    `x[1,5]`  = 5,
    `x[1,6]`  = 6,
    `x[1,7]`  = 7,
    `x[1,8]`  = 8,
    `x[1,9]`  = 9,
    `x[1,10]` = 10,
    `x[2,1]`  = 2,
    `x[2,4]`  = 5,
    `x[2,2]`  = 3,
    `x[2,3]`  = 4,
    `x[2,5]`  = 6,
    `x[2,6]`  = 7,
    `x[2,7]`  = 8,
    `x[2,8]`  = 9,
    `x[2,9]`  = 10,
    `x[2,10]` = 11,
  ) %>%
    rbind(. + 10)

  null_dimnames = function(v) {
    dimnames(v) = list(NULL, NULL)
    v
  }

  expect_equal(spread_draws_long_(test_draws, "x", c("i","."))[["x"]][[2]], 2:11)
  expect_equal(spread_draws_long_(test_draws, "x", c(".","."))[["x"]][[1]], null_dimnames(rbind(1:10, 2:11)))
  expect_equal(spread_draws_long_(test_draws, "x", c("1","2"))[["x"]][[1]], null_dimnames(rbind(1:10, 2:11)))
  expect_equal(spread_draws_long_(test_draws, "x", c("2","1"))[["x"]][[1]], null_dimnames(cbind(1:10, 2:11)))
  expect_equal(spread_draws_long_(test_draws, "x", c(".","."))[["x"]][[2]], null_dimnames(rbind(11:20, 12:21)))
  expect_equal(spread_draws_long_(test_draws, "x", c("2","1"))[["x"]][[2]], null_dimnames(cbind(11:20, 12:21)))
})

test_that("nested 3d arrays are correctly extracted", {
  test_draws = tibble(
    .chain = NA, .iteration = NA, .draw = 1,
    `x[1,1,1]`  = 111,
    `x[1,1,2]`  = 112,
    `x[1,1,3]`  = 113,
    `x[1,2,1]`  = 121,
    `x[1,2,2]`  = 122,
    `x[1,2,3]`  = 123,
    `x[2,2,1]`  = 221,
    `x[2,2,2]`  = 222,
    `x[2,2,3]`  = 223,
    `x[2,1,1]`  = 211,
    `x[2,1,2]`  = 212,
    `x[2,1,3]`  = 213
  ) %>%
    rbind(. + 10)

  #column-major order of the above 3d array
  ref = array(c(111, 211, 121, 221, 112, 212, 122, 222, 113, 213, 123, 223), dim = c(2,2,3))
  dimnames(ref) = list(NULL, NULL, NULL)

  expect_equal(spread_draws_long_(test_draws, "x", c(".",".","."))[["x"]][[1]], ref)
})

test_that("nested matrices with multiple variables are correctly extracted", {
  test_draws = tibble(
    .chain = NA, .iteration = NA, .draw = 1,
    `x[2,1]`  = 5,
    `x[2,3]`  = 7,
    `x[2,2]`  = 6,
    `x[2,4]`  = 8,
    `x[1,1]`  = 1,
    `x[1,2]`  = 2,
    `x[1,4]`  = 4,
    `x[1,3]`  = 3,
    `y[1,1]`  = 11,
    `y[1,2]`  = 12,
    `y[1,4]`  = 14,
    `y[1,3]`  = 13,
    `y[2,1]`  = 15,
    `y[2,2]`  = 16,
    `y[2,3]`  = 17,
    `y[2,4]`  = 18
  ) %>%
    rbind(. + 10)

  ref = rbind(11:14, 15:18)
  dimnames(ref) = list(NULL, NULL)

  expect_equal(spread_draws_long_(test_draws, c("x", "y"), c(".","."))[["y"]][[1]], ref)
})

test_that("nested matrices with named columns correctly extracted", {
  test_draws = tibble(
    .chain = NA, .iteration = NA, .draw = 1,
    `x[1,b]`  = 11,
    `x[1,a]`  = 12,
    `x[2,b]`  = 21,
    `x[2,a]`  = 22,
  ) %>%
    rbind(. + 10)

  expect_equal(spread_draws_long_(test_draws, "x", c(".","."))[["x"]][[1]],
    array(c(11,21,12,22), dim = c(2, 2), dimnames = list(NULL, c("b", "a"))))
})

test_that("nested ragged arrays are correctly extracted", {
  test_draws = tibble(
    .chain = NA, .iteration = NA, .draw = 1,
    `x[d,a]`  = 11,
    `x[d,b]`  = 12,
    `x[c,1]`  = 23,
    `x[c,b]`  = 22,
  ) %>%
    rbind(. + 10)

  expect_equal(spread_draws_long_(test_draws, "x", c(".","."))[["x"]][[1]],
    array(c(11,NA,12,22,NA,23), dim = c(2, 3), dimnames = list(c("d", "c"), c("a", "b", "1"))))
})

test_that("nested vectors are correctly extracted", {
  test_draws = tibble(
    .chain = NA, .iteration = NA, .draw = 1,
    `x[2]`  = 2,
    `x[1]`  = 1,
    `x[3]`  = 3,
  )

  expect_equal(spread_draws_long_(test_draws, "x", ".")[["x"]][[1]], c(1,2,3))
  test_draws[["x[d]"]] = 4
  expect_equal(spread_draws_long_(test_draws, "x", ".")[["x"]][[1]], c(`2` = 2, `1` = 1, `3` = 3, d = 4))
})

test_that("nested arrays with string names are correctly extracted", {
  test_draws = tibble(
    .chain = NA, .iteration = NA, .draw = 1,
    `x[a]`  = 1,
    `x[c]`  = 3,
    `x[b]`  = 2,
  )

  expect_equal(spread_draws_long_(test_draws, "x", ".")[["x"]][[1]], c(a = 1, c = 3, b = 2))
})

test_that("nested arrays with numeric indices that aren't 1:N are extracted properly", {
  test_draws = tibble(
    .chain = NA, .iteration = NA, .draw = 1,
    `x[2]`  = 2,
    `x[5]`  = 5,
    `x[3]`  = 3,
  )

  expect_equal(spread_draws_long_(test_draws, "x", ".")[["x"]][[1]], c(NA, 2, 3, NA, 5))
})

test_that("nested arrays with numeric indices that aren't 1:N are extracted properly", {
  test_draws = tibble(
    .chain = NA, .iteration = NA, .draw = 1,
    `x[1]`  = 4,
    `x[0]`  = 2,  # will be treated as string, so non-numerical order is kept
    `x[2]`  = 3,
  )

  expect_equal(spread_draws_long_(test_draws, "x", ".")[["x"]][[1]], c(`1` = 4, `0` = 2, `2` = 3))
})

# all_elements_identical ------------------------------------------------------------------

test_that("all_elements_identical base case: on empty list is TRUE", {
  expect_equal(all_elements_identical(list()), TRUE)
})

# abind0 ------------------------------------------------------------------

test_that("abind0 base cases", {
  expect_equal(abind0(c(1,2,3)), c(1,2,3))
  expect_equal(abind0(list(c(1,2,3))), c(1,2,3))
})
