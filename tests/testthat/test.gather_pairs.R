# Tests for gather_pairs
#
# Author: mjskay
###############################################################################

library(dplyr)
library(forcats)

context("gather_pairs")

set.seed(1234)
t_a = rnorm(10)
t_b = rnorm(10, t_a * 2)
t_c = rnorm(10)

pairs_df = bind_rows(
  tibble(.chain = NA, .iteration = NA, .draw = 1:10, g = "a", t = t_a),
  tibble(.chain = NA, .iteration = NA, .draw = 1:10, g = "b", t = t_b),
  tibble(.chain = NA, .iteration = NA, .draw = 1:10, g = "c", t = t_c)
)

set_groups_and_levels = function(ref) {
  ref %>% mutate(
    .row = factor(.row, levels = c("a", "b", "c")),
    .col = factor(.col, levels = c("a", "b", "c"))
  ) %>%
    group_by(.row, .col)
}

aa_ref = tibble(.chain = NA, .iteration = NA, .draw = 1:10, .row = "a", .y = t_a, .col = "a", .x = t_a)
ab_ref = tibble(.chain = NA, .iteration = NA, .draw = 1:10, .row = "a", .y = t_a, .col = "b", .x = t_b)
ac_ref = tibble(.chain = NA, .iteration = NA, .draw = 1:10, .row = "a", .y = t_a, .col = "c", .x = t_c)
ba_ref = tibble(.chain = NA, .iteration = NA, .draw = 1:10, .row = "b", .y = t_b, .col = "a", .x = t_a)
bb_ref = tibble(.chain = NA, .iteration = NA, .draw = 1:10, .row = "b", .y = t_b, .col = "b", .x = t_b)
bc_ref = tibble(.chain = NA, .iteration = NA, .draw = 1:10, .row = "b", .y = t_b, .col = "c", .x = t_c)
ca_ref = tibble(.chain = NA, .iteration = NA, .draw = 1:10, .row = "c", .y = t_c, .col = "a", .x = t_a)
cb_ref = tibble(.chain = NA, .iteration = NA, .draw = 1:10, .row = "c", .y = t_c, .col = "b", .x = t_b)
cc_ref = tibble(.chain = NA, .iteration = NA, .draw = 1:10, .row = "c", .y = t_c, .col = "c", .x = t_c)

test_that("basic gather_pairs works with character keys", {

  upper_ref = set_groups_and_levels(bind_rows(aa_ref, ab_ref, ac_ref, bb_ref, bc_ref, cc_ref))
  upper_only_ref = set_groups_and_levels(bind_rows(ab_ref, ac_ref, bc_ref))
  lower_ref = set_groups_and_levels(bind_rows(aa_ref, ba_ref, bb_ref, ca_ref, cb_ref, cc_ref))
  lower_only_ref = set_groups_and_levels(bind_rows(ba_ref, ca_ref, cb_ref))
  both_ref = set_groups_and_levels(bind_rows(aa_ref, ab_ref, ac_ref, ba_ref, bb_ref, bc_ref, ca_ref, cb_ref, cc_ref))
  both_only_ref = set_groups_and_levels(bind_rows(ab_ref, ac_ref, ba_ref, bc_ref, ca_ref, cb_ref))

  expect_equal(gather_pairs(pairs_df, g, t, triangle = "upper"), upper_ref)
  expect_equal(gather_pairs(pairs_df, g, t, triangle = "upper only"), upper_only_ref)
  expect_equal(gather_pairs(pairs_df, g, t, triangle = "lower"), lower_ref)
  expect_equal(gather_pairs(pairs_df, g, t), lower_only_ref)
  expect_equal(gather_pairs(pairs_df, g, t, triangle = "lower only"), lower_only_ref)
  expect_equal(gather_pairs(pairs_df, g, t, triangle = "both"), both_ref)
  expect_equal(gather_pairs(pairs_df, g, t, triangle = "both only"), both_only_ref)
})
