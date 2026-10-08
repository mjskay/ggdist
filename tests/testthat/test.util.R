# Tests for util
#
# Author: mjskay
###############################################################################


test_that("all_names works", {
  expect_equal(all_names(1), NULL)
  expect_error(
    all_names(list()),
    'Don\'t know how to handle type "list"'
  )
})

test_that(".Deprecated_argument_alias works properly", {

  expect_warning(point_interval(0:10, .prob = .50), paste0(
    "In point_interval\\.numeric\\(\\)\\: The `\\.prob` argument is a deprecated alias for `\\.width`\\.\n",
    "Use the `\\.width` argument instead\\.\n",
    "See help\\(\"tidybayes-deprecated\"\\) or help\\(\"ggdist-deprecated\"\\)\\."
  ))

})

test_that(".Deprecated_arguments works properly", {

  foo = function(new_arg, ...) {
    .Deprecated_arguments(
      c("old_arg1", "old_arg2"), ...,
      message = "Use new_arg instead"
    )
    new_arg
  }

  expect_error(foo(old_arg1 = 1), "The `old_arg1` argument is deprecated.*Use new_arg instead")
  expect_error(geom_pointinterval(size_domain = 1), "The `size_domain` argument is deprecated.")

})


# rev_order ----------------------------------------------------------------

test_that("rev_order works properly", {
  expect_equal(rev_order(c("a","b","c")), ordered(c("a","b","c"), levels = c("c","b","a")))
})


# dlply_ ------------------------------------------------------------------

test_that("dlply_ works properly", {
  df = data.frame(
    x = 1:8,
    g = c(rep("a", 2), rep("(Missing)", 2), rep("(Missing)+", 2), rep(NA, 2)),
    stringsAsFactors = FALSE
  )

  expect_equal(
    dlply_(df, "g", identity),
    list(
      new_data_frame(df[3:4,]),
      new_data_frame(df[5:6,]),
      new_data_frame(df[1:2,]),
      new_data_frame(df[7:8,])
    )
  )

  expect_equal(dlply_(df, NULL, identity), list(df))

})


# stop_if_not_installed ---------------------------------------------------

test_that("stop_if_not_installed works properly", {
  e = tryCatch(stop_if_not_installed("_fake_package"), error = function(e) e)
  expect_s3_class(e, "ggdist_missing_package")
  expect_s3_class(e, "error")
  expect_equal(e$ggdist_package, "_fake_package")
})


# sequences ----------------------------------------------------------------------------------

test_that("seq_interleaved_grouped works", {
  expect_equal(
    seq_interleaved_grouped(c(2, 1, 3, 4, 4, 5, 3, 4, 1, 1, 2, 3)),
    c(2L, 10L, 9L, 11L, 1L, 12L, 3L, 7L, 4L, 8L, 5L, 6L)
  )

  expect_equal(
    seq_interleaved_grouped(c(1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5)),
    c(1L, 3L, 2L, 5L, 4L, 8L, 6L, 7L, 9L, 11L, 10L, 12L)
  )

  expect_equal(
    seq_interleaved_grouped(c(1, 2, 5, 3, 6, 8, 4, 4, 3, 2, 34, 4, 6, 76, 3, 2)),
    c(1L, 16L, 2L, 10L, 4L, 15L, 9L, 12L, 7L, 8L, 3L, 13L, 5L, 6L, 11L, 14L)
  )
})

test_that("seq_interleaved_centered_grouped works", {
  expect_equal(
    seq_interleaved_centered_grouped(rep(1, 8)),
    c(4L, 6L, 2L, 8L, 1L, 7L, 3L, 5L)
  )
})

test_that("seq_interleaved_grouped works on binned groups", {
  bins =       c(1,1,1,2,2,2,2,3,4,4,5,5,5,5,5,5,5,7,7,8,9,9,9,9,9,10,10,10,10)
  bin_groups = c(1,1,2,1,1,2,2,1,2,2,1,2,2,1,2,2,2,1,1,2,2,2,2,2,2, 1, 1, 1, 1)
  first_index = rank(bins, ties.method = "min")
  ref = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 14, 12, 17, 13, 16, 15, 18, 19, 20, 21, 25, 22, 24, 23, 26, 29, 27, 28)

  expect_equal(
    data.frame(bins, bin_groups, first_index) |>
      dlply_("bins", \(d) seq_interleaved_grouped(d$bin_groups) + c(d$first_index) - 1) |>
      unlist(),
    ref
  )

  expect_equal(seq_interleaved_grouped(bin_groups, bins), ref)
})

test_that("seq_interleaved_centered_grouped works on binned groups", {
  bins =       c(1,1,1,2,2,2,2,3,4,4,5,5,5,5,5,5,5,7,7,8,9,9,9,9,9,10,10,10,10)
  bin_groups = c(1,1,2,1,1,2,2,1,2,2,1,2,2,1,2,2,2,1,1,2,2,2,2,2,2, 1, 1, 1, 1)
  first_index = rank(bins, ties.method = "min")
  ref = c(2, 1, 3, 7, 5, 4, 6, 8, 9, 10, 16, 13, 14, 11, 17, 12, 15, 18, 19, 20, 22, 24, 21, 25, 23, 27, 29, 26, 28)

  expect_equal(
    data.frame(bins, bin_groups, first_index) |>
      dlply_("bins", \(d) seq_interleaved_centered_grouped(d$bin_groups) + c(d$first_index) - 1) |>
      unlist(),
    ref
  )

  expect_equal(seq_interleaved_centered_grouped(bin_groups, bins), ref)
})
