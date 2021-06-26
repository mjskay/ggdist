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

test_that("fct_rev_ works properly", {
  expect_equal(fct_rev_(c("a","b","c")), factor(c("a","b","c"), levels = c("c","b","a")))
  expect_error(fct_rev_(1:3), "must be a factor")
})
