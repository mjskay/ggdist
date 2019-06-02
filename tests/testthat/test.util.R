# Tests for util
#
# Author: mjskay
###############################################################################

context("util")


test_that("combine_chains_for_deprecated_ works properly", {
  df = data.frame(
    .chain = c(1,1,1,1,1,2,2,2,2,2),
    .iteration = c(1:5, 1:5),
    .draw = c(1:10)
  )

  ref = data.frame(
    .chain = NA_integer_,
    .iteration = c(1:10)
  )

  expect_equal(combine_chains_for_deprecated_(df), ref)
})

test_that(".Deprecated_argument_alias works properly", {

  expect_warning(point_interval(0:10, .prob = .50), paste0(
    "In point_interval\\.numeric\\(\\)\\: The `\\.prob` argument is a deprecated alias for `\\.width`\\.\n",
    "Use the `\\.width` argument instead\\.\n",
    "See help\\(\"tidybayes-deprecated\"\\)\\."
  ))

})
