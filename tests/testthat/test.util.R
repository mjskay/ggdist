# Tests for util
#
# Author: mjskay
###############################################################################

context("util")


test_that(".Deprecated_argument_alias works properly", {

  expect_warning(point_interval(0:10, .prob = .50), paste0(
    "In point_interval\\.numeric\\(\\)\\: The `\\.prob` argument is a deprecated alias for `\\.width`\\.\n",
    "Use the `\\.width` argument instead\\.\n",
    "See help\\(\"tidybayes-deprecated\"\\) or help\\(\"ggdist-deprecated\"\\)\\."
  ))

})
