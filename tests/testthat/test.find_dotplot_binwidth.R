# Tests for dynamic binning
#
# Author: mjskay
###############################################################################


test_that("find_dotplot_binwidth edge cases work", {
  # tests that when the minimum and maximum number of bin in the search
  # are next to each other the max is chosen correctly
  expect_equal(find_dotplot_binwidth(c(1,1,2,2,3,3,4,4), 1), 0.5)
})
