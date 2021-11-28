# Tests for geom helpers
#
# Author: mjskay
###############################################################################



test_that("geom aesthetic documention generator works", {

  geom_output = paste0(rd_slabinterval_aesthetics("slabinterval"), collapse = "\n")
  expect_match(geom_output, "@section Aesthetics:")
  expect_match(geom_output, "Positional aesthetics")
  expect_match(geom_output, "\\\\itemize")
  expect_match(geom_output, "\\\\item \\\\code\\{group\\}")

  geom_stat_output = paste0(rd_slabinterval_aesthetics("slabinterval", stat = Stat), collapse = "\n")
  expect_match(geom_stat_output, "These `stat`s support the following aesthetics:")
  expect_match(geom_stat_output, "@section Aesthetics:")
  expect_match(geom_stat_output, "\\\\itemize")
  expect_match(geom_stat_output, "\\\\item \\\\code\\{group\\}")

})

test_that("define_orientation_variables fails on incorrect orientation", {
  expect_error(define_orientation_variables("foo"), "Unknown orientation")
})
