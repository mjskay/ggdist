# Tests for spike documentation
#
# Author: mjskay
###############################################################################


test_that("spike aesthetic documention generator works", {

  geom_output = paste0(rd_spike_aesthetics("spike"), collapse = "\n")
  expect_match(geom_output, "@section Aesthetics:")
  expect_match(geom_output, "Positional aesthetics")
  expect_match(geom_output, "\\itemize", fixed = TRUE)
  expect_match(geom_output, "\\item{`group`", fixed = TRUE)

  geom_stat_output = paste0(rd_spike_aesthetics("lineribbon", stat = StatLineribbon), collapse = "\n")
  expect_match(geom_stat_output, "These `stat`s support the following aesthetics:")
  expect_match(geom_stat_output, "@section Aesthetics:")
  expect_match(geom_stat_output, "\\itemize", fixed = TRUE)
  expect_match(geom_stat_output, "\\item{`group`", fixed = TRUE)

})
