# Tests for geom helpers
#
# Author: mjskay
###############################################################################



test_that("lineribbon aesthetic documention generator works", {

  geom_output = paste(rd_lineribbon_aesthetics("lineribbon"), collapse = "\n")
  expect_match(geom_output, "@section Aesthetics:")
  expect_match(geom_output, "Positional aesthetics")
  expect_match(geom_output, "\\itemize", fixed = TRUE)
  expect_match(geom_output, "\\item{`group`", fixed = TRUE)

  geom_stat_output = paste(rd_lineribbon_aesthetics("lineribbon", stat = StatLineribbon), collapse = "\n")
  expect_match(geom_stat_output, "These `stat`s support the following aesthetics:")
  expect_match(geom_stat_output, "@section Aesthetics:")
  expect_match(geom_stat_output, "\\itemize", fixed = TRUE)
  expect_match(geom_stat_output, "\\item{`group`", fixed = TRUE)

})

test_that("shortcut stat documentation generator works", {

  stat_output = paste(
    rd_lineribbon_shortcut_stat("lineribbon", chart_type = "multiple-ribbon", from_name = "slabinterval"),
    collapse = "\n"
  )
  expect_match(stat_output, "@title Multiple-ribbon plot (shortcut stat)", fixed = TRUE)
  expect_match(stat_output, ".width = c(0.5, 0.8, 0.95)", fixed = TRUE)

  stat_output = paste(rd_lineribbon_shortcut_stat(
    "lineribbon", geom_name = "lineribbon", chart_type = "line+ribbon"
  ), collapse = "\n")
  expect_match(stat_output, "`linewidth`: Width", fixed = TRUE)
  expect_match(stat_output, "stat_lineribbon()", fixed = TRUE)

})
