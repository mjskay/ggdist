# Tests for dotsinterval documentation generators
#
# Author: mjskay
###############################################################################



test_that("dotsinterval aesthetic documention generator works", {

  geom_output = paste(rd_dotsinterval_aesthetics("dotsinterval"), collapse = "\n")
  expect_match(geom_output, "@section Aesthetics:")
  expect_match(geom_output, "Positional aesthetics")
  expect_match(geom_output, "\\itemize", fixed = TRUE)
  expect_match(geom_output, "\\item{`group`", fixed = TRUE)

  geom_stat_output = paste(rd_dotsinterval_aesthetics("dotsinterval", stat = StatDotsinterval), collapse = "\n")
  expect_match(geom_stat_output, "These `stat`s support the following aesthetics:")
  expect_match(geom_stat_output, "@section Aesthetics:")
  expect_match(geom_stat_output, "\\itemize", fixed = TRUE)
  expect_match(geom_stat_output, "\\item{`group`", fixed = TRUE)
})

test_that("shortcut stat_dotsinterval documentation generator works", {

  stat_output = paste(rd_dotsinterval_shortcut_stat("dots", chart_type = "dot"), collapse = "\n")
  expect_match(stat_output, "@title Dot plot (shortcut stat)", fixed = TRUE)
  expect_match(stat_output, "show_interval = FALSE", fixed = TRUE)

})

test_that("shortcut geom_dotsinterval documentation generator works", {

  geom_output = paste(rd_dotsinterval_shortcut_geom("dots", chart_type = "dot"), collapse = "\n")
  expect_match(geom_output, "@title Dot plot (shortcut geom)", fixed = TRUE)
  expect_match(geom_output, "show_point = FALSE", fixed = TRUE)

})

test_that("param docs work", {
  expect_match(rd_param_dots_layout(), "@param layout")
  expect_match(rd_param_dots_overlaps(), "@param overlaps")
})
