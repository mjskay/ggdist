# Tests for geom helpers
#
# Author: mjskay
###############################################################################

context("geom")

test_that("geom aesthetic documention generator works", {
  # using as.data.frame here because comparison of tibbles with
  # list columns directly doesn't seem to work...
  expect_equal(
    as.data.frame(parse_dist(c("Normal(0,1)", "log-normal(2,3)", "CHI square(4)", "bad", "bad2(1"))),
    as.data.frame(tibble(.dist = c("norm", "lnorm", "chisq", NA, NA), .args = list(list(0,1), list(2,3), list(4), NA, NA)))
  )

  geom_output = paste0(rd_slabinterval_aesthetics(Geom), collapse = "\n")
  expect_match(geom_output, "@section Aesthetics:")
  expect_match(geom_output, "These geoms support the following aesthetics:")
  expect_match(geom_output, "\\\\itemize")
  expect_match(geom_output, "\\\\item \\\\code\\{group\\}")

  geom_stat_output = paste0(rd_slabinterval_aesthetics(Geom, stat = Stat), collapse = "\n")
  expect_match(geom_stat_output, "These stats support the following aesthetics:")
  expect_match(geom_stat_output, "@section Aesthetics:")
  expect_match(geom_stat_output, "\\\\itemize")
  expect_match(geom_stat_output, "\\\\item \\\\code\\{group\\}")

})

test_that("define_orientation_variables fails on incorrect orientation", {
  expect_error(define_orientation_variables("foo"), "Unknown orientation")
})
