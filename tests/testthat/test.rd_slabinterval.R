# Tests for geom helpers
#
# Author: mjskay
###############################################################################



test_that("slabinterval aesthetic documention generator works", {

  geom_output = paste0(rd_slabinterval_aesthetics("slabinterval"), collapse = "\n")
  expect_match(geom_output, "@section Aesthetics:")
  expect_match(geom_output, "Positional aesthetics")
  expect_match(geom_output, "\\itemize", fixed = TRUE)
  expect_match(geom_output, "\\item{`group`", fixed = TRUE)

  geom_stat_output = paste0(rd_slabinterval_aesthetics("slabinterval", stat = StatSlabinterval), collapse = "\n")
  expect_match(geom_stat_output, "These `stat`s support the following aesthetics:")
  expect_match(geom_stat_output, "@section Aesthetics:")
  expect_match(geom_stat_output, "\\itemize", fixed = TRUE)
  expect_match(geom_stat_output, "\\item{`group`", fixed = TRUE)
})

test_that("slabinterval parameter documention generator works", {

  geom_output = paste0(rd_layer_params("slabinterval"), collapse = "\n")
  expect_match(geom_output, "@param na.rm", fixed = TRUE)

  # parameters without documentation raise a warning
  GeomTest <<- ggproto("GeomTest", GeomSlabinterval,
    default_params = defaults(list(
      foo = "bar"
    ), GeomSlabinterval$default_params)
  )
  expect_error(rd_layer_params("test"), "Missing docs for params: foo")

})

test_that("shortcut stat_slabinterval documentation generator works", {

  stat_output = paste0(rd_slabinterval_shortcut_stat("interval", chart_type = "multiple-interval"), collapse = "\n")
  expect_match(stat_output, "@title Multiple-interval plot (shortcut stat)", fixed = TRUE)
  expect_match(stat_output, ".width = c(0.5, 0.8, 0.95)", fixed = TRUE)

  stat_output = paste0(rd_slabinterval_shortcut_stat(
    "halfeye", geom_name = "slabinterval", chart_type = "half-eye", example_layers = "scale_fill_brewer()"
  ), collapse = "\n")
  expect_match(stat_output, "`f`: (deprecated) For slabs", fixed = TRUE)
  expect_match(stat_output, "stat_halfeye() +\n  scale_fill_brewer()", fixed = TRUE)

})

test_that("shortcut stat_spike documentation generator works", {

  stat_output = paste0(rd_slabinterval_shortcut_stat("spike", chart_type = "spike"), collapse = "\n")
  expect_match(stat_output, "@title Spike plot (shortcut stat)", fixed = TRUE)
  expect_match(stat_output, "- `at`:", fixed = TRUE)

})

test_that("shortcut geom_slabinterval documentation generator works", {

  geom_output = paste0(rd_slabinterval_shortcut_geom("interval", chart_type = "multiple-interval"), collapse = "\n")
  expect_match(geom_output, "@title Multiple-interval plot (shortcut geom)", fixed = TRUE)
  expect_match(geom_output, "interval_size_range = c(1, 6)", fixed = TRUE)

})

test_that("param docs work", {
  expect_match(rd_param_slab_side(), "@param side")
})
