# Tests for abstract_geom
#
# Author: mjskay
###############################################################################



test_that("make_geom works", {

  geom = make_geom(GeomSlabinterval)

  args = setdiff(
    c(
      "mapping", "data", "stat", "position", "...",
      names(GeomSlabinterval$default_params),
      names(GeomSlabinterval$layer_args)
    ),
    GeomSlabinterval$hidden_params
  )
  expect_setequal(args, names(formals(geom)))


  geom = make_geom(GeomInterval)

  args = setdiff(
    c(
      "mapping", "data", "stat", "position", "...",
      names(GeomInterval$default_params),
      names(GeomInterval$layer_args)
    ),
    GeomInterval$hidden_params
  )
  expect_setequal(args, names(formals(geom)))

})
