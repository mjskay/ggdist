# Tests for abstract_stat
#
# Author: mjskay
###############################################################################



test_that("make_stat works", {

  stat = make_stat(StatSlabinterval, geom = "slabinterval")

  args = setdiff(
    c(
      "mapping", "data", "geom", "position", "...",
      names(StatSlabinterval$default_params),
      names(StatSlabinterval$layer_args)
    ),
    StatSlabinterval$hidden_params
  )

  expect_setequal(args, names(formals(stat)))

})
