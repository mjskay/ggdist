# Tests for dynamic binning
#
# Author: mjskay
###############################################################################


test_that("find_dotplot_binwidth edge cases work", {
  # tests that when the minimum and maximum number of bin in the search
  # are next to each other the max is chosen correctly
  for (layout in list("bin", "bar", layout_swarm(strata = 5), layout_swarm(strata = Inf))) {
    expect_equal(find_dotplot_binwidth(c(1,1,2,2,3,3,4,4), 1, layout = !!layout), 0.5)
  }
})


test_that("split_monotonic works", {
  df = data.frame(
    x = 1:10,
    y = c((1:3)^2, 3^2, 4^2, 3^2, 7^2, (4:6)^2)
  )

  ref = list(
    data.frame(
      x = c(1L, 2L, 3L, 4L, 6L, 8L, 9L, 10L),
      y = c(1, 4, 9, 9, 9, 16, 25, 36)
    ),
    data.frame(
      x = c(1L, 2L, 3L, 4L, 6L, 7L),
      y = c(1, 4, 9, 9, 9, 49)
    ),
    data.frame(
      x = c(1L, 2L, 3L, 4L, 5L, 7L),
      y = c(1, 4, 9, 9, 16, 49)
    )
  )

  expect_equal(split_monotonic(df) |> lapply(`rownames<-`, NULL), ref)
})
