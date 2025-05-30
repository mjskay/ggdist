# Tests for binning methods
#
# Author: mjskay
###############################################################################


test_that("binning works on symmetric distributions", {

  expect_equal(
    automatic_bin(0, width = .5),
    list(bins = 1, bin_midpoints = 0)
  )

  expect_equal(
    automatic_bin(c(1,2,3), width = 1),
    list(bins = c(1, 2, 3), bin_midpoints = c(1, 2, 3))
  )

  expect_equal(
    automatic_bin(c(1,2,3), width = 2.01),
    list(bins = c(1, 1, 1), bin_midpoints = 2)
  )

  expect_equal(
    automatic_bin(qnorm(ppoints(20, a = 1/2)), width = .5),
    list(
      bins = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 7, 7, 8),
      bin_midpoints = c(
        -1.95996398454005, -1.29494042565723,
        -0.766174708557979, -0.258234484056547, 0.258234484056547, 0.766174708557979,
        1.29494042565723, 1.95996398454005
      )
    )
  )

  expect_equal(
    automatic_bin(ppoints(12, a = 1/2), width = .25),
    list(
      bins = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
      bin_midpoints = c(0.125, 0.375, 0.625, 0.875)
    )
  )

  expect_equal(
    automatic_bin(ppoints(9, a = 1/2), width = .25),
    list(
      bins = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
      bin_midpoints = c(0.166666666666667, 0.5, 0.833333333333333)
    )
  )

  expect_equal(automatic_bin(c(1,2,2,3,3), width = 1),
    list(bins = c(1L, 2L, 2L, 3L, 3L), bin_midpoints = c(1, 2, 3))
  )

  expect_equal(automatic_bin(c(1,2,3), width = 1),
    list(bins = c(1, 2, 3), bin_midpoints = c(1, 2, 3))
  )

  expect_equal(automatic_bin(c(1,2,2,3), width = 1),
    list(bins = c(1, 2, 2, 3), bin_midpoints = c(1, 2, 3))
  )

  expect_equal(automatic_bin(c(1,2), width = 1.01),
    list(bins = c(1, 1), bin_midpoints = 1.5)
  )

  expect_equal(automatic_bin(c(1,2), width = 1),
    list(bins = c(1, 2), bin_midpoints = c(1, 2))
  )

  expect_equal(automatic_bin(c(1:5, 5.5), width = 4),
    list(bins = c(1, 1, 1, 2, 2, 2), bin_midpoints = c(2, 4.75))
  )

  expect_equal(automatic_bin(1:8, width = 2.1),
    list(bins = c(1, 1, 2, 2, 3, 3, 4, 4), bin_midpoints = c(1.5, 3.5, 5.5, 7.5))
  )
})

test_that("bin sweeping fixes edge effects", {
  x = sort(round(-qexp(ppoints(20), 1/4)))
  automatic_bin(x, 2)

  expect_equal(
    automatic_bin(x, 2),
    list(
      bins = c(1,2,3,3,4,4,5,5,5,5,5,5,5,5,6,6,6,6,6,6),
      bin_midpoints = c(-15,-10,-7.5,-5.5,-3,-0.5)
    )
  )
})

test_that("binning works on empty data", {

  expect_equal(automatic_bin(NULL, width = 1),
    list(bins = integer(0), bin_midpoints = numeric(0))
  )

  expect_equal(wilkinson_bin_from_center(NULL, width = 1),
    list(bins = integer(0), bin_midpoints = numeric(0))
  )

  expect_equal(wilkinson_bin_to_right(NULL, width = 1),
    list(bins = integer(0), bin_midpoints = numeric(0), bin_left = numeric(0), bin_right = numeric(0))
  )

  expect_equal(wilkinson_bin(NULL, width = 1),
    list(bins = integer(0), bin_midpoints = numeric(0))
  )
})


test_that("bin nudging works", {

  # when no nudging required
  expect_equal(
    nudge_bins(NULL, width = 1),
    NULL
  )
  expect_equal(
    nudge_bins(1, width = 1),
    1
  )
  expect_equal(
    nudge_bins(c(1,2), width = 1),
    c(1,2)
  )
  expect_equal(
    nudge_bins(c(1,2,3,4), width = 1),
    c(1,2,3,4)
  )
  expect_equal(
    nudge_bins(bin_midpoints = c(1,2,3), width = 1),
    c(1,2,3)
  )

  # on even number of bins
  expect_equal(
    nudge_bins(bin_midpoints = c(1,2), width = 1.1),
    c(0.95,2.05)
  )
  expect_equal(
    nudge_bins(c(1,2,3,4), width = 1.1),
    c(.85,1.95,3.05,4.15)
  )

  # on odd number of bins
  expect_equal(
    nudge_bins(c(1,2,3), width = 1.1),
    c(0.9,2,3.1)
  )
})


test_that("bin layouts work", {
  ref = data.frame(
    x = c(1, 1, 3, 5, 5),
    y = c(1, 3, 1, 1, 3),
    bin = c(1L,  1L, 2L, 3L, 3L)
  )
  expect_equal(as.data.frame(bin_dots(1:5, 0, binwidth = 2, layout = "bin")), ref)

  ref = data.frame(
    x = c(1, 2, 3, 4.5, 5.1),
    y = c(0.75, 2.25, 0.75, 0.75, 2.25),
    bin = c(1L,  1L, 2L, 3L, 3L)
  )
  expect_equal(as.data.frame(bin_dots(c(1:3, 4.5, 5.1), 0, binwidth = 1.5, layout = "weave")), ref)

  expect_error(bin_dots(1:5, 0, binwidth = 2, layout = "abc"))
})


test_that("swarm layout works", {
  skip_if_not_installed("beeswarm")

  ref = data.frame(
    x = 1:5,
    y = c(1, 2.73205080756888, 1, 2.73205080756888,  1),
    bin = c(1, 1, 2, 3, 3)
  )
  expect_equal(bin_dots(1:5, 0, binwidth = 2, layout = "swarm"), ref)
})


test_that("find_dotplot_binwidth edge cases work", {
  # tests that when the minimum and maximum number of bin in the search
  # are next to each other the max is chosen correctly
  expect_equal(find_dotplot_binwidth(c(1,1,2,2,3,3,4,4), 1), 0.5)
})


test_that("small bins work", {
  expect_equal(wilkinson_bin(c(0, .Machine$double.eps*2), .Machine$double.eps)$bins, 1:2)
})
