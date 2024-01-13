# Tests for density estimators
#
# Author: mjskay
###############################################################################


# drop the portion of a density() object that depends on the input function
# call expression (to make comparison across functions easier)
drop_call = function(x) {
  x[!names(x) %in% c("call", "data.name")]
}

test_that("density_bounded works", {
  x = 1:1000

  expect_equal(density_bounded(adjust = 0.5)(x), density_bounded(x, adjust = 0.5))
  expect_equal(drop_call(density_bounded(1:3)), drop_call(density_bounded(c(1:3, NA), na.rm = TRUE)))
  # density for Uniform(1, 1000) is 1/999, estimator should be close-ish to that
  expect_true(all(abs(density_bounded(x)$y - 1/999) < .0001))
  # default bounder is a bit looser, these tighter ones should be even closer to correct:
  expect_true(all(abs(density_bounded(x, bounder = bounder_cdf(p = 0.5))$y - 1/999) < .00001))
  expect_true(all(abs(density_bounded(x, bounder = "cooke")$y - 1/999) < .00001))
  expect_true(all(abs(density_bounded(x, bounder = "range")$y - 1/999) < .00001))
  expect_true(all(abs(density_bounded(x, bounds = c(1,1000))$y - 1/999) < .00001))

  expect_error(density_bounded(1:2, n = 0), "`n` of at least 1")
  expect_error(density_bounded(1:4, bounds = c(1,2)), "`x` must be inside `bounds`")
  expect_error(density_bounded(c(1, 2, NA)), "must not contain missing \\(NA\\) values")
})

test_that("density_unbounded works", {
  x = 1:10

  expect_equal(density_unbounded(adjust = 0.5)(x), density_unbounded(x, adjust = 0.5))
  expect_equal(drop_call(density_unbounded(1:3)), drop_call(density_unbounded(c(1:3, NA), na.rm = TRUE)))

  ref = density(x, bw = bw.SJ(x, method = "dpi"))
  ref$cdf = ecdf(x)(ref$x)
  expect_equal(drop_call(density_unbounded(x)), drop_call(ref))

  expect_error(density_unbounded(1:2, n = 0), "`n` of at least 1")
  expect_error(density_unbounded(c(1, 2, NA)), "must not contain missing \\(NA\\) values")
})

test_that("density_bounded(range_only = TRUE) works", {
  expect_equal(
    density_bounded(range_only = TRUE, trim = TRUE, bounder = bounder_cooke())(1:2),
    list(x = 1:2, y = c(NA_real_, NA_real_))
  )

  expect_equal(
    density_bounded(range_only = TRUE, trim = FALSE, bounder = bounder_cooke())(1:2),
    list(x = c(0.75, 2.25), y = c(NA_real_, NA_real_))
  )
})


# bandwidth ---------------------------------------------------------------

test_that("bandwidth estimators work", {
  x = 1:1000

  expect_equal(bandwidth_nrd0()(x), bw.nrd0(x))
  expect_equal(bandwidth_nrd()(x), bw.nrd(x))
  expect_equal(bandwidth_ucv()(x), bw.ucv(x))
  expect_equal(bandwidth_bcv()(x), bw.bcv(x))
  expect_equal(bandwidth_SJ()(x), bw.SJ(x))
  expect_equal(bandwidth_dpi()(x), bw.SJ(x, method = "dpi"))
})

test_that("bandwidth fallback works", {
  x = c(rep(1, 10), 1.1)

  expect_warning(expect_equal(bandwidth_nrd(x), bw.nrd0(x)), class = "ggdist_bandwidth_fallback_warning")
  expect_warning(expect_equal(bandwidth_SJ(x), bw.nrd0(x)), class = "ggdist_bandwidth_fallback_warning")
  expect_warning(expect_equal(bandwidth_dpi(x), bw.nrd0(x)), class = "ggdist_bandwidth_fallback_warning")
})


# adaptive density estimator ----------------------------------------------

test_that("adaptive density estimator works", {
  skip_if_no_vdiffr()
  skip_if_sensitive_to_density()


  x = qlnorm(ppoints(1000), 1/2, 1)

  # red (adaptive) KDE should do better near the mode
  vdiffr::expect_doppelganger("adaptive KDE (red) better matches mode",
    ggplot() +
      stat_slab(aes(xdist = distributional::dist_lognormal(1/2, 1)), fill = NA, color = "gray50") +
      stat_slab(aes(x), density = density_bounded(adapt = 1, bandwidth = "dpi", bounds = c(0, Inf)), fill = NA, color = "blue", linetype = "11") +
      stat_slab(aes(x), density = density_bounded(adapt = 100, bandwidth = "dpi", bounds = c(0, Inf)), fill = NA, color = "red", linetype = "11") +
      scale_thickness_shared() +
      coord_cartesian(xlim = c(0, 10))
  )


  x = qlnorm(ppoints(100), 1/2, 1)

  vdiffr::expect_doppelganger("adapt = n better matches mode",
    ggplot() +
    stat_slab(aes(xdist = distributional::dist_lognormal(1/2, 1)), fill = NA, color = "gray50") +
    stat_slab(aes(x), density = density_bounded(adapt = 1, bandwidth = "dpi", bounds = c(0, Inf)), fill = NA, color = "blue", linetype = "11") +
    stat_slab(aes(x), density = density_bounded(adapt = 100, bandwidth = "dpi", bounds = c(0, Inf)), fill = NA, color = "red", linetype = "11") +
    scale_thickness_shared() +
    coord_cartesian(xlim = c(0, 10))
  )
})


# weighted density --------------------------------------------------------

test_that("weighted density estimator works", {
  x1 = 1:10
  w1 = rep(1, 10)
  x2 = c(1,2,10)
  w2 = rep(2, 3)
  x = c(x1, x2)
  w = c(w1, w2)

  du = density_unbounded(x, weights = w, bandwidth = 0.5)
  d = density(x, weights = w/sum(w), bw = 0.5)

  expect_equal(du$x, d$x)
  expect_equal(du$y, d$y)
})


