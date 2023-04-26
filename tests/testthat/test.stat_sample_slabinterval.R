# Tests for sample distribution plots
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)



test_that("gradientinterval works", {
  skip_if_no_vdiffr()


  set.seed(1234)
  p = tribble(
    ~dist,  ~x,
    "norm", rnorm(100),
    "t",    rt(100, 3)
  ) %>%
    unnest(x) %>%
    ggplot() +
    scale_slab_alpha_continuous(range = c(0,1))

  vdiffr::expect_doppelganger("gradientinterval with two groups",
    p + stat_gradientinterval(aes(x = dist, y = x), n = 15, fill_type = "segments")
  )
  vdiffr::expect_doppelganger("gradientintervalh with two groups",
    p + stat_gradientinterval(aes(y = dist, x = x), n = 15, fill_type = "segments")
  )

})

test_that("fill_type = 'gradient' works", {
  skip_if_no_vdiffr()
  skip_if_no_linearGradient()


  set.seed(1234)
  p = tribble(
    ~dist,  ~x,
    "norm", rnorm(100),
    "t",    rt(100, 3)
  ) %>%
    unnest(x) %>%
    ggplot() +
    scale_slab_alpha_continuous(range = c(0,1))

  vdiffr::expect_doppelganger("fill_type = gradient with two groups",
    p + stat_gradientinterval(aes(x = dist, y = x), n = 15, fill_type = "gradient"),
    writer = write_svg_with_gradient
  )
  vdiffr::expect_doppelganger("fill_type = gradient with two groups, h",
    p + stat_gradientinterval(aes(y = dist, x = x), n = 15, fill_type = "gradient"),
    writer = write_svg_with_gradient
  )

})

test_that("histinterval and slab work", {
  skip_if_no_vdiffr()


  set.seed(1234)
  p = tribble(
    ~dist,  ~x,
    "norm", rnorm(100),
    "t",    rt(100, 3)
  ) %>%
    unnest(x) %>%
    ggplot()

  vdiffr::expect_doppelganger("histinterval with outline",
    p + stat_histinterval(aes(x = dist, y = x), slab_color = "black")
  )
  vdiffr::expect_doppelganger("histintervalh with outline",
    p + stat_histinterval(aes(y = dist, x = x), slab_color = "black", breaks = seq(-5, 7, length.out = 20))
  )
  vdiffr::expect_doppelganger("histinterval with outlines bw bars",
    p + stat_histinterval(aes(x = dist, y = x), slab_color = "black", outline_bars = TRUE)
  )

  vdiffr::expect_doppelganger("slab with outline",
    p + stat_slab(aes(x = dist, y = x), n = 20, slab_color = "black")
  )

})

test_that("scale transformation works", {
  skip_if_no_vdiffr()


  p_log = data.frame(x = 10^c(-1, -0.55, -0.35, -0.15, -0.05, -0.01, 0.01, 0.05, 0.15, 0.35, 0.55, 1)) %>%
    ggplot(aes(y = "a", x = x)) +
    scale_x_log10(breaks = 10^seq(-5,7, by = 2))

  vdiffr::expect_doppelganger("halfeyeh log scale transform",
    p_log + stat_halfeye(point_interval = mode_hdci, n = 20, density = density_unbounded(kernel = "tri"), .width = .5)
  )

  vdiffr::expect_doppelganger("ccdfintervalh log scale transform",
    p_log + stat_ccdfinterval(point_interval = mean_hdi, n = 15, .width = .5, trim = FALSE)
  )

  vdiffr::expect_doppelganger("cdfintervalh log scale transform",
    p_log + stat_cdfinterval(point_interval = mean_hdi, n = 15, .width = .5, trim = FALSE)
  )

  vdiffr::expect_doppelganger("histintervalh log scale transform",
    p_log + stat_histinterval(point_interval = median_qi, n = 10, .width = .5)
  )

})

test_that("pdf and cdf aesthetics work", {
  skip_if_no_vdiffr()


  p = data.frame(
    x = c("a", "b"),
    y = qnorm(ppoints(100), c(1, 2), 2)
  ) %>%
    ggplot(aes(x = x, y = y))

  vdiffr::expect_doppelganger("pdf and cdf on a sample slabinterval",
    p + stat_sample_slabinterval(aes(fill = x, thickness = after_stat(pdf), slab_alpha = after_stat(cdf)), n = 15)
  )
})

test_that("constant distributions work", {
  skip_if_no_vdiffr()


  p = data.frame(
    x = c("constant = 1", "normal(2,1)"),
    # sd of 0 will generate constant dist
    y = qnorm(ppoints(100), mean = c(1, 2), sd = c(0, 1))
  ) %>%
    ggplot(aes(x = x, y = y))

  vdiffr::expect_doppelganger("constant dist on halfeye",
    p + stat_halfeye(n = 15, slab_color = "blue")
  )

  vdiffr::expect_doppelganger("constant dist on histinterval",
    p + stat_histinterval(n = 15, slab_color = "blue")
  )

  vdiffr::expect_doppelganger("constant dist on ccdf",
    p + stat_ccdfinterval(trim = FALSE)
  )

  # constant dist when n = 1
  p = data.frame(
    x = c("constant = 1", "constant = 2", "constant = 3"),
    y = c(0,1,2)
  ) %>%
    ggplot(aes(x = x, y = y))

  vdiffr::expect_doppelganger("constant dist on halfeye with n = 1",
    p + stat_sample_slabinterval(n = 15, slab_color = "blue")
  )

  vdiffr::expect_doppelganger("constant dist on ccdf with n = 1",
    p + stat_ccdfinterval()
  )
})

test_that("side and justification can vary", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("varying side and just",
    mtcars %>%
      ggplot(aes(x = mpg, y = cyl,
        side = case_when(cyl == 4 ~ "top", cyl == 6 ~ "both", cyl == 8 ~ "bottom"),
        justification = case_when(cyl == 4 ~ 0, cyl == 6 ~ 0.5, cyl == 8 ~ 1),
        scale = case_when(cyl == 4 ~ 0.5, cyl == 6 ~ 1, cyl == 8 ~ 0.5)
      )) +
      stat_sample_slabinterval(orientation = "horizontal", normalize = "groups", n = 15)
  )
})

test_that("n is calculated correctly", {
  skip_if_no_vdiffr()

  set.seed(1234)
  df = data.frame(
    g = c("a","a","a","b","c"),
    x = rnorm(120, c(1,1,1,2,3))
  )

  vdiffr::expect_doppelganger("pdf*n for different-sized groups",
    df %>%
      ggplot(aes(x = x, y = g, thickness = after_stat(pdf*n), fill = after_stat(n))) +
      stat_sample_slabinterval(n = 15)
  )
})


# missing data is handled correctly ---------------------------------------

test_that("NAs are handled correctly", {
  skip_if_no_vdiffr()

  p = data.frame(x = c(1:5000, NA)) %>%
    ggplot(aes(x = x, y = "a"))

  expect_warning(
    vdiffr::expect_doppelganger("NAs with na.rm = FALSE",
      p + stat_halfeye(na.rm = FALSE, n = 15)
    ),
    "Removed 1 rows"
  )

  vdiffr::expect_doppelganger("NAs with na.rm = TRUE",
    p + stat_halfeye(na.rm = TRUE, n = 15)
  )
})


# trim and expand ---------------------------------------------------------

test_that("trim and expand work", {
  skip_if_no_vdiffr()

  set.seed(1234)
  df = data.frame(
    g = c("a","a","a","b","c"),
    x = rnorm(120, c(1,1,1,2,3))
  )

  vdiffr::expect_doppelganger("untrimmed and expanded",
    df %>%
      ggplot(aes(x = x, y = g)) +
      stat_sample_slabinterval(n = 15, slab_color = "black", expand = TRUE, trim = FALSE)
  )
})

test_that("expand can take length two vector", {

  df = data.frame(
    g = c("a","a","b","b"),
    x = c(1,2,2,3)
  )

  p = df %>%
    ggplot(aes(x = x, y = g)) +
    lims(x = c(0, 4))

  ld = layer_data(p + stat_ccdfinterval(expand = c(TRUE, TRUE)))
  expect_equal(min(ld$x), 0)
  expect_equal(max(ld$x), 4)

  ld = layer_data(p + stat_ccdfinterval(
    expand = c(TRUE, FALSE),
    density = density_bounded(bounder = "range")
  ))
  expect_equal(min(ld$x), 0)
  expect_equal(max(ld$x), 3)

  ld = layer_data(p + stat_ccdfinterval(
    expand = c(FALSE, TRUE),
    density = density_bounded(bounder = "range")
  ))
  expect_equal(min(ld$x), 1)
  expect_equal(max(ld$x), 4)

  ld = layer_data(p + stat_ccdfinterval(
    expand = c(FALSE, FALSE),
    density = density_bounded(bounder = "range")
  ))
  expect_equal(min(ld$x), 1)
  expect_equal(max(ld$x), 3)
})


# discrete distributions --------------------------------------------------

test_that("characters work", {
  p = ggplot_build(
    ggplot() +
      stat_slabinterval(aes(x = c("a","a","a","b","b","c"), group = NA))
  )

  slab_ref = data.frame(
    thickness = c(3,3,3,3,3,3, 2,2,2,2,2,2, 1,1,1,1,1,1)/6,
    pdf = c(3,3,3,3,3,3, 2,2,2,2,2,2, 1,1,1,1,1,1)/6,
    cdf = c(0,0,0, 3,3,3,3,3,3, 5,5,5,5,5,5, 6,6,6)/6,
    f = c(3,3,3,3,3,3, 2,2,2,2,2,2, 1,1,1,1,1,1)/6,
    n = 6,
    datatype = "slab",
    .width = c(NA,NA, .66,.66,.66,.66,.66,.66,.66,.66, .95,.95,.95,.95, NA,NA,NA,NA),
    stringsAsFactors = FALSE
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(.5,.5, 1,1, 1.5,1.5,1.5,1.5, 2,2, 2.5,2.5,2.5,2.5, 3,3, 3.5,3.5))
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "slab", names(slab_ref)], slab_ref)

  interval_ref = data.frame(
    datatype = "interval",
    .width = c(0.66, 0.95),
    stringsAsFactors = FALSE
  )
  interval_ref$xmin = ggplot2:::mapped_discrete(c(1, 1))
  interval_ref$xmax = ggplot2:::mapped_discrete(c(2.15, 2.875))
  interval_ref$x = ggplot2:::mapped_discrete(c(1.5, 1.5))
  attr(interval_ref, "row.names") = c(19L, 20L)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "interval", names(interval_ref)], interval_ref)
})

test_that("logical conditions at bin edges on histograms work", {
  p = ggplot() +
    stat_slab(
      aes(x = c(1,1,2,2,2,3,3,3,3), fill = after_stat(x > 1.5)),
      density = "histogram",
      breaks = breaks_fixed(width = 1),
      align = "center"
    ) +
    scale_fill_manual(values = c("red", "blue"))

  ref = data.frame(
    x = c(0.5, 0.5, 1, 1, 1.5, 1.5, 1.5, 1.5, 2, 2, 2.5, 2.5, 2.5, 2.5,  3, 3, 3.5, 3.5),
    fill = c(rep("red", 7), rep("blue", 11)),
    stringsAsFactors = FALSE
  )
  expect_equal(layer_data(p)[,c("x", "fill")], ref)

  # with outline
  p = ggplot() +
    stat_slab(
      aes(x = c(1,1,2,2,2,3,3,3,3), fill = after_stat(x > 1.5)),
      density = "histogram",
      breaks = breaks_fixed(width = 1),
      align = "center",
      outline_bars = TRUE,
      color = "black"
    ) +
    scale_fill_manual(values = c("red", "blue"))

  ref = data.frame(
    x = c(0.5, 0.5, 0.5, 1, 1, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 2, 2, 2.5,  2.5, 2.5, 2.5, 2.5, 2.5, 3, 3, 3.5, 3.5, 3.5),
    fill = c(rep("red", 10), rep("blue", 14)),
    stringsAsFactors = FALSE
  )
  expect_equal(layer_data(p)[,c("x", "fill")], ref)
})


# deprecated params -------------------------------------------------------

test_that("slab_type throws appropriate warnings and errors", {
  expect_warning(
    expect_warning(
      layer_data(ggplot() + stat_slab(aes(1:5), slab_type = "xx")),
      "slab_type.*is deprecated"
    ),
    'Unknown `slab_type`: "xx"'
  )
})
