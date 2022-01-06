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

  write_svg_with_gradient = function(plot, file, title = "") {
    svglite::svglite(file, width = 10, height = 8, bg = "white", pointsize = 12, standalone = TRUE, always_valid = FALSE)
    on.exit(grDevices::dev.off())
    vdiffr:::print_plot(plot, title)
  }

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



  p_log = data.frame(x = qlnorm(ppoints(200), log(10), 2 * log(10))) %>%
    ggplot(aes(y = "a", x = x)) +
    scale_x_log10(breaks = 10^seq(-5,7, by = 2))

  vdiffr::expect_doppelganger("halfeyeh log scale transform",
    p_log + stat_halfeye(point_interval = mode_hdci, n = 20)
  )

  vdiffr::expect_doppelganger("ccdfintervalh log scale transform",
    p_log + stat_ccdfinterval(point_interval = mean_hdi, n = 20)
  )

  vdiffr::expect_doppelganger("cdfintervalh log scale transform",
    p_log + stat_cdfinterval(point_interval = mean_hdi, n = 20)
  )

  vdiffr::expect_doppelganger("histintervalh log scale transform",
    p_log + stat_histinterval(point_interval = median_qi, n = 20)
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
    p + stat_sample_slabinterval(aes(fill = x, thickness = stat(pdf), slab_alpha = stat(cdf)), n = 15)
  )

  expect_error(weighted_ecdf(NULL), "Need at least 1 or more values")
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
    p + stat_ccdfinterval()
  )

  # constant dist when n = 1
  p = data.frame(
    x = c("constant = 1", rep("normal(2,1)", 50)),
    # sd of 0 will generate constant dist
    y = qnorm(c(0.5, ppoints(50)), mean = c(1, rep(2, 50)), sd = c(0, rep(1, 50)))
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
      ggplot(aes(x = x, y = g, thickness = stat(pdf*n), fill = stat(n))) +
      stat_sample_slabinterval(n = 15)
  )
})


# missing data is handled correctly ---------------------------------------

test_that("NAs are handled correctly", {
  skip_if_no_vdiffr()

  p = data.frame(x = c(1:10, NA)) %>%
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
