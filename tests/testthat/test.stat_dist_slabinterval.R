# Tests for analytical distribution plots
#
# Author: mjskay
###############################################################################

library(dplyr)
library(distributional)



test_that("distribution eye plots work with the args aesthetic", {
  skip_if_no_vdiffr()


  p = tribble(
    ~dist, ~args,
    "norm", list(0, 1),
    "beta", list(5, 5),
    NA, NA
  ) %>%
    ggplot(aes(dist = dist, args = args))

  expect_warning(
    vdiffr::expect_doppelganger("vertical eye using args without na.rm",
      p + stat_dist_eye(aes(x = dist), n = 20)
    ),
    "Removed 2 rows containing missing values"
  )

  vdiffr::expect_doppelganger("vertical eye using args",
    p + stat_dist_eye(aes(x = dist), na.rm = TRUE, n = 20)
  )

  vdiffr::expect_doppelganger("horizontal eye using args",
    p + stat_dist_eye(aes(y = dist), na.rm = TRUE, n = 20)
  )

  vdiffr::expect_doppelganger("vertical half-eye using args",
    p + stat_dist_halfeye(aes(x = dist), na.rm = TRUE, n = 20)
  )

  vdiffr::expect_doppelganger("horizontal half-eye using args",
    p + stat_dist_halfeye(aes(y = dist), na.rm = TRUE, n = 20)
  )

  vdiffr::expect_doppelganger("ccdfinterval using args",
    p + stat_dist_ccdfinterval(aes(x = dist), na.rm = TRUE, n = 25)
  )

  vdiffr::expect_doppelganger("ccdfintervalh using args",
    p + stat_dist_ccdfinterval(aes(y = dist), na.rm = TRUE, n = 25)
  )

  vdiffr::expect_doppelganger("cdfinterval using args",
    p + stat_dist_cdfinterval(aes(x = dist), na.rm = TRUE, n = 25)
  )

  vdiffr::expect_doppelganger("cdfintervalh using args",
    p + stat_dist_cdfinterval(aes(y = dist), na.rm = TRUE, n = 25)
  )

})

test_that("args and arg1...n work with named args", {
  skip_if_no_vdiffr()


  vdiffr::expect_doppelganger("named args for dist", {
    tibble(args = list(list(mean = 1)), sd = 2) %>%
      ggplot(aes(xdist = "norm", args = args, arg1 = sd)) +
      stat_halfeye(n = 15)
  })

})

test_that("layer data is correct", {
  p = data.frame(dist = dist_normal(0, 1)) %>%
    ggplot(aes(xdist = dist)) +
    stat_halfeye(n = 5, p_limits = c(0.01, 0.99))

  x = seq(qnorm(0.01), qnorm(0.99), length.out = 5)
  ref =
    data.frame(
      size = c(NA_real_, 1, 6, 1, NA_real_),
      thickness = dnorm(x),
      f = dnorm(x),
      pdf = dnorm(x),
      cdf = pnorm(x),
      n = Inf,
      x = x,
      datatype = "slab",
      .width = c(NA_real_, 0.95, 0.66, 0.95, NA_real_),
      level = ordered(c(NA, 0.95, 0.66, 0.95, NA), levels = c(0.95, 0.66)),
      .point = NA_character_,
      .interval = NA_character_,
      xmin = NA_real_,
      xmax = NA_real_,
      stringsAsFactors = FALSE
    ) %>%
    rbind(data.frame(
      size = c(6, 1),
      thickness = NA_real_,
      f = NA_real_,
      pdf = NA_real_,
      cdf = NA_real_,
      n = NA_real_,
      x = 0,
      datatype = "interval",
      .width = c(0.66, 0.95),
      level = ordered(c(0.66, 0.95), levels = c(0.95, 0.66)),
      .point = "median",
      .interval = "qi",
      xmin = qnorm(c(0.17, 0.025)),
      xmax = qnorm(c(0.83, 0.975)),
      stringsAsFactors = FALSE
    )) %>%
    cbind(data.frame(
      y = 0,
      height = 1,
      ymin = 0,
      ymax = 1,
      side = "topright",
      scale = 0.9,
      stringsAsFactors = FALSE
    ))
  ref$xdist = rep(list(dist_normal(0, 1)), 7)

  expect_equal(layer_data(p)[, names(ref)], ref)
})

test_that("xdist and ydist aesthetics work", {
  skip_if_no_vdiffr()


  df = data.frame(var = c(1,2), dist = dist_normal(0:1,1))

  vdiffr::expect_doppelganger("ydist",
    df %>%
      ggplot(aes(x = var, ydist = dist)) +
      stat_dist_halfeye(n = 15) +
      scale_y_continuous(limits = c(-7, 7))
  )

  vdiffr::expect_doppelganger("xdist",
    df %>%
      ggplot(aes(xdist = dist, y = var)) +
      stat_dist_halfeye(n = 15) +
      scale_x_continuous(limits = c(-7, 7))
  )

})

test_that("mapping dist to x or y gives helpful error", {
  df = data.frame(var = c(1,2), dist = dist_normal(0:1,1))

  expect_error(
    ggplot_build(df %>%
      ggplot(aes(x = var, y = dist)) +
      stat_dist_halfeye(n = 15)),
    "Cannot use distribution or rvar"
  )

  expect_error(
    ggplot_build(df %>%
      ggplot(aes(x = dist, y = var)) +
      stat_dist_halfeye(n = 15)),
    "Cannot use distribution or rvar"
  )
})

test_that("stat fill aesthetic on halfeye works", {
  skip_if_no_vdiffr()


  vdiffr::expect_doppelganger("gradient fill/color halfeye",
    data.frame(dist = "norm", mean = 0, sd = 1) %>%
      ggplot(aes(y = 1, dist = dist, arg1 = mean, arg2 = sd, slab_color = after_stat(x > 0), fill = after_stat(f), slab_linetype = after_stat(x > -1), slab_linewidth = after_stat(x > 1))) +
      stat_dist_halfeye(n = 10)
  )
})

test_that("stat_dist_gradientinterval works", {
  skip_if_no_vdiffr()


  p = tribble(
    ~dist, ~args,
    "norm", list(0, 1),
    "t", list(3)
  ) %>%
    ggplot(aes(dist = dist, args = args, fill = dist)) +
    scale_slab_alpha_continuous(range = c(0,1))

  vdiffr::expect_doppelganger("dist_gradientinterval with two groups",
    p + stat_dist_gradientinterval(aes(x = dist), n = 15, p_limits = c(0.01, 0.99), fill_type = "segments")
  )
  vdiffr::expect_doppelganger("dist_gradientintervalh with two groups",
    p + stat_dist_gradientinterval(aes(y = dist), n = 15, p_limits = c(0.01, 0.99), fill_type = "segments")
  )
})

test_that("fill_type = 'gradient' works", {
  skip_if_no_vdiffr()
  skip_if_no_linearGradient()


  p = tribble(
    ~dist, ~args,
    "norm", list(0, 1),
    "t", list(3)
  ) %>%
    ggplot(aes(dist = dist, args = args, fill = dist)) +
    scale_slab_alpha_continuous(range = c(0,1))

  write_svg_with_gradient = function(plot, file, title = "") {
    svglite::svglite(file, width = 10, height = 8, bg = "white", pointsize = 12, standalone = TRUE, always_valid = FALSE)
    on.exit(grDevices::dev.off())
    vdiffr:::print_plot(plot, title)
  }

  vdiffr::expect_doppelganger("fill_type = gradient with two groups",
    p + stat_dist_gradientinterval(aes(x = dist), n = 15, p_limits = c(0.01, 0.99), fill_type = "gradient"),
    writer = write_svg_with_gradient
  )
  vdiffr::expect_doppelganger("fill_type = gradient with two groups, h",
    p + stat_dist_gradientinterval(aes(y = dist), n = 15, p_limits = c(0.01, 0.99), fill_type = "gradient"),
    writer = write_svg_with_gradient
  )
})

test_that("stat_dist_pointinterval, interval, and slab work", {
  skip_if_no_vdiffr()


  p = tribble(
    ~dist, ~args,
    "norm", list(0, 1),
    "t", list(3)
  ) %>%
    ggplot(aes(dist = dist, args = args)) +
    scale_color_brewer()

  vdiffr::expect_doppelganger("dist_pointinterval with two groups",
    p + stat_dist_pointinterval(aes(x = dist), n = 20)
  )
  vdiffr::expect_doppelganger("dist_pointintervalh with two groups",
    p + stat_dist_pointinterval(aes(y = dist), n = 20)
  )

  vdiffr::expect_doppelganger("dist_interval with two groups",
    p + stat_dist_interval(aes(x = dist), n = 20)
  )
  vdiffr::expect_doppelganger("dist_intervalh with two groups",
    p + stat_dist_interval(aes(y = dist), n = 20)
  )

  vdiffr::expect_doppelganger("dist_slab with two groups",
    p + stat_dist_slab(aes(x = dist), n = 15)
  )
  vdiffr::expect_doppelganger("dist_slabh with two groups",
    p + stat_dist_slab(aes(y = dist), n = 15)
  )
})



# scale (and density) transformation --------------------------------------

test_that("density transformation works", {
  expect_equal(transform_pdf(dnorm, 1:5, scales::exp_trans()), dlnorm(1:5))
  expect_equal(transform_pdf(dlnorm, -2:2, scales::log_trans()), dnorm(-2:2))
})

test_that("scale transformation works", {
  skip_if_no_vdiffr()


  # this setup should yield a 95% interval from a little above 1e-3 to a little below 1e+5
  p_log = data.frame(dist = "lnorm") %>%
    ggplot(aes(y = 1, dist = dist, arg1 = log(10), arg2 = 2*log(10))) +
    scale_x_log10(breaks = 10^seq(-5,7, by = 2))

  vdiffr::expect_doppelganger("dist_halfeyeh log scale transform",
    p_log + stat_dist_halfeye(n = 20)
  )

  vdiffr::expect_doppelganger("dist_ccdfintervalh log scale transform",
    p_log + stat_dist_ccdfinterval(n = 20)
  )


  p_log_dist = data.frame(x = dist_sample(list(qlnorm(ppoints(100))))) %>%
    ggplot(aes(xdist = x, y = 0))

  vdiffr::expect_doppelganger("transformed scale with dist_sample",
    p_log_dist + stat_dist_halfeye(n = 20, point_interval = mode_hdci) + scale_x_log10()
  )


  p_log_wrap = data.frame(x = dist_wrap("lnorm")) %>%
    ggplot(aes(xdist = x, y = 0))

  vdiffr::expect_doppelganger("transformed scale with dist_wrap(lnorm)",
    p_log_wrap + stat_dist_halfeye(n = 20, point_interval = mode_hdci) + scale_x_log10()
  )


  p_log_samp = data.frame(x = qlnorm(ppoints(100))) %>%
    ggplot(aes(x = x, y = 0))

  vdiffr::expect_doppelganger("transformed scale with sample data on x",
    p_log_samp + stat_dist_halfeye(n = 20, point_interval = mode_hdi) + scale_x_log10()
  )

  p_rev = data.frame(dist = "lnorm") %>%
    ggplot(aes(y = 1, dist = dist, arg1 = 1, arg2 = 0.5)) +
    scale_x_reverse()

  vdiffr::expect_doppelganger("dist_halfeyeh reverse scale transform",
    p_rev + stat_dist_halfeye(n = 40)
  )

  vdiffr::expect_doppelganger("ccdfinterval reverse scale transform",
    p_rev + stat_dist_ccdfinterval(n = 40)
  )


  p_logit = data.frame(dist = dist_beta(2,2)) %>%
    ggplot(aes(xdist = dist)) +
    scale_x_continuous(trans = scales::trans_new("logit", qlogis, plogis))

  vdiffr::expect_doppelganger("beta eye with logit scale",
    p_logit + stat_eye(n = 15, slab_color = "gray50")
  )

  vdiffr::expect_doppelganger("dist_halfeyeh log scale mode_hdi",
    p_log + stat_dist_halfeye(n = 20, point_interval = mode_hdi)
  )
})

test_that("scale transformation sets appropriate axis limits", {
  p = data.frame(x = dist_lognormal(10, 0.5)) %>%
    ggplot(aes(xdist = x)) +
    stat_halfeye()

  # without scale transformation, the lower limit of a log-normal is finite
  # and so should be 0
  limits = range(layer_data(p)$x)
  expect_equal(limits[[1]], 0)
  expect_equal(limits[[2]], qlnorm(0.999, 10, 0.5))

  # with scale transformation, the lower limit is no longer finite, so it
  # should be set to the 0.001 quantile ...
  limits = range(layer_data(p + scale_x_log10())$x)
  expect_equal(limits[[1]], log(qlnorm(0.001, 10, 0.5), base = 10))
  expect_equal(limits[[2]], log(qlnorm(0.999, 10, 0.5), base = 10))

  # ... but if other data is added, it should be extended to cover that point
  limits = range(layer_data(p + scale_x_log10() + geom_point(aes(x = 2, y = 0)))$x)
  expect_equal(limits[[1]], log(2, base = 10))
})


# orientation detection ---------------------------------------------------

test_that("orientation detection works properly on stat_dist", {
  skip_if_no_vdiffr()


  vdiffr::expect_doppelganger("stat_dist with no main axis",
    ggplot(data.frame(), aes(dist = "norm")) + stat_dist_slabinterval(n = 10)
  )

  vdiffr::expect_doppelganger("stat_dist with main axis of y",
    ggplot(data.frame(), aes(y = "a", dist = "norm")) + stat_dist_slabinterval(n = 10)
  )

  vdiffr::expect_doppelganger("stat_dist with main axis of x",
    ggplot(data.frame(), aes(x = "a", dist = "norm")) + stat_dist_slabinterval(n = 10)
  )

})

test_that("auto-grouping works on stat_dist", {
  skip_if_no_vdiffr()


  p = data.frame(
    dist = c("norm", "norm"),
    x = c(1,2)
  ) %>% ggplot(aes(dist = dist, arg1 = x, y = 0))

  vdiffr::expect_doppelganger("stat_dist with no grouping",
    p + stat_dist_slab(alpha = 0.5, n = 10)
  )

})

test_that("pdf and cdf aesthetics work", {
  skip_if_no_vdiffr()


  p = tribble(
    ~dist, ~args,
    "norm", list(0, 1),
    "t", list(3)
  ) %>%
    ggplot(aes(dist = dist, args = args, fill = dist, thickness = after_stat(pdf), slab_alpha = after_stat(cdf))) +
    scale_slab_alpha_continuous(range = c(0,1))

  vdiffr::expect_doppelganger("pdf and cdf on a slabinterval",
    p + stat_dist_slabinterval(aes(x = dist), n = 15, p_limits = c(0.01, 0.99))
  )
})

test_that("distributional objects work", {
  skip_if_no_vdiffr()


  p = tribble(
    ~name, ~dist,
    "norm", dist_normal(0, 1.5),
    "t", dist_student_t(3)
  ) %>%
    ggplot(aes(x = name, dist = dist))

  vdiffr::expect_doppelganger("dist objects in stat_dist_halfeye",
    p + stat_dist_halfeye(n = 15)
  )

  vdiffr::expect_doppelganger("dist objects in stat_dist_ccdfinterval",
    p + stat_dist_ccdfinterval(n = 15)
  )

  vdiffr::expect_doppelganger("dist_sample",
    tibble(
      x = dist_sample(list(qnorm(ppoints(100)), qnorm(ppoints(100), mean = 1)))
    ) %>%
      ggplot(aes(dist = x, y = "a")) +
      stat_dist_slab(fill = NA, color = "black", n = 15)
  )

})

test_that("stat_dist_ works on factor dist names", {
  skip_if_no_vdiffr()


  p = data.frame(
    x = factor(c("norm", "norm")),
    y = factor(c("a", "b"))
  ) %>%
    ggplot(aes(dist = x, y = y))

  vdiffr::expect_doppelganger("stat_dist_ with factor dist name",
    p + stat_dist_slabinterval(n = 15)
  )

})

test_that("automatic finite limits work", {
  skip_if_no_vdiffr()


  # this setup should yield a 95% interval from a little above 1e-3 to a little below 1e+5
  p = data.frame(dist = dist_beta(2,2)) %>%
    ggplot(aes(y = 0, dist = dist))

  vdiffr::expect_doppelganger("dist_slab beta(2,2)",
    p + stat_dist_slab(n = 31)
  )
})

test_that("justification can vary", {
  skip_if_no_vdiffr()

  p = tribble(
    ~id, ~name, ~dist,                ~just,
    1, "norm", dist_normal(0, 1.5),  1,
    2, "norm", dist_normal(0, 1),  0.5,
    3, "t",    dist_student_t(3),    0
  ) %>%
    ggplot(aes(x = id, dist = dist, justification = just))

  vdiffr::expect_doppelganger("ccdf with varying just",
    p + stat_dist_ccdfinterval(n = 15)
  )
})

test_that("NA distributional objects work", {
  skip_if_no_vdiffr()


  p = tribble(
    ~name, ~dist,
    "norm", dist_normal(0, 1.5),
    "missing", NULL
  ) %>%
    ggplot(aes(x = name, dist = dist))

  vdiffr::expect_doppelganger("NA dists in stat_dist_slabinterval",
    p + stat_dist_halfeye(n = 15, na.rm = TRUE)
  )

  vdiffr::expect_doppelganger("NA dists in stat_dist_dotsinterval",
    p + stat_dist_dotsinterval(n = 15, na.rm = TRUE)
  )

})

test_that("stat_dist_ throws appropriate errors on ill-formed dists", {
  expect_warning(
    invisible(ggplot_build(
      tibble(y = c("a","b","c"), x = list(1,2,3)) %>%
        ggplot(aes(y = y, dist = x)) + stat_dist_slabinterval()
    ))
    ,
    'The `dist` aesthetic does not support objects of type "numeric"'
  )

  expect_error(
    distr_cdf(dist_normal(c(0,1))),
    "distributional objects should never have length > 1 here"
  )
})


# discrete distributions --------------------------------------------------

test_that("stat_dist_ detects discrete distributions", {
  skip_if_no_vdiffr()


  p = tibble(lambda = c(13,7,2)) %>%
    ggplot(aes(x = lambda))

  vdiffr::expect_doppelganger("dist_poisson", {
    p + stat_dist_halfeye(aes(dist = dist_poisson(lambda)), slab_color = "gray50")
  })

  vdiffr::expect_doppelganger("dist_poisson ccdf", {
    p + stat_dist_ccdfinterval(aes(dist = dist_poisson(lambda)), slab_color = "gray50")
  })

  vdiffr::expect_doppelganger("dpois", {
    p + stat_dist_halfeye(aes(dist = "pois", arg1 = lambda), slab_color = "gray50", outline_bars = TRUE)
  })

  vdiffr::expect_doppelganger("dpois ccdf", {
    p + stat_dist_ccdfinterval(aes(dist = "pois", arg1 = lambda), slab_color = "gray50", outline_bars = TRUE)
  })

})

test_that("rvar_factor works", {
  skip_if_not_installed("posterior")

  p = ggplot_build(
    ggplot() +
      stat_slabinterval(aes(xdist = posterior::rvar(c("a","a","a","b","b","c"))))
  )

  slab_ref = data.frame(
    thickness = c(3,3,3,3, 2,2,2,2, 1,1,1,1)/6,
    pdf = c(3,3,3,3, 2,2,2,2, 1,1,1,1)/6,
    cdf = NA_real_,
    f = c(3,3,3,3, 2,2,2,2, 1,1,1,1)/6,
    n = 6,
    datatype = "slab",
    .width = NA_real_
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(.5, 1,1, 1.5,1.5, 2,2, 2.5,2.5, 3,3, 3.5))
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "slab", names(slab_ref)], slab_ref)

  interval_ref = data.frame(
    datatype = "interval",
    .width = c(0.66, 0.95)
  )
  interval_ref$xmin = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  interval_ref$xmax = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  interval_ref$x = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  attr(interval_ref, "row.names") = c(13L, 14L)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "interval", names(interval_ref)], interval_ref)

  x_scale = p$plot$scales$get_scales("x")
  expect_true(x_scale$is_discrete())
  expect_equal(x_scale$get_limits(), c("a","b","c"))
})

test_that("rvar_ordered works", {
  skip_if_not_installed("posterior")

  p = ggplot_build(
    ggplot() +
      stat_slabinterval(aes(xdist = posterior::rvar_ordered(c("a","a","a","b","b","c"))))
  )

  slab_ref = data.frame(
    thickness = c(3,3,3,3, 2,2,2,2, 1,1,1,1)/6,
    pdf = c(3,3,3,3, 2,2,2,2, 1,1,1,1)/6,
    cdf = c(0,0, 3,3,3,3, 5,5,5,5, 6,6)/6,
    f = c(3,3,3,3, 2,2,2,2, 1,1,1,1)/6,
    n = 6,
    datatype = "slab",
    .width = c(NA, .66,.66,.66,.66,.66,.66, .95,.95, NA,NA,NA)
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(.5, 1,1, 1.5,1.5, 2,2, 2.5,2.5, 3,3, 3.5))
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "slab", names(slab_ref)], slab_ref)

  interval_ref = data.frame(
    datatype = "interval",
    .width = c(0.66, 0.95)
  )
  interval_ref$xmin = ggplot2:::mapped_discrete(c(1, 1))
  interval_ref$xmax = ggplot2:::mapped_discrete(c(2.15, 2.875))
  interval_ref$x = ggplot2:::mapped_discrete(c(1.5, 1.5))
  attr(interval_ref, "row.names") = c(13L, 14L)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "interval", names(interval_ref)], interval_ref)

  x_scale = p$plot$scales$get_scales("x")
  expect_true(x_scale$is_discrete())
  expect_equal(x_scale$get_limits(), c("a","b","c"))
})

test_that("rvar_ordered works with modified scale limits", {
  skip_if_not_installed("posterior")

  p = ggplot_build(
    ggplot() +
      stat_slab(aes(xdist = posterior::rvar_ordered(c("a","a","a","c")))) +
      scale_x_discrete(limits = c("a","b","c"))
  )

  slab_ref = data.frame(
    thickness = c(3,3,3,3, 0,0,0,0, 1,1,1,1)/4,
    pdf = c(3,3,3,3, 0,0,0,0, 1,1,1,1)/4,
    cdf = c(0,0, 3,3,3,3, 3,3,3,3, 4,4)/4,
    f = c(3,3,3,3, 0,0,0,0, 1,1,1,1)/4,
    n = 4,
    datatype = "slab",
    .width = c(NA, .66,.66,.66,.66, .95,.95,.95,.95, NA,NA,NA)
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(.5, 1,1, 1.5,1.5, 2,2, 2.5,2.5, 3,3, 3.5))
  expect_equal(p$data[[1]][, names(slab_ref)], slab_ref)
})

test_that("dist_bernoulli works", {
  p = ggplot_build(
    ggplot() +
      stat_slabinterval(aes(xdist = dist_bernoulli(0.8)))
  )

  slab_ref = data.frame(
    thickness = c(0.2, 0.2, 0.2, 0.2, 0.8, 0.8, 0.8, 0.8),
    pdf = c(0.2, 0.2, 0.2, 0.2, 0.8, 0.8, 0.8, 0.8),
    cdf = c(0, 0, 0.2, 0.2, 0.2, 0.2, 1, 1),
    f = c(0.2, 0.2, 0.2, 0.2, 0.8, 0.8, 0.8, 0.8),
    n = Inf,
    datatype = "slab",
    .width = c(NA, 0.66, 0.66, 0.66, 0.66, 0.66, 0.66, NA),
    x = c(-0.5, 0, 0, 0.5, 0.5, 1, 1, 1.5)
  )
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "slab", names(slab_ref)], slab_ref)

  interval_ref = data.frame(
    datatype = "interval",
    .width = c(0.66, 0.95),
    xmin = c(0, 0),
    xmax = c(1, 1),
    x = c(1, 1)
  )
  attr(interval_ref, "row.names") = c(9L, 10L)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "interval", names(interval_ref)], interval_ref)

  x_scale = p$plot$scales$get_scales("x")
  expect_true(!x_scale$is_discrete())
  expect_equal(x_scale$get_limits(), c(0, 1))
})

test_that("dist_categorical works", {
  skip_if_not_installed("posterior")

  p = ggplot_build(
    ggplot() +
      stat_slabinterval(aes(xdist = dist_categorical(list(3:1/6), list(c("a","b","c")))))
  )

  slab_ref = data.frame(
    thickness = c(3,3,3,3, 2,2,2,2, 1,1,1,1)/6,
    pdf = c(3,3,3,3, 2,2,2,2, 1,1,1,1)/6,
    cdf = NA_real_,
    f = c(3,3,3,3, 2,2,2,2, 1,1,1,1)/6,
    n = Inf,
    datatype = "slab",
    .width = NA_real_
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(.5, 1,1, 1.5,1.5, 2,2, 2.5,2.5, 3,3, 3.5))
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "slab", names(slab_ref)], slab_ref)

  interval_ref = data.frame(
    datatype = "interval",
    .width = c(0.66, 0.95)
  )
  interval_ref$xmin = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  interval_ref$xmax = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  interval_ref$x = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  attr(interval_ref, "row.names") = c(13L, 14L)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "interval", names(interval_ref)], interval_ref)

  x_scale = p$plot$scales$get_scales("x")
  expect_true(x_scale$is_discrete())
  expect_equal(x_scale$get_limits(), c("a","b","c"))
})

test_that("dist_categorical works with modified scale limits", {
  skip_if_not_installed("posterior")

  p = ggplot_build(
    ggplot() +
      stat_dots(aes(xdist = dist_categorical(list(c(3,1)/4), list(c("a","c"))))) +
      scale_x_discrete(limits = c("a","b","c"))
  )

  slab_ref = data.frame(
    thickness = c(3,3,3,3, NA,NA,NA,NA, 1,1,1,1)/4,
    pdf = c(3,3,3,3, NA,NA,NA,NA, 1,1,1,1)/4,
    cdf = NA_real_,
    f = c(3,3,3,3, NA,NA,NA,NA, 1,1,1,1)/4,
    n = Inf,
    datatype = "slab",
    .width = NA_real_
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(.5, 1,1, 1.5,1.5, 2,2, 2.5,2.5, 3,3, 3.5))
  expect_equal(p$data[[1]][, names(slab_ref)], slab_ref)
})


# grouping order ----------------------------------------------------------

test_that("stat_dist_ preserves existing grouping order", {
  skip_if_no_vdiffr()


  df = tribble(
    ~Model, ~Parameter, ~Coefficient,  ~SE, ~linetype,
    "C",       "MZ",         0.34, 0.07,    "dashed",
    "C",     "Ereg",         0.28, 0.06,    "twodash",
    "C",     "AE-Beta",      0.25, 0.06,    "solid",
    "D",       "MZ",         0.31, 0.08,    "dashed"
  )

  # the labels should overlap the points exactly if grouping order is preserved
  vdiffr::expect_doppelganger("grouped labels with pointintervals",
    df %>%
      ggplot() +
      aes(x = Model, y = Coefficient,
        label = Parameter, color = Parameter,
        group = linetype,
        dist = dist_normal(mu = Coefficient, sigma = SE)
      ) +
      stat_dist_pointinterval(position = "dodge") +
      geom_label(position = position_dodge(width = 1))
  )

})


# constant distributions --------------------------------------------------

test_that("constant distributions work", {
  skip_if_no_vdiffr()


  p = data.frame(
    x = c("constant = 1", "normal(2,1)", "constant = 2"),
    y = c(dist_normal(1:2, 0:1), dist_sample(list(2)))
  ) %>%
    ggplot(aes(x = x, dist = y))

  vdiffr::expect_doppelganger("constant dist on halfeye",
    p + stat_dist_slabinterval(n = 15, slab_color = "blue")
  )

  vdiffr::expect_doppelganger("constant dist on halfeye expanded",
    p + stat_dist_slabinterval(n = 15, slab_color = "blue", expand = TRUE)
  )

  vdiffr::expect_doppelganger("constant dist on ccdf",
    p + stat_dist_ccdfinterval(slab_color = "blue", n = 15)
  )

  # with a scale transformation...
  p = data.frame(
    x = c("constant = 10", "lognormal(2,1)", "constant = 2"),
    y = c(dist_wrap("lnorm", c(log(10), 2), 0:1), dist_sample(list(2)))
  ) %>%
    ggplot(aes(x = x, dist = y)) +
    scale_y_log10()

  vdiffr::expect_doppelganger("constant dist on halfeye, log scale",
    p + stat_dist_slabinterval(n = 15, slab_color = "blue")
  )

  vdiffr::expect_doppelganger("constant dist on ccdf, log scale",
    p + stat_dist_ccdfinterval(slab_color = "blue", n = 15)
  )

  # with sample data...
  p = data.frame(
    x = c(5, 5)
  ) %>%
    ggplot(aes(x = x)) +
    expand_limits(x = c(0,10))

  vdiffr::expect_doppelganger("constant dist on halfeye, sample data",
    p + stat_dist_slabinterval(n = 15, slab_color = "blue")
  )

  vdiffr::expect_doppelganger("constant dist on ccdf, sample data",
    p + stat_dist_ccdfinterval(n = 15, slab_color = "blue")
  )

})


# point_interval ----------------------------------------------------------

test_that("point_interval works", {
  skip_if_no_vdiffr()


  p = data.frame(
    x = dist_mixture(dist_normal(0, 0.5), dist_normal(4, 1), weights = c(0.5, 0.5))
  ) %>%
    ggplot(aes(xdist = x))

  vdiffr::expect_doppelganger("mixture dist with median_qi",
    p + stat_dist_halfeye(point_interval = median_qi, n = 30)
  )

  vdiffr::expect_doppelganger("mixture dist with NULL point_interval",
    p + stat_dist_halfeye(point_interval = NULL, n = 30)
  )

  # need to set.seed here until https://github.com/mitchelloharawild/distributional/issues/71 is fixed
  set.seed(1234)
  vdiffr::expect_doppelganger("mixture dist with mean_qi",
    p + stat_dist_halfeye(point_interval = mean_qi, n = 30)
  )

  vdiffr::expect_doppelganger("mixture dist with mode_hdi",
    p + stat_dist_halfeye(point_interval = mode_hdi, n = 30)
  )

})


# rvars -------------------------------------------------------------------

test_that("rvars work", {
  skip_if_no_vdiffr()
  skip_if_not_installed("posterior")


  set.seed(1234)
  p = tibble(
      mu = 1:2,
      x = posterior::rvar_rng(rnorm, 2, mu, 1:2)
    ) %>%
    ggplot(aes(y = mu, xdist = x, fill = after_stat(cdf)))

  vdiffr::expect_doppelganger("halfeye with rvar and cdf",
    p + stat_halfeye(n = 20, trim = FALSE, expand = TRUE, slab_color = "black")
  )
})


# missing rvars and dists -------------------------------------------------

test_that("missing distributions work", {
  expect_warning(
    expect_equal(
      layer_data(ggplot() + stat_slabinterval(aes(xdist = dist_missing()))),
      data.frame()
    ),
    "Removed 1 rows containing missing values"
  )
})

test_that("missing rvars work", {
  skip_if_not_installed("posterior")

  expect_warning(
    expect_equal(
      layer_data(ggplot() + stat_slabinterval(aes(xdist = posterior::rvar(c(1,NA))))),
      data.frame()
    ),
    "Removed 1 rows containing missing values"
  )

  expect_warning(
    expect_equal(
      layer_data(
        ggplot() +
          stat_slabinterval(aes(xdist = posterior::rvar(c("a","b")))) +
          scale_x_discrete(limits = "a")
        ),
      data.frame()
    ),
    "Removed 1 rows containing missing values"
  )
})


# without attaching ggdist namespace --------------------------------------

test_that("stats work without attaching the ggdist namespace", {
  skip_if_no_vdiffr()

  detach("package:ggdist")
  on.exit(require(ggdist))

  vdiffr::expect_doppelganger("simple halfeye",
    data.frame(x = dist_normal(0,1)) %>%
      ggplot(aes(xdist = x)) +
      ggdist::stat_halfeye()
  )
})


# multiple dists with unique groups ---------------------------------------

test_that("multiple dists supplied to the same group", {
  p = data.frame(
      y = dist_normal(c(0, 10, 20, 0, 10, 20)),
      x = c(0,0,0,1,1,1)
    ) %>%
    ggplot(aes(ydist = y, x = x, group = rep(c("a","a","b"), 2)))


  # multiple dists can be supplied to the same group with slabinterval,
  # since they can be distinguished ...
  vdiffr::expect_doppelganger("halfeye multiple dists per group",
    p + stat_halfeye()
  )

  # but not to lineribbon, since they can't be distinguished
  expect_warning(ggplot_build(p + stat_lineribbon()),
    "Distributions passed to the `dist` aesthetic must be uniquely associated"
  )
})
