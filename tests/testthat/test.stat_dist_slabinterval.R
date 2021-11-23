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
    print(df %>%
      ggplot(aes(x = var, y = dist)) +
      stat_dist_halfeye(n = 15)),
    "Cannot use distribution or rvar"
  )

  expect_error(
    print(df %>%
      ggplot(aes(x = dist, y = var)) +
      stat_dist_halfeye(n = 15)),
    "Cannot use distribution or rvar"
  )
})

test_that("stat fill aesthetic on halfeye works", {
  skip_if_no_vdiffr()


  vdiffr::expect_doppelganger("gradient fill/color halfeye",
    data.frame(dist = "norm", mean = 0, sd = 1) %>%
      ggplot(aes(y = 1, dist = dist, arg1 = mean, arg2 = sd, slab_color = stat(x > 0), fill = stat(f), slab_linetype = stat(x > -1), slab_size = stat(x > 1))) +
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

  # N.B. the following two tests are currently a bit useless as vdiffr doesn't
  # support linearGradient yet, but leaving them here so that once it does we
  # have tests for this.
  skip("linearGradient not supported in vdiffr yet")
  vdiffr::expect_doppelganger("fill_type = gradient with two groups",
    p + stat_dist_gradientinterval(aes(x = dist), n = 15, p_limits = c(0.01, 0.99), fill_type = "gradient")
  )
  vdiffr::expect_doppelganger("fill_type = gradient with two groups, h",
    p + stat_dist_gradientinterval(aes(y = dist), n = 15, p_limits = c(0.01, 0.99), fill_type = "gradient")
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


  p_rev = data.frame(dist = "lnorm") %>%
    ggplot(aes(y = 1, dist = dist, arg1 = 1, arg2 = 0.5)) +
    scale_x_reverse()

  vdiffr::expect_doppelganger("dist_halfeyeh reverse scale transform",
    p_rev + stat_dist_halfeye(n = 40)
  )

  vdiffr::expect_doppelganger("ccdfinterval reverse scale transform",
    p_rev + stat_dist_ccdfinterval(n = 40)
  )
})

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
    ggplot(aes(dist = dist, args = args, fill = dist, thickness = stat(pdf), slab_alpha = stat(cdf))) +
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
    print(
      tibble(y = c("a","b","c"), x = list(1,2,3)) %>%
        ggplot(aes(y = y, dist = x)) + stat_dist_slabinterval()
    ),
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
