# Tests for dots geoms and stats
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)
library(distributional)



test_that("vanilla dots geoms and stats work", {
  skip_if_no_vdiffr()


  set.seed(1234)
  p = tribble(
    ~dist,  ~x,
    "norm", rnorm(20),
    "t",    rt(20, 3)
  ) %>%
    unnest(x) %>%
    ggplot()

  vdiffr::expect_doppelganger("vanilla geom_dots",
    p + geom_dots(aes(x = dist, y = x))
  )

  vdiffr::expect_doppelganger("vanilla geom_dotsh",
    p + geom_dots(aes(y = dist, x = x))
  )

  vdiffr::expect_doppelganger("stat_dotsh with a group with 1 dot",
    p + stat_dots(aes(y = dist, x = x, color = x > 2))
  )

  vdiffr::expect_doppelganger("stat_dotsh with a group with 2 dots",
    p + stat_dots(aes(y = dist, x = x, color = x > 1))
  )

  set.seed(1234)
  p = tribble(
    ~dist,  ~x, ~datatype,
    "norm", rnorm(20), "slab",
    "t",    rt(20, 3), "slab"
  ) %>%
    unnest(x) %>%
    bind_rows(tribble(
      ~ dist,  ~x, ~datatype, ~lower, ~upper,
      "norm", 0, "interval", -1, 1,
      "t", 0, "interval", -2, 2
    )) %>%
    ggplot()

  vdiffr::expect_doppelganger("vanilla geom_dotsinterval",
    p + geom_dotsinterval(aes(y = dist, x = x, xmin = lower, xmax = upper, datatype = datatype))
  )

  set.seed(1234)
  p = tribble(
    ~dist,  ~x,
    "norm", rnorm(100),
    "t",    rt(100, 3)
  ) %>%
    unnest(x) %>%
    ggplot()

  vdiffr::expect_doppelganger("vanilla stat_dotsinterval",
    p + stat_dotsinterval(aes(x = dist, y = x), quantiles = 20)
  )

  vdiffr::expect_doppelganger("vanilla stat_dotsintervalh",
    p + stat_dotsinterval(aes(y = dist, x = x), quantiles = 20)
  )

})


# scale_ and coord_ transformations ----------------------------------------------

test_that("coordinate transformations work", {
  skip_if_no_vdiffr()


  set.seed(1234)
  p = tribble(
    ~dist,  ~x, ~datatype,
    "norm", rnorm(20), "slab",
    "t",    rt(20, 3), "slab"
  ) %>%
    unnest(x) %>%
    bind_rows(tribble(
      ~ dist,  ~x, ~datatype, ~lower, ~upper,
      "norm", 0, "interval", -1, 1,
      "t", 0, "interval", -2, 2
    )) %>%
    ggplot() +
    geom_dotsinterval(aes(y = dist, x = x, xmin = lower, xmax = upper, datatype = datatype))

  vdiffr::expect_doppelganger("coord_flip with dotsinterval",
    p + coord_flip()
  )

  expect_error(
    print(p + coord_polar(), newpage = FALSE),
    "geom_dotsinterval does not work properly with non-linear coordinates"
  )

})


test_that("scale transformations work", {
  skip_if_no_vdiffr()


  p = data.frame(x = dist_sample(list(qlnorm(ppoints(20))))) %>%
    ggplot(aes(xdist = x, y = 0))

  vdiffr::expect_doppelganger("transformed scale with dist_sample",
    p + stat_dist_dotsinterval() + scale_x_log10()
  )

  p = data.frame(x = qlnorm(ppoints(20))) %>%
    ggplot(aes(x = x, y = 0))

  vdiffr::expect_doppelganger("transformed scale with sample data on x",
    p + stat_dist_dotsinterval() + scale_x_log10()
  )

  p = data.frame(x = qlnorm(ppoints(100))) %>%
    ggplot(aes(x = x, y = 0))

  vdiffr::expect_doppelganger("transformed scale, sample data, quantiles",
    p + stat_dist_dotsinterval(quantiles = 20) + scale_x_log10()
  )

})


# dists -------------------------------------------------------------------

test_that("stat_dist_dots[interval] works", {
  skip_if_no_vdiffr()


  p = tribble(
    ~dist,  ~args,
    "norm", list(0, 1),
    "t",    list(3)
  ) %>%
    ggplot(aes(dist = dist, args = args))

  vdiffr::expect_doppelganger("vanilla stat_dist_dots",
    p + stat_dist_dots(aes(x = dist), n = 20, quantiles = 20)
  )

  vdiffr::expect_doppelganger("vanilla stat_dist_dotsinterval",
    p + stat_dist_dotsinterval(aes(x = dist), n = 20, quantiles = 20)
  )

  vdiffr::expect_doppelganger("vanilla stat_dist_dotsintervalh",
    p + stat_dist_dotsinterval(aes(y = dist), n = 20, quantiles = 20)
  )

})

test_that("stat_dist_dots works on NA data", {
  skip_if_no_vdiffr()


  p = data.frame(
    x = c("norm", NA, "norm"),
    y = c("a","b", NA)
  ) %>%
    ggplot(aes(dist = x, y = y))

  expect_warning(vdiffr::expect_doppelganger("stat_dist_dots with na.rm = FALSE",
    p + stat_dist_dots(na.rm = FALSE, quantiles = 20)
  ), "Removed 1 rows containing")

  vdiffr::expect_doppelganger("stat_dist_dots with na.rm = TRUE",
    p + stat_dist_dots(na.rm = TRUE, quantiles = 20)
  )

})

test_that("stat_dist_dots works on distributional objects", {
  skip_if_no_vdiffr()


  p = data.frame(
    x = dist_normal(0:1, 1:2),
    y = c("a","b")
  ) %>%
    ggplot(aes(dist = x, y = y))

  vdiffr::expect_doppelganger("stat_dist_dots with dist_normal",
    p + stat_dist_dots(quantiles = 20)
  )

})


# binwidth -----------------------------------------------------

test_that("geom_dots binwidth can be specified in unit()s", {
  skip_if_no_vdiffr()


  # these dots should be the same size (10% of facet height)
  vdiffr::expect_doppelganger("geom_dots with unit() binwidth",
    mtcars %>%
      ggplot(aes(y = mpg)) +
      geom_dots(binwidth = unit(0.1, "native")) +
      facet_grid(~ am, scales = "free")
  )
})

test_that("geom_dots allows constraints on binwidth", {
  skip_if_no_vdiffr()

  p = data.frame(x = seq(0, 2, length.out = 20)) %>%
    ggplot(aes(x = x, y = 0L)) +
    coord_cartesian(expand = FALSE)

  # max width of 1/40th of the viewport should approx space
  # this data with about 1 dot of space in between each dot
  vdiffr::expect_doppelganger("max binwidth",
    p + geom_dots(binwidth = unit(c(0, 1/40), "npc"))
  )

  # min width of 1/4th of the viewport should give us four giant bins
  # also test that verbose = TRUE outputs the binwidth
  expect_message(
    vdiffr::expect_doppelganger("min binwidth",
      p + geom_dots(binwidth = unit(c(1/4, Inf), "npc"), verbose = TRUE)
    ),
    'binwidth = 0\\.5 data units.*unit\\(0\\.25, "npc"\\)'
  )

})


# layout ------------------------------------------------------------------

test_that("dotplot layouts work", {
  skip_if_no_vdiffr()

  df = rbind(
    cbind(mtcars, side = "top", stringsAsFactors = FALSE),
    cbind(mtcars, side = "both", stringsAsFactors = FALSE),
    cbind(mtcars, side = "bottom", stringsAsFactors = FALSE),
    stringsAsFactors = FALSE
  )

  vdiffr::expect_doppelganger("weave",
    df %>%
      ggplot(aes(x = mpg)) +
      geom_dots(aes(side = side), layout = "weave") +
      facet_grid(~ side)
  )

  vdiffr::expect_doppelganger("swarm",
    df %>%
      ggplot(aes(x = mpg)) +
      geom_dots(aes(side = side), layout = "swarm") +
      facet_grid(~ side)
  )

  vdiffr::expect_doppelganger("hex",
    df %>%
      ggplot(aes(x = mpg)) +
      geom_dots(aes(side = side), layout = "hex", stackratio = 0.92) +
      facet_grid(~ side)
  )

  vdiffr::expect_doppelganger("swarm vertical",
    mtcars %>%
      ggplot(aes(y = mpg)) +
      geom_dots(layout = "swarm")
  )

})

test_that("dot order is correct", {
  skip_if_no_vdiffr()

  p = data.frame(x = qnorm(ppoints(50)), g = c("a", "b")) %>%
    ggplot(aes(x = x, fill = after_stat(x < 0), color = g, group = NA)) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Paired")

  vdiffr::expect_doppelganger("bin dot order",
    p +
      geom_dots(layout = "bin", size = 5) +
      geom_vline(xintercept = 0)
  )

  vdiffr::expect_doppelganger("bin dot order, kept",
    p +
      geom_dots(aes(order = g), layout = "bin", size = 5) +
      geom_vline(xintercept = 0)
  )

  vdiffr::expect_doppelganger("weave dot order",
    p +
      geom_dots(layout = "weave", size = 5) +
      geom_vline(xintercept = 0)
  )

  vdiffr::expect_doppelganger("swarm dot order",
    p +
      geom_dots(layout = "swarm", size = 5) +
      geom_vline(xintercept = 0)
  )

})

test_that("overflow = compress works", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("overflow = compress",
    ggplot(mtcars) + geom_dots(aes(x = mpg), binwidth = 4, overflow = "compress", alpha = 0.5)
  )
})


# NAs -------------------------------------------------------------------

test_that("na.rm is propagated to quantile dotplot", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("na.rm with quantile arg",
    data.frame(x = qnorm(ppoints(100), 1)) %>%
      ggplot(aes(x, y = 0)) +
      stat_dots(na.rm = TRUE, quantiles = 20) +
      scale_x_continuous(limits = c(0,4))
  )
})

test_that("geom_dots works with NA in non-data axis", {
  skip_if_no_vdiffr()

  p = mtcars %>%
    ggplot(aes(x = mpg, y = factor(cyl))) +
    scale_y_discrete(limits = c("4", "6"))

  # without na.rm this should work but also throw a warning
  expect_warning(vdiffr::expect_doppelganger("NA on y axis",
    p + geom_dots(na.rm = FALSE)
  ))

  # with na.rm this should not throw a warning
  vdiffr::expect_doppelganger("removed NA on y axis",
    p + geom_dots(na.rm = TRUE)
  )
})

test_that("empty slab from NA removal works", {
  skip_if_no_vdiffr()


  vdiffr::expect_doppelganger("dots with no slab from NA removal", {
    data.frame(x = c(1, NA), datatype = c("interval", "slab")) %>%
      ggplot(aes(x = x, xmin = x - 1, xmax = x + 1, datatype = datatype)) +
      geom_dotsinterval(na.rm = TRUE)
  })
})


# discrete distributions (raw) ---------------------------------------------

test_that("geom_dots works on discrete distributions", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("one integer bin",
    data.frame(x = rep(1L, 10)) %>%
      ggplot(aes(x = x, y = 0)) +
      stat_dots(orientation = "horizontal") +
      geom_hline(yintercept = 0.9)
  )

  vdiffr::expect_doppelganger("three integer bins",
    data.frame(x = c(rep(1L, 10), rep(2L, 12), rep(3L, 5))) %>%
      ggplot(aes(x = x, y = 0)) +
      stat_dots(orientation = "horizontal") +
      geom_hline(yintercept = 0.9)
  )

  vdiffr::expect_doppelganger("one character bin",
    data.frame(x = rep("a", 10)) %>%
      ggplot(aes(x = x, y = 0)) +
      stat_dots(orientation = "horizontal") +
      geom_hline(yintercept = 0.9)
  )

  vdiffr::expect_doppelganger("three character bins",
    data.frame(x = c(rep("a", 10), rep("b", 12), rep("c", 5))) %>%
      ggplot(aes(x = x, y = 0)) +
      stat_dots(orientation = "horizontal") +
      geom_hline(yintercept = 0.9)
  )

})

# discrete dist/rvar --------------------------------------------------

test_that("rvar_factor works", {
  skip_if_not_installed("posterior", "1.3.1.9000")

  p = ggplot_build(
    ggplot() +
      stat_dotsinterval(aes(xdist = posterior::rvar(c("a","a","a","b","b","c"))))
  )

  slab_ref = data.frame(
    thickness = rep(1, 6),
    n = 6,
    datatype = "slab",
    .width = NA_real_,
    stringsAsFactors = FALSE
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(1, 1, 1, 2, 2, 3))
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "slab", names(slab_ref)], slab_ref)

  interval_ref = data.frame(
    datatype = "interval",
    .width = c(0.66, 0.95),
    stringsAsFactors = FALSE
  )
  interval_ref$xmin = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  interval_ref$xmax = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  interval_ref$x = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  attr(interval_ref, "row.names") = c(7L, 8L)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "interval", names(interval_ref)], interval_ref)

  x_scale = p$plot$scales$get_scales("x")
  expect_true(x_scale$is_discrete())
  expect_equal(x_scale$get_limits(), c("a","b","c"))


  # quantiles works
  p = ggplot_build(
    ggplot() +
      stat_dotsinterval(aes(xdist = posterior::rvar(c("a","a","a","b","b","c"))), quantiles = 12)
  )
  expect_equal(p$data[[1]]$x, ggplot2:::mapped_discrete(c(1,1,1,1,1,1,2,2,2,2,3,3,NA,NA)))
})

test_that("rvar_ordered works and integer dist_sample works", {
  skip_if_not_installed("posterior", "1.3.1.9000")

  p = ggplot_build(
    ggplot() +
      stat_dotsinterval(aes(xdist = posterior::rvar_ordered(c("a","a","a","b","b","c"))))
  )

  slab_ref = data.frame(
    thickness = 1,
    n = 6,
    datatype = "slab",
    .width = c(.66, .66, .66, .66, .66, NA),
    stringsAsFactors = FALSE
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(1, 1, 1, 2, 2, 3))
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "slab", names(slab_ref)], slab_ref)

  interval_ref = data.frame(
    datatype = "interval",
    .width = c(0.66, 0.95),
    stringsAsFactors = FALSE
  )
  interval_ref$xmin = ggplot2:::mapped_discrete(c(1, 1))
  interval_ref$xmax = ggplot2:::mapped_discrete(c(2.15, 2.875))
  interval_ref$x = ggplot2:::mapped_discrete(c(1.5, 1.5))
  attr(interval_ref, "row.names") = c(7L, 8L)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "interval", names(interval_ref)], interval_ref)

  x_scale = p$plot$scales$get_scales("x")
  expect_true(x_scale$is_discrete())
  expect_equal(x_scale$get_limits(), c("a","b","c"))


  # integer dist_sample
  p = ggplot_build(
    ggplot() +
      stat_dotsinterval(aes(xdist = dist_sample(list(c(1L,1L,1L,2L,2L,3L)))))
  )
  slab_ref$x = as.numeric(slab_ref$x)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "slab", names(slab_ref)], slab_ref)
  interval_ref$x = as.numeric(interval_ref$x)
  interval_ref$xmin = as.numeric(interval_ref$xmin)
  interval_ref$xmax = as.numeric(interval_ref$xmax)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "interval", names(interval_ref)], interval_ref)


  # quantiles works
  p = ggplot_build(
    ggplot() +
      stat_dotsinterval(aes(xdist = posterior::rvar_ordered(c("a","a","a","b","b","c"))), quantiles = 12)
  )
  expect_equal(p$data[[1]]$x, ggplot2:::mapped_discrete(c(1,1,1,1,1,1,2,2,2,2,3,3, 1.5,1.5)))

  p = ggplot_build(
    ggplot() +
      stat_dotsinterval(aes(xdist = dist_sample(list(c(1L,1L,1L,2L,2L,3L)))), quantiles = 12)
  )
  expect_equal(p$data[[1]]$x, c(1,1,1,1,1,1,2,2,2,2,3,3, 1.5,1.5))


  # raw ordered
  p = ggplot_build(
    ggplot() +
      stat_dotsinterval(aes(x = ordered(c("a","a","a","b","b","c")), group = NA), quantiles = 12)
  )
  expect_equal(p$data[[1]]$x, ggplot2:::mapped_discrete(c(1,1,1,1,1,1,2,2,2,2,3,3, 1.5,1.5)))
})

test_that("rvar_ordered works with modified scale limits", {
  skip_if_not_installed("posterior", "1.3.1.9000")

  p = ggplot_build(
    ggplot() +
      stat_dots(aes(xdist = posterior::rvar_ordered(c("a","a","a","c")))) +
      scale_x_discrete(limits = c("a","b","c"))
  )

  slab_ref = data.frame(
    thickness = 1,
    n = 4,
    datatype = "slab",
    .width = c(.66,.66,.66, NA),
    stringsAsFactors = FALSE
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(1, 1, 1, 3))
  expect_equal(p$data[[1]][, names(slab_ref)], slab_ref)
})

test_that("dist_bernoulli works", {
  p = ggplot_build(
    ggplot() +
      stat_dotsinterval(aes(xdist = dist_bernoulli(0.8)), quantiles = 5)
  )

  slab_ref = data.frame(
    thickness = 1,
    n = 5,
    datatype = "slab",
    .width = .66,
    x = c(0, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "slab", names(slab_ref)], slab_ref)

  interval_ref = data.frame(
    datatype = "interval",
    .width = c(0.66, 0.95),
    xmin = c(0, 0),
    xmax = c(1, 1),
    x = c(1, 1),
    stringsAsFactors = FALSE
  )
  attr(interval_ref, "row.names") = c(6L, 7L)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "interval", names(interval_ref)], interval_ref)

  x_scale = p$plot$scales$get_scales("x")
  expect_true(!x_scale$is_discrete())
  expect_equal(x_scale$get_limits(), c(0, 1))
})

test_that("dist_categorical works", {
  p = ggplot_build(
    ggplot() +
      stat_dotsinterval(aes(xdist = dist_categorical(list(3:1/6), list(c("a","b","c")))), quantiles = 6)
  )

  slab_ref = data.frame(
    thickness = rep(1, 6),
    n = 6,
    datatype = "slab",
    .width = NA_real_,
    stringsAsFactors = FALSE
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(1, 1, 1, 2, 2, 3))
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "slab", names(slab_ref)], slab_ref)

  interval_ref = data.frame(
    datatype = "interval",
    .width = c(0.66, 0.95),
    stringsAsFactors = FALSE
  )
  interval_ref$xmin = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  interval_ref$xmax = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  interval_ref$x = ggplot2:::mapped_discrete(c(NA_real_, NA_real_))
  attr(interval_ref, "row.names") = c(7L, 8L)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "interval", names(interval_ref)], interval_ref)

  x_scale = p$plot$scales$get_scales("x")
  expect_true(x_scale$is_discrete())
  expect_equal(x_scale$get_limits(), c("a","b","c"))

  # with integer categorical distribution
  p = ggplot_build(
    ggplot() +
      stat_dotsinterval(aes(xdist = dist_categorical(list(3:1/6))), quantiles = 6)
  )
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "slab", names(slab_ref)], slab_ref)
  expect_equal(p$data[[1]][p$data[[1]]$datatype == "interval", names(interval_ref)], interval_ref)
})

test_that("dist_categorical works with modified scale limits", {
  p = ggplot_build(
    ggplot() +
      stat_dots(aes(xdist = dist_categorical(list(c(3,1)/4), list(c("a","c")))), quantiles = 4) +
      scale_x_discrete(limits = c("a","b","c"))
  )

  slab_ref = data.frame(
    thickness = rep(1, 4),
    n = 4,
    datatype = "slab",
    .width = NA_real_,
    stringsAsFactors = FALSE
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(1,1,1, 3))
  expect_equal(p$data[[1]][, names(slab_ref)], slab_ref)
})

test_that("dist_categorical works with explicit integer levels", {
  p = ggplot_build(
    ggplot() +
      stat_dots(aes(xdist = dist_categorical(list(c(3,1)/4), list(c(1L,3L)))), quantiles = 4)
  )

  slab_ref = data.frame(
    thickness = rep(1, 4),
    n = 4,
    datatype = "slab",
    .width = NA_real_,
    stringsAsFactors = FALSE
  )
  slab_ref$x = ggplot2:::mapped_discrete(c(1,1,1, 2))
  expect_equal(p$data[[1]][, names(slab_ref)], slab_ref)
})


# dot stroke --------------------------------------------------------------

test_that("geom_dots correctly adjusts dot size for stroke size", {
  skip_if_no_vdiffr()

  p = data.frame(x = ppoints(40)) %>%
    ggplot(aes(x = x))

  vdiffr::expect_doppelganger("size = 1 and 3",
    p +
      geom_dots(aes(y = "a"), binwidth = 1/20, size = 1, color = "black") +
      geom_dots(aes(y = "b"), binwidth = 1/20, size = 3, color = "black")
  )

})


# side, justification, scale aes ------------------------------------------

test_that("side, justification, and scale can vary", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("varying side",
    mtcars %>%
      ggplot(aes(x = mpg, y = cyl,
        side = case_when(cyl == 4 ~ "top", cyl == 6 ~ "both", cyl == 8 ~ "bottom"),
        )) +
      stat_dotsinterval(orientation = "horizontal")
  )

  vdiffr::expect_doppelganger("varying side and just",
    mtcars %>%
      ggplot(aes(x = mpg, y = cyl,
        side = case_when(cyl == 4 ~ "top", cyl == 6 ~ "both", cyl == 8 ~ "bottom"),
        justification = case_when(cyl == 4 ~ 1, cyl == 6 ~ 0.25, cyl == 8 ~ 0)
      )) +
      stat_dotsinterval(orientation = "horizontal", scale = 0.5)
  )

  vdiffr::expect_doppelganger("varying scale, side, just",
    tibble(
      x = c(0, rep(1, 9), 0),
      group = c(rep("a", 4), rep("b", 7)),
      scale = c(rep(1/3, 4), rep(2/3, 7)),
      side = c(rep("top", 4), rep("bottom", 7)),
      justification = c(rep(0, 4), rep(1, 7))
    ) %>%
      ggplot(aes(x = x, y = group, scale = scale, side = side, justification = justification, color = group)) +
      stat_dots()
  )
})
