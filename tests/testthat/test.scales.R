# Tests for scales
#
# Author: mjskay
###############################################################################

library(dplyr)

context("scales")


test_that("direct scale setting works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")


  vdiffr::expect_doppelganger("direct scale setting",
    data.frame(dist = "norm", mean = 1, sd = 2) %>%
      ggplot(aes(y = "", dist = dist, arg1 = mean, arg2 = sd)) +
      stat_dist_halfeyeh(
        n = 20,
        shape = 21,  # this point shape has a fill and outline
        point_color = "red",
        point_fill = "black",
        point_alpha = .1,
        point_size = 6,
        stroke = 2,
        interval_color = "blue",
        # interval sizes are scaled from [1, 6] onto [0.6, 1.4] by default
        # see the interval_size_range option in help("geom_slabinterval")
        interval_size = 8,
        interval_linetype = "dashed",
        interval_alpha = .25,
        # fill sets the fill color of the slab (here the density)
        slab_color = "green",
        slab_fill = "purple",
        slab_size = 3,
        slab_linetype = "dotted",
        slab_alpha = .5
      )
    )
})

test_that("mapping custom aesthetics works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("svglite")


  df = tibble(
      datatype = "slab",
      x = c("a", "b", "c"),
      x_num = c(1, 2, 3),
      y = list(c(-0.75, -0.25)),
      f = .1
    ) %>%
    unnest(y) %>%
    bind_rows(tibble(
      datatype = "interval",
      x = c("a", "b", "c"),
      x_num = c(1, 2, 3),
      y = 0
    ))

  p = df %>%
    ggplot(aes(x = x, y = y, datatype = datatype, ymin = -1, ymax = 1, thickness = f))

  # POINT
  vdiffr::expect_doppelganger("point_color discrete mapping",
    p + geom_slabinterval(aes(point_color = x), normalize = "none", point_size = 5, shape = 21, stroke = 2)
  )
  vdiffr::expect_doppelganger("point_color continuous mapping",
    p + geom_slabinterval(aes(point_color = x_num), normalize = "none", point_size = 5, shape = 21, stroke = 2)
  )

  vdiffr::expect_doppelganger("point_fill discrete mapping",
    p + geom_slabinterval(aes(point_fill = x), normalize = "none", point_size = 5, shape = 21, stroke = 2)
  )
  vdiffr::expect_doppelganger("point_fill continuous mapping",
    p + geom_slabinterval(aes(point_fill = x_num), normalize = "none", point_size = 5, shape = 21, stroke = 2)
  )

  vdiffr::expect_doppelganger("point_alpha discrete mapping",
    p + geom_slabinterval(aes(point_alpha = x), normalize = "none", point_size = 5, shape = 21, stroke = 2)
  )
  vdiffr::expect_doppelganger("point_alpha continuous mapping",
    p + geom_slabinterval(aes(point_alpha = x_num), normalize = "none", point_size = 5, shape = 21, stroke = 2)
  )

  vdiffr::expect_doppelganger("point_size discrete mapping",
    p + geom_slabinterval(aes(point_size = x), normalize = "none", shape = 21, stroke = 2)
  )
  vdiffr::expect_doppelganger("point_size continuous mapping",
    p + geom_slabinterval(aes(point_size = x_num), normalize = "none", shape = 21, stroke = 2)
  )

  # INTERVAL
  vdiffr::expect_doppelganger("interval_color discrete mapping",
    p + geom_slabinterval(aes(interval_color = x), normalize = "none")
  )
  vdiffr::expect_doppelganger("interval_color continuous mapping",
    p + geom_slabinterval(aes(interval_color = x_num), normalize = "none")
  )

  vdiffr::expect_doppelganger("interval_alpha discrete mapping",
    p + geom_slabinterval(aes(interval_alpha = x), normalize = "none")
  )
  vdiffr::expect_doppelganger("interval_alpha continuous mapping",
    p + geom_slabinterval(aes(interval_alpha = x_num), normalize = "none")
  )

  vdiffr::expect_doppelganger("interval_size discrete mapping",
    p + geom_slabinterval(aes(interval_size = x), normalize = "none")
  )
  vdiffr::expect_doppelganger("interval_size continuous mapping",
    p + geom_slabinterval(aes(interval_size = x_num), normalize = "none")
  )

  vdiffr::expect_doppelganger("interval_linetype discrete mapping",
    p + geom_slabinterval(aes(interval_linetype = x), normalize = "none")
  )
  expect_error(
    print(p + geom_slabinterval(aes(interval_linetype = x_num), normalize = "none")),
    "A continuous variable cannot be mapped to linetype"
  )


  # SLAB
  vdiffr::expect_doppelganger("slab_color discrete mapping",
    p + geom_slabinterval(aes(slab_color = x), normalize = "none")
  )
  vdiffr::expect_doppelganger("slab_color continuous mapping",
    p + geom_slabinterval(aes(slab_color = x_num), normalize = "none")
  )

  vdiffr::expect_doppelganger("slab_fill discrete mapping",
    p + geom_slabinterval(aes(slab_fill = x), normalize = "none")
  )
  vdiffr::expect_doppelganger("slab_fill continuous mapping",
    p + geom_slabinterval(aes(slab_fill = x_num), normalize = "none")
  )

  vdiffr::expect_doppelganger("slab_alpha discrete mapping",
    p + geom_slabinterval(aes(slab_alpha = x), normalize = "none")
  )
  vdiffr::expect_doppelganger("slab_alpha continuous mapping",
    p + geom_slabinterval(aes(slab_alpha = x_num), normalize = "none")
  )

  vdiffr::expect_doppelganger("slab_size discrete mapping",
    p + geom_slabinterval(aes(slab_size = x), normalize = "none", slab_color = "black")
  )
  vdiffr::expect_doppelganger("slab_size continuous mapping",
    p + geom_slabinterval(aes(slab_size = x_num), normalize = "none", slab_color = "black")
  )

  vdiffr::expect_doppelganger("slab_linetype discrete mapping",
    p + geom_slabinterval(aes(slab_linetype = x), normalize = "none", slab_color = "black")
  )
  expect_error(
    print(p + geom_slabinterval(aes(slab_linetype = x_num), normalize = "none", size = 10, shape = 21, stroke = 2)),
    "A continuous variable cannot be mapped to linetype"
  )

  vdiffr::expect_doppelganger("slab_linetype plus slab_color mapping",
    p + geom_slabinterval(aes(slab_color = x == "a", slab_linetype = x), normalize = "none")
  )


  # DOTS
  p = data.frame(x = qnorm(ppoints(20)), y = "a") %>%
    rbind(data.frame(x = qnorm(ppoints(20), 3, 2), y = "b")) %>%
    ggplot(aes(x = x, y = y, group = NA))

  vdiffr::expect_doppelganger("dots: slab_color plus slab_shape mapping",
    p + geom_dots(aes(color = x > 1.9, shape = x > 1.9), orientation = "horizontal")
  )

})


