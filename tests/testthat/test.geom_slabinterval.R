# Tests for geom_slabinterval
#
# Author: mjskay
###############################################################################

library(dplyr)




# group_slab_data_by -------------------------------------------------

test_that("group_slab_data_by works", {

  df = data.frame(
    x = 1:8,
    ymin = 2:9,
    ymax = 3:10,
    fill = c("a","a","a","b","b","b","b","a"),
    alpha = 1
  )

  ref = data.frame(
    x =    c(1:3, 3.5, 3.5, 3:1,   3.5, 4:7, 7.5, 7.5, 7:4, 3.5,    7.5, 8, 8, 7.5),
    ymin = c(1:3, 3.5, 3.5, 3:1,   3.5, 4:7, 7.5, 7.5, 7:4, 3.5,    7.5, 8, 8, 7.5) + 1,
    ymax = c(1:3, 3.5, 3.5, 3:1,   3.5, 4:7, 7.5, 7.5, 7:4, 3.5,    7.5, 8, 8, 7.5) + 2,
    fill = c(rep("a", 8), rep("b", 12), rep("a", 4)),
    alpha = 1,
    group = c(rep(0, 8), rep(1, 12), rep(2,4)),
    y    = c(3:5, 5.5, 4.5, 4:2,   5.5, 6:9, 9.5, 8.5, 8:5, 4.5,    9.5, 10, 9, 8.5)
  )

  grouped_slab_data = arrange(group_slab_data_by(df, side = "both"), group)
  expect_equal(grouped_slab_data, ref)

  df$fill = "a"
  expect_equal(group_slab_data_by(df, side = "topright"), mutate(df, y = ymax))

})


# geom_slab ---------------------------------------------------------------

test_that("geom_slab works", {
  skip_if_no_vdiffr()


  p = tibble(
    x = seq(-4,4, length.out = 20),
    d = dnorm(x)
  ) %>%
    ggplot(aes(thickness = d))

  vdiffr::expect_doppelganger("geom_slab one group",
    p + geom_slab(aes(x = 1, y = x))
  )

  vdiffr::expect_doppelganger("geom_slabh one group",
    p + geom_slab(aes(x = x, y = 1))
  )

})



# normalization -----------------------------------------------------------

test_that("normalize works", {
  skip_if_no_vdiffr()


  p = tribble(
    ~y, ~id, ~p, ~ dist, ~ mu, ~ sigma,
    1,  "A",  1, "norm",    0,       1,
    1,  "B",  1, "norm",    8,       2,
    2,  "A",  1, "norm",    0,       2,
    2,  "A",  2, "norm",    0,       2,
    1,  "B",  2, "norm",    8,       2,
  ) %>%
    ggplot(aes(y = y, dist = dist, arg1 = mu, arg2 = sigma, fill = id)) +
    facet_grid(~p)

  vdiffr::expect_doppelganger("halfeye with normalize = all",
    p + stat_dist_halfeye(normalize = "all", n = 20)
  )
  vdiffr::expect_doppelganger("halfeye with normalize = panels",
    p + stat_dist_halfeye(normalize = "panels", n = 20)
  )
  vdiffr::expect_doppelganger("halfeye with normalize = xy",
    p + stat_dist_halfeye(normalize = "xy", n = 20)
  )
  vdiffr::expect_doppelganger("halfeye with normalize = groups",
    p + stat_dist_halfeye(normalize = "groups", n = 20)
  )
  vdiffr::expect_doppelganger("halfeye with normalize = none",
    p + stat_dist_halfeye(normalize = "none", n = 20)
  )
})


# alpha in fill colors works ----------------------------------------------

test_that("alpha channel in fill colors works", {
  skip_if_no_vdiffr()


  vdiffr::expect_doppelganger("alpha channel in slab fill",
    data.frame(x = c(0,1), y = "a", d = c(1,2)) %>%
      ggplot(aes(x = x, y = y, thickness = d)) +
      geom_slab(fill = scales::alpha("black", 0.2))
  )
})



# side, justification, and scale can vary ---------------------------------

test_that("side and justification can vary", {
  skip_if_no_vdiffr()

  df = tibble(
    x = rep(1:10, each = 2),
    y = dnorm(x, c(4.5, 5.5), 2),
    g = rep(c("a","b"), 10)
  )

  vdiffr::expect_doppelganger("varying side",
    df %>%
      ggplot(aes(x = x, y = g, color = g, thickness = y,
        side = g,
        scale = ifelse(g == "a", 0.5, 0.25)
      )) +
      geom_slab() +
      scale_side_mirrored()
  )

  vdiffr::expect_doppelganger("varying side and just",
    df %>%
      ggplot(aes(x = x, y = g, thickness = y,
        side = ifelse(g == "a", "top", "bottom"),
        justification = ifelse(g == "a", 1, 0),
        scale = ifelse(g == "a", 0.5, 0.25)
      )) +
      geom_slab()
  )

  expect_error(
    print(newpage = FALSE,
      ggplot(df, aes(x = x, y = g, thickness = y, group = g,
        side = ifelse(x < 5, "top", "bottom")
      )) +
      geom_slab(orientation = "horizontal")
    ),
    "Slab `side` cannot vary within groups"
  )

  expect_error(
    print(newpage = FALSE,
      ggplot(df, aes(x = x, y = g, thickness = y, group = g,
        justification = ifelse(x < 5, 0, 1)
      )) +
      geom_slab(orientation = "horizontal")
    ),
    "Slab `justification` cannot vary within groups"
  )

  expect_error(
    print(newpage = FALSE,
      ggplot(df, aes(x = x, y = g, thickness = y, group = g,
        scale = ifelse(x < 5, 0.5, 0.25)
      )) +
      geom_slab(orientation = "horizontal")
    ),
    "Slab `scale` cannot vary within groups"
  )

})



# Incorrect values of enums -----------------------------------------------

test_that("define_orientation_variables fails on incorrect orientation", {
  expect_error(define_orientation_variables("foo"), "Unknown orientation")
})

test_that("incorrect side, orientation are caught", {
  p = data.frame(x = 1) %>%
    ggplot(aes(x = x, y = x, thickness = x))

  expect_error(print(newpage = FALSE,
    p + geom_slabinterval(side = "foo", orientation = "horizontal")
  ), "Unknown side")

  expect_error(print(newpage = FALSE,
    p + geom_slabinterval(side = "foo", orientation = "vertical")
  ), "Unknown side")

  expect_error(print(newpage = FALSE,
    p + geom_slabinterval(orientation = "foo")
  ), "Unknown orientation")
  expect_error(switch_side("top", "foo"), "Unknown orientation")

  expect_error(print(newpage = FALSE,
    p + geom_slabinterval(fill_type = "foo")
  ), "Unknown fill_type")

})


# no interval data in input -----------------------------------------------

test_that("geoms without interval data are valid", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("slabinterval without interval data", {
    data.frame(x = 1:2) %>%
      ggplot(aes(x = x, xmin = x,xmax = x, datatype = "slab", thickness = x)) +
      geom_slabinterval()
  })
})


# NAs in thickness --------------------------------------------------------

test_that("NAs in thickness produce gaps", {
  skip_if_no_vdiffr()

  vdiffr::expect_doppelganger("slabinterval with NAs in thickness", {
    rbind(
      data.frame(y = "gaps", t = c(NA, 1:2, NA, 2:1, NA), x = 0:6),
      data.frame(y = "blank", t = NA, x = 0:6)
    ) %>%
      ggplot(aes(x, y, thickness = t)) +
      geom_slab(color = "black")
  })

  # when side = "both", should not see outlines wrap around
  vdiffr::expect_doppelganger("side = both with NAs in thickness", {
    data.frame(y = "gaps", t = c(NA, 1:2, NA, 2:1, NA), x = 0:6) %>%
      ggplot(aes(x, y, thickness = t)) +
      geom_slab(color = "black", side = "both")
  })
})

test_that("all-NA thickness produces no slab", {
  grob = layer_grob(ggplot() + geom_slab(aes(x = 1, thickness = NA)))
  expect_length(grob, 1)
  expect_is(grob[[1]], "gTree")
  expect_equal(grob[[1]]$children, gList())
})

test_that("NAs and Infs in x work", {
  skip_if_no_vdiffr()

  expect_warning(
    vdiffr::expect_doppelganger("Inf and NA in x works",
      ggplot() +
        geom_slab(aes(y = "NA", x = c(NA,2:4,NA), thickness = c(0,1,2,0.5,0.25)), color = "black") +
        geom_slab(aes(y = "Inf", x = c(-Inf,2:4,Inf), thickness = c(0,1,2,0.5,0.25)), color = "black")
    ),
    "Removed 2 rows\\s+containing\\s+missing\\s+values"
  )
})

# NA width / xmax -----------------------------------------------------------

test_that("NA width works", {
  skip_if_no_vdiffr()

  expect_warning(
    vdiffr::expect_doppelganger("missing width",
      data.frame(
        y = 1:6, f = c(0,1,0,0,Inf,0),
        g = rep(c("a","b"), each = 3),
        w = rep(c(1, NA), each = 3)
      ) %>% ggplot(aes(x = g, y = y, thickness = f, width = w)) +
        geom_slab(color = "red")
    ),
    "Removed 3 rows"
  )
})
