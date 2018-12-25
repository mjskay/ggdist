# Tests for compose_data
#
# Author: mjskay
###############################################################################

library(dplyr)

context("compose_data")


get_nested_data = function() {
  data.frame(
    plot = factor(paste0("p", rep(1:8, times = 2))),
    site = factor(paste0("s", rep(1:4, each = 2, times = 2)))
  )
}

test_that("compose_data returns pure lists (for compatibility with runjags)", {
  df = get_nested_data()

  expect_equal(class(compose_data(df)), "list")
})

test_that("compose_data works for a basic set of factors", {
  df = get_nested_data()

  ref = list(
    plot = as.array(as.numeric(df$plot)),
    n_plot = nlevels(df$plot),
    site = as.array(as.numeric(df$site)),
    n_site = nlevels(df$site),
    n = nrow(df)
  )

  expect_equal(compose_data(df), ref)
})

test_that("compose_data works with two data frames and a named argument", {
  df = get_nested_data()
  df2 = get_nested_data() %>%
    transmute(
      plot2 = plot,
      site2 = site
    )

  ref = list(
    plot = as.array(as.numeric(df$plot)),
    n_plot = nlevels(df$plot),
    site = as.array(as.numeric(df$site)),
    n_site = nlevels(df$site),
    n = nrow(df),
    plot2 = as.array(as.numeric(df2$plot2)),
    n_plot2 = nlevels(df2$plot2),
    site2 = as.array(as.numeric(df2$site2)),
    n_site2 = nlevels(df2$site2),
    n_d2 = nrow(df2)
  )

  expect_equal(compose_data(df, d2 = df2), ref)
})

test_that("compose_data arguments are evaluated within the list composed so far", {
  df = get_nested_data()

  ref = list(
    plot = as.array(as.numeric(df$plot)),
    n_plot = nlevels(df$plot),
    site = as.array(as.numeric(df$site)),
    n_site = nlevels(df$site),
    n = nrow(df),
    site2 = as.array(as.numeric(df$site) + 1)
  )

  expect_equal(compose_data(df, site2 = site + 1), ref)
})

test_that("setting compose_data arguments to null removes them from the final list", {
  expect_equal(compose_data(x = 1, x = NULL), set_names(list(), character(0)))
})

test_that("as_data_list psses through existing data lists unchanged", {
  expect_equal(data_list(x = 1), as_data_list(data_list(x = 1)))
})

test_that("compose_data converts character vectors into dimension indices", {
  ref = list(x = array(c(1, 2, 3)), n_x = 3, n = 3)

  expect_equal(compose_data(tibble(x = c("a","b","c"))), ref)
})

test_that("compose_data warns if there are unused levels in a factor", {
  expect_warning(compose_data(x = factor(c("a","b","c"), levels = c("a","b","c","d"))),
    'Some levels of factor "x" are unused. This may cause issues if you are using it as the dimension for a variable in a model.')
})

test_that("compose_data converts logicals correctly", {
  expect_equal(compose_data(x = c(TRUE, FALSE)), list(x = c(1, 0)))
})

test_that("warning generated for unnamed values", {
  expect_warning(compose_data(set_names(list(1, 2), c("a", ""))),
    "No name provided for value `2`")
})

test_that("data_list print method gives correct output", {
  expect_output(print(data_list(x = 1)), "data_list:\n\n$x\n[1] 1\n", fixed = TRUE)
})

test_that("unsupported formats are dropped", {
  expect_warning(res <- compose_data(a = 1, b = Sys.Date()),
    '"b" has unsupported type "Date" and was dropped.')
  expect_equal(res, list(a = 1))
})
