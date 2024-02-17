# Tests for MCSE dot plots
#
# Author: mjskay
###############################################################################


test_that("stat_mcse_dots calculates correctly", {
  skip_if_not_installed("posterior")

  set.seed(1234)
  x = rnorm(200)

  # MCSE on all points
  df_all = layer_data(ggplot() + stat_mcse_dots(aes(x)))
  expect_equal(df_all$x, sort(x))
  expect_equal(df_all$sd, posterior::mcse_quantile(x, ppoints(200), names = FALSE))

  # MCSE on 100 quantiles
  df_100 = layer_data(ggplot() + stat_mcse_dots(aes(x), quantiles = 100))
  expect_equal(df_100$x, quantile(x, ppoints(100), type = 5, names = FALSE))
  expect_equal(df_100$sd, posterior::mcse_quantile(x, ppoints(100), names = FALSE))
})
