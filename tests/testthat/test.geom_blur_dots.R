# Tests for blurry dot plots
#
# Author: mjskay
###############################################################################


test_that("geom_blur_dots displays correctly", {
  skip_if_no_vdiffr()
  skip_if_no_gradient()
  skip_if_not_installed("posterior")

  df = data.frame(x = 0, sd = c(0,0.1,0.25,1,2))

  vdiffr::expect_doppelganger("blur types work correctly",
    ggplot(df, aes(x = x, sd = sd)) +
      geom_blur_dots(aes(y = "gaussian")) +
      geom_blur_dots(aes(y = "interval 95%"), blur = "interval") +
      geom_blur_dots(aes(y = "interval +/- 1sd"), blur = blur_interval(.width = 0.6826895)) +
      geom_vline(xintercept = c(-2, -1, -0.25, 0.25, 1, 2), alpha = 0.25),
    writer = write_svg_with_gradient
  )
})
