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
      geom_blur_dots(aes(y = "interval 95%"), blur = "interval", color = "red") +
      geom_blur_dots(aes(y = "interval +/- 1sd"), blur = blur_interval(.width = 0.6826895)) +
      geom_vline(xintercept = c(-2, -1, -0.25, 0.25, 1, 2), alpha = 0.25)
    ,
    writer = write_svg_with_gradient
  )

  vdiffr::expect_doppelganger("square blur",
    ggplot(df, aes(x = x, sd = sd)) +
      geom_blur_dots(aes(y = "gaussian"), shape = "square") +
      geom_blur_dots(aes(y = "interval 95%"), blur = "interval", shape = 15, color = "red") +
      geom_blur_dots(aes(y = "interval +/- 1sd"), blur = blur_interval(.width = 0.6826895), shape = 0) +
      geom_vline(xintercept = c(-2, -1, -0.25, 0.25, 1, 2), alpha = 0.25)
    ,
    writer = write_svg_with_gradient
  )

  expect_error(
    print(
      ggplot(df, aes(x = x, sd = sd)) +
        geom_blur_dots(aes(y = "gaussian"), shape = 2),
      newpage = FALSE
    ),
    class = "ggdist_invalid_blur_dot_shape"
  )

})
