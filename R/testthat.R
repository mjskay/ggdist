# Helpers for testthat tests
#
# Author: mjskay
###############################################################################

#' skip tests if there is no vdiffr setup or if the setup is likely
#' to produce false positive test failures (e.g. old version of ggplot2)
#' @noRd
skip_if_no_vdiffr = function() {
  testthat::skip_if_not_installed("vdiffr")
  testthat::skip_if_not_installed("ggplot2", "3.3.3.9000")
}

#' skip tests senstive to minor changes in density() in R 4.4
#' @noRd
skip_if_sensitive_to_density = function() {
  testthat::skip_if(getRversion() < "4.4", "density() output changed in R 4.4")
}

#' skip tests if gradient support for visual test cases is not available
#' (old versions of svglite did not support it and so test cases with
#' gradients would be incorrect)
#' @noRd
skip_if_no_gradient = function() {
  testthat::skip_if_not(getRversion() >= "4.1")
  testthat::skip_if_not_installed("svglite", "2.1.0")
  testthat::skip_if_not_installed("fontquiver")
  testthat::skip_if_not_installed("sysfonts")
  testthat::skip_if_not_installed("showtext")
}

#' alternative SVG writer that supports gradients
#' @noRd
write_svg_with_gradient = function(plot, file, title = "") {
  # use Liberation Sans and Symbola to avoid platform-specific font differences
  liberation_sans = fontquiver::font_styles("Liberation", "Sans")
  symbola = fontquiver::font("Symbola", "Symbols", "Regular")
  sysfonts::font_add(
    "Liberation Sans",
    regular = liberation_sans$Regular$ttf,
    bold = liberation_sans$Bold$ttf,
    italic = liberation_sans$Italic$ttf,
    bolditalic = liberation_sans$`Bold Italic`$ttf,
    symbol = symbola$ttf
  )

  svglite::svglite(file, width = 10, height = 8, bg = "white", pointsize = 12, standalone = TRUE, always_valid = FALSE)
  showtext::showtext_begin()
  on.exit({
    showtext::showtext_end()
    grDevices::dev.off()
  })

  print(
    plot + ggtitle(title) + theme_test(base_family = "Liberation Sans")
  )
}
