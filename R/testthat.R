# Helpers for testthat tests
#
# Author: mjskay
###############################################################################


# skips -------------------------------------------------------------------

#' skip tests if there is no vdiffr setup or if the setup is likely
#' to produce false positive test failures (e.g. old version of ggplot2)
#' @noRd
skip_if_no_vdiffr = function() {
  testthat::skip_if_not_installed("vdiffr")
}

#' skip tests of plot() on old versions of R (minor changes appear to
#' make them incompatible)
#' @noRd
skip_if_old_plot = function() {
  testthat::skip_if_not(getRversion() > "4.3", "plot() output changed in R 4.3")
}

#' skip tests sensitive to minor changes in density() in R 4.4
#' @noRd
skip_if_sensitive_to_density = function() {
  testthat::skip_if(getRversion() < "4.4", "density() output changed in R 4.4")
}

#' skip tests with minor numerical variations on Mac OS
#' @noRd
skip_if_mac = function() {
  testthat::skip_on_os("mac")
}

#' skip tests if gradient support for visual test cases is not available
#' (old versions of svglite did not support it and so test cases with
#' gradients would be incorrect)
#' @noRd
skip_if_no_gradient = function() {
  testthat::skip_if_not(getRversion() >= "4.2")
  testthat::skip_if_not_installed("svglite", "2.1.0")
  testthat::skip_if_not_installed("fontquiver")
  testthat::skip_if_not_installed("sysfonts")
  testthat::skip_if_not_installed("showtext")
}


# variants ----------------------------------------------------------------

system_os = function() tolower(Sys.info()[["sysname"]])

#' A variant for vdiffr tests that distinguishes between mac platforms and
#' non-mac platforms (since there are minor numerical variations due to this)
#' @noRd
variant_mac = function() {
  if (system_os() == "darwin") "mac" else "not_mac"
}

#' update the mac snapshots in the package using the tests/_snaps folder from
#' an R CMD CHECK run on a mac.
#' After a run on MacOS on Github CI, download the zip archive of the test
#' output, extract it, and run this function on the _snap folder in the archive.
#' @noRd
update_mac_snapshots = function(mac_snap_folder) {
  snap_folder = file.path("tests", "testthat", "_snaps")
  for (test_name in list.dirs(mac_snap_folder, full.names = FALSE, recursive = FALSE)) {
    source_test_folder = file.path(mac_snap_folder, test_name)
    test_folder = file.path(snap_folder, test_name)
    mac_test_folder = file.path(snap_folder, "mac", test_name)
    not_mac_test_folder = file.path(snap_folder, "not_mac", test_name)
    for (snap_file in list.files(source_test_folder, pattern = ".*\\.new\\.svg")) {
      snap_name = substr(snap_file, 1, nchar(snap_file) - 8)
      snap_new_svg = paste0(snap_name, ".new.svg")
      snap_svg = paste0(snap_name, ".svg")

      cat("Updating", test_name, snap_name, "\n")

      dir.create(mac_test_folder, showWarnings = FALSE)
      dir.create(not_mac_test_folder, showWarnings = FALSE)

      file.copy(
        file.path(source_test_folder, snap_new_svg),
        file.path(mac_test_folder, snap_svg),
        overwrite = TRUE
      )
      file.copy(
        file.path(source_test_folder, snap_svg),
        file.path(not_mac_test_folder, snap_svg),
        overwrite = TRUE
      )
      file.remove(
        file.path(test_folder, snap_svg)
      )
    }
  }
}


# vdiffr writers ----------------------------------------------------------

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
