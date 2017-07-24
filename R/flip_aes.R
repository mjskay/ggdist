# flip_aes
#
# Based on https://github.com/lionel-/ggstance/blob/master/R/flip-aes.R by lionel-
# Modified to add flip_aes.function by mjskay
###############################################################################

flip_lookup <- c(
  xmin = "ymin", xmax = "ymax", xend = "yend", x = "y",
  ymin = "xmin", ymax = "xmax", yend = "xend", y = "x",
  xintercept = "yintercept", xmin_final = "ymin_final",
  xmax_final = "ymax_final", yintercept = "xintercept",
  ymin_final = "xmin_final", ymax_final = "xmax_final",
  x_scales = "y_scales", y_scales = "x_scales",
  SCALE_X = "SCALE_Y", SCALE_Y = "SCALE_X",
  lower = "xlower", middle = "xmiddle", upper = "xupper",
  xlower = "lower", xmiddle = "middle", xupper = "upper"
)


flip_aes <- function(x) {
  UseMethod("flip_aes")
}

flip_aes.character <- function(x) {
  flipped <- flip_lookup[x]
  x[!is.na(flipped)] <- flipped[!is.na(flipped)]
  x
}

flip_aes.data.frame <- function(x) {
  names(x) <- flip_aes(names(x))
  x
}

flip_aes.function <- function(x) function(...) flip_aes(x(...))
