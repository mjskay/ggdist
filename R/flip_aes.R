# flip_aes
#
# Originally based on https://github.com/lionel-/ggstance/blob/master/R/flip-aes.R by lionel-
# Modified to add flip_aes.function, customizable lookup table and horizontal/vertical lookups by mjskay
###############################################################################

horizontal_aes_lookup = c(
  ymin = "xmin", ymax = "xmax", yend = "xend", y = "x",
  yintercept = "xintercept",
  ymin_final = "xmin_final", ymax_final = "xmax_final",
  y_scales = "x_scales",
  SCALE_Y = "SCALE_X",
  lower = "xlower", middle = "xmiddle", upper = "xupper"
)

vertical_aes_lookup = c(
  xmin = "ymin", xmax = "ymax", xend = "yend", x = "y",
  xintercept = "yintercept",
  xmin_final = "ymin_final", xmax_final = "ymax_final",
  x_scales = "y_scales",
  SCALE_X = "SCALE_Y",
  xlower = "lower", xmiddle = "middle", xupper = "upper"
)

flip_aes_lookup = c(horizontal_aes_lookup, vertical_aes_lookup)


flip_aes = function(x, lookup = flip_aes_lookup) {
  UseMethod("flip_aes")
}

flip_aes.character = function(x, lookup = flip_aes_lookup) {
  flipped = lookup[x]
  x[!is.na(flipped)] = flipped[!is.na(flipped)]
  x
}

flip_aes.data.frame = function(x, lookup = flip_aes_lookup) {
  names(x) = flip_aes(names(x), lookup = lookup)
  x
}

flip_aes.function = function(x, lookup = flip_aes_lookup) function(...) flip_aes(x(...), lookup = lookup)


horizontal_aes = function(x) flip_aes(x, lookup = horizontal_aes_lookup)

vertical_aes = function(x) flip_aes(x, lookup = vertical_aes_lookup)
