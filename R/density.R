# density estimators
#
# Author: mjskay
###############################################################################



density_histogram = function(x, breaks = "Sturges", ...) {
  h = hist(x, breaks = breaks, plot = FALSE)

  x_1 = h$breaks[-length(h$breaks)]
  x_mid = h$mids
  x_2 = h$breaks[-1]

  cdf = cumsum(h$density) / sum(h$density)

  list(
    x = c(x_1[[1]],  rbind(x_1, x_mid, x_2), x_2[[length(x_2)]]),
    y = c(       0, rep(h$density, each = 3),                0),
    F_x = c(x_1[[1]], rbind(x_1, x_mid, x_2), x_2[[length(x_2)]])
  )
}

density_bounded = function(x, bounds = c(-Inf, Inf), ...) {
  if (missing(x)) return(partial_self("density_bounded"))

  d = density(x, ...)

  x = d$x
  y = d$y

  left = min(bounds)
  is_left = x < left
  y_left = y[is_left]

  right = max(bounds)
  is_right = x > right
  y_right = y[is_right]

  is_mid = !is_left & !is_right
  x = x[!is_left & !is_right]
  y = y[!is_left & !is_right]

  left_len = min(length(y), length(y_left))
  left_i = seq_len(left_len)
  y[left_i] = y[left_i] + rev(y_left)[left_i]

  right_len = min(length(y), length(y_right))
  right_i = seq_len(right_len)
  right_i_y = length(y) + 1 - right_i
  y[right_i_y] = y[right_i_y] + y_right[right_i]

  d$x = x
  d$y = y
  d$call = match.call()
  d
}
