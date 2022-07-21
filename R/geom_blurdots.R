#Geom for blurry dotplots
#
# Author: mjskay
###############################################################################



# blur functions ---------------------------------------------------------------

blur_alpha_0 = function(...) {
  blur_alpha_1(..., rescale = TRUE)
}

blur_alpha_1 = function(n, r, sd, min_alpha = 0.1, rescale = FALSE) {
  x = seq(0, max(2*sd, r), length.out = n)
  dens = dnorm(x, 0, sd) / dnorm(0, 0, sd)
  weight = 1 - pnorm(x, r, max(sd - abs(r - sd), 0) / 2)
  f = weight + (1 - weight) * dens
  if (rescale) f = pmax(f * dnorm(0, 0, sd) / dnorm(0, 0, min(sd, r/2)), min_alpha)
  f
}

blur_alpha_2 = function(n, r, sd) {
  x = seq(0, max(2*sd, r), length.out = n)
  dens = dnorm(x, 0, sd) / dnorm(0, 0, sd)
  weight = 1 - pnorm(x, r, max(sd - abs(r - sd), 0))
  weight + (1 - weight) * dens
}

blur_alpha_3 = function(n, r, sd) {
  if (r >= 2*sd) {
    rep(1, n)
  } else {
    x = seq(0, max(2*sd, r), length.out = n)
    dens = dnorm(x, 0, sd) / dnorm(0, 0, sd)
    weight = 1 - pnorm(x, (2*sd - r) / 6 + r, (2*sd - r) / 6)
    weight + (1 - weight) * dens
  }
}

blur_alpha_4 = function(n, r, sd, min_alpha = 0.1) {
  x = seq(0, max(2*sd, r), length.out = n)
  dens = dnorm(x, 0, sd) / dnorm(0, 0, min(sd, r/2))
  weight = 1 - pnorm(x, r, max(sd - abs(r - sd), 0) / 2)
  pmax(weight + (1 - weight) * dens, min_alpha)
}

blur_dot = function(x = 0.5, y = 0.5, r = unit(0.5 ,"npc"), sd = unit(0.25, "npc"), n = 20, fill = "black", col = NA, lwd = 1, lty = "solid", vp = NULL, blur_alpha = blur_alpha_1) {
  # r = convertWidth(r, unitTo = "npc")
  # sd = convertWidth(sd, unitTo = "npc")

  groupGrob(do.call(grobTree, .mapply(list(x, y, fill, sd, lwd, lty), NULL, FUN = function(x, y, fill, sd, lwd, lty) {
    sd = unit(sd, "points")
    # circ = if (as.numeric(r) >= 2 * as.numeric(sd)) {
      # circleGrob(x = x, y = y, gp = gpar(fill = fill, col = NA), r = r)
    # } else {
      grad_colors = alpha(fill, c(blur_alpha(n, as.numeric(r), as.numeric(sd)), 0))
      grad = radialGradient(grad_colors, r2 = max(2*sd, r))
      circ = rectGrob(x = x, y = y, gp = gpar(fill = grad, col = NA), height = 2*r, width = max(4*sd, 2*r))
    # }
    grobTree(
      circ,
      circleGrob(x = x, y = y, gp = gpar(col = col, fill = NA, lwd = lwd, lty = lty), r = r)
    )
  })), op = "over")
}
