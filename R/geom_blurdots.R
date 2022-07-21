#Geom for blurry dotplots
#
# Author: mjskay
###############################################################################



# blur functions ---------------------------------------------------------------

blur_alpha_1 = function(n, r, sd) {
  x = seq(0, max(2*sd, r), length.out = n)
  dens = dnorm(x, 0, sd) / dnorm(0, 0, sd)
  weight = 1 - pnorm(x, r, max(sd - abs(r - sd), 0) / 2)
  weight + (1 - weight) * dens
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

blur_dot = function(x = 0.5, y = 0.5, r = unit(0.5 ,"npc"), sd = unit(0.25, "npc"), n = 20, fill = "black", vp = NULL, blur_alpha = blur_alpha_2) {
  # r = convertWidth(r, unitTo = "npc")
  # sd = convertWidth(sd, unitTo = "npc")

  do.call(grobTree, .mapply(list(x, y, fill), NULL, FUN = function(x, y, fill) {
    circ = if (as.numeric(r) >= 2 * as.numeric(sd)) {
      circleGrob(x = x, y = y, gp = gpar(fill = fill), r = r)
    } else {
      grad_colors = alpha(fill, c(blur_alpha(n, as.numeric(r), as.numeric(sd)), 0))
      grad = radialGradient(grad_colors, r2 = sd*2)
      rectGrob(x = x, y = y, gp = gpar(fill = grad, col = NA), height = 2*r, width = max(4*sd, 2*r))
    }
    # grobTree(
      # circ,
      # circleGrob(x = x, y = y, gp = gpar(col = "red", fill = NA), r = r)
    # )
    circ
  }))
}
