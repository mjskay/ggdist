#Geom for blurry dotplots
#
# Author: mjskay
###############################################################################



# blur functions ---------------------------------------------------------------

blur_alpha_0 = function(...) {
  blur_alpha_1(..., rescale = TRUE)
}

blur_alpha_1 = function(x, r, sd, min_alpha = 0.1, rescale = FALSE) {
  # x = seq(0, max(2*sd, r), length.out = n)
  dens = dnorm(x, 0, sd) / dnorm(0, 0, sd)
  weight = 1 - pnorm(x, r, max(sd - abs(r - sd), 0) / 2)
  f = weight + (1 - weight) * dens
  if (rescale) f = pmax(f * dnorm(0, 0, sd) / dnorm(0, 0, min(sd, r/2)), min_alpha)
  f
}

blur_alpha_2 = function(x, r, sd) {
  # x = seq(0, max(2*sd, r), length.out = n)
  dens = dnorm(x, 0, sd) / dnorm(0, 0, sd)
  weight = 1 - pnorm(x, r, max(sd - abs(r - sd), 0))
  weight + (1 - weight) * dens
}

blur_alpha_3 = function(x, r, sd) {
  if (r >= 2*sd) {
    rep(1, length(x))
  } else {
    # x = seq(0, max(2*sd, r), length.out = n)
    dens = dnorm(x, 0, sd) / dnorm(0, 0, sd)
    weight = 1 - pnorm(x, (2*sd - r) / 6 + r, (2*sd - r) / 6)
    weight + (1 - weight) * dens
  }
}

blur_alpha_4 = function(x, r, sd, min_alpha = 0.1) {
  # x = seq(0, max(2*sd, r), length.out = n)
  dens = dnorm(x, 0, sd) / dnorm(0, 0, min(sd, r/2))
  weight = 1 - pnorm(x, r, max(sd - abs(r - sd), 0) / 2)
  pmax(weight + (1 - weight) * dens, min_alpha)
}

blur_alpha_gaussian = function(x, r, sd, min_alpha = 0.1, rescale = FALSE) {
  pnorm(x + r, 0, sd) - pnorm(x - r, 0, sd)
}

blur_alpha_gaussian_dot = function(x, r, sd, min_alpha = 0.1, rescale = FALSE) {
  ifelse(x < r,
    pnorm(r, 0, sd) - pnorm(-r, 0, sd),
    pnorm(x + r, 0, sd) - pnorm(x - r, 0, sd)
  )
}

blur_alpha_interval = function(x, r, sd, min_alpha = 0.1, rescale = FALSE) {
  ifelse(
    x < r, 1, ifelse(
    x < 2*sd, 0.5,
    0
  ))
}

blur_dot = function(x = 0.5, y = 0.5, r = unit(0.5 ,"npc"), sd = unit(0.25, "npc"), n = 100, fill = "black", col = NA, lwd = 1, lty = "solid", vp = NULL, blur_alpha = blur_alpha_interval) {
  r = convertWidth(r, unitTo = "points")
  sd = convertWidth(sd, unitTo = "points")

  groupGrob(do.call(grobTree, .mapply(list(x, y, fill, sd, lwd, lty), NULL, FUN = function(x, y, fill, sd, lwd, lty) {
    # circ = if (as.numeric(r) >= 2 * as.numeric(sd)) {
      # circleGrob(x = x, y = y, gp = gpar(fill = fill, col = NA), r = r)
    # } else {
      blur_width = 2*sd + r #max(2*sd + r, r)
      blur_x = seq(0, as.numeric(blur_width), length.out = n)
      grad_colors = alpha(fill, c(blur_alpha(blur_x, as.numeric(r), as.numeric(sd)), 0))
      grad = radialGradient(grad_colors, r2 = blur_width)
      circ = rectGrob(x = x, y = y, gp = gpar(fill = grad, col = NA), height = 2*r, width = 2*blur_width)
    # }
    grobTree(
      circ,
      circleGrob(x = x, y = y, gp = gpar(col = col, fill = NA, lwd = lwd, lty = lty), r = r)
    )
  })), op = "over")
}
