# Build icons for pkgdown documentation
#
# Author: mjskay
###############################################################################

library(ggplot2)
library(dplyr)
library(ggdist)
library(distributional)
library(ragg)


make_icon = function(
  geom,
  data = data.frame(dist = dist_normal(0, 1)),
  mapping = aes(xdist = dist),
  ...,
  names = deparse1(substitute(geom)),
  x_expansion = 0,
  y_expansion = 0.4
) {
  for (name in names) tryCatch({
    agg_png(here::here("icons", paste0(name, ".png")), width = 60, height = 60)

    p = ggplot(data, mapping) +
      geom(..., interval_size_range = c(0.6, 1.4) * 1.5) +
      theme_void() +
      scale_color_brewer(palette = "Greys") +
      scale_fill_brewer(palette = "Greys") +
      scale_x_continuous(expand = expansion(x_expansion)) +
      scale_y_continuous(expand = expansion(y_expansion)) +
      guides(color = "none", fill = "none")
    print(p)
  }, finally = {
    dev.off()
  })
}


# slabinterval family
make_icon(stat_slabinterval, names = c("stat_slabinterval", "geom_slabinterval"))
make_icon(stat_pointinterval, names = c("stat_pointinterval", "geom_pointinterval"))
make_icon(stat_interval, size = 25, names = c("stat_interval", "geom_interval"))
make_icon(stat_slab, names = c("stat_slab", "geom_slab"))
make_icon(stat_halfeye)
make_icon(stat_eye)
make_icon(stat_ccdfinterval)
make_icon(stat_cdfinterval)
make_icon(stat_gradientinterval, fill_type = "gradient")
make_icon(stat_histinterval, data.frame(x = qnorm(ppoints(20))), aes(x = x), breaks = 8)

# spike family
make_icon(stat_spike, names = c("stat_spike", "geom_spike"), at = \(x) quantile(x, ppoints(7, a = 0))[[1]], x_expansion = 0.1)

# dotsinterval family
make_icon(stat_dotsinterval, quantiles = 20, names = c("geom_dotsinterval", "stat_dotsinterval"), x_expansion = 0.1)
make_icon(stat_dots, quantiles = 20, names = c("geom_dots", "stat_dots"), x_expansion = 0.1)
make_icon(stat_dots, side = "both", layout = "weave", quantiles = 30, names = c("geom_weave"), x_expansion = 0.1)
make_icon(stat_dots, side = "both", layout = "swarm", quantiles = 31, names = c("geom_swarm"), x_expansion = 0.1)
make_icon(
  stat_mcse_dots,
  data = data.frame(dist = dist_sample(list(sample(qnorm(ppoints(75)), 75)))),
  quantiles = 18, names = c("geom_blur_dots", "stat_mcse_dots"), x_expansion = 0.1
)


# lineribbon family
set.seed(1234)
x = 1:70/1000
y = rnorm(70, x / 2)
m = lm(y ~ x)
line_data = bind_cols(data.frame(x, y), predict(m, se.fit = TRUE))
line_aes = aes(x, ydist = dist_student_t(df, fit, se.fit))

make_icon(stat_lineribbon, line_data, line_aes, y_expansion = 0, .width = c(.66, .95), names = c("stat_lineribbon", "geom_lineribbon"))
make_icon(stat_ribbon, line_data, line_aes, y_expansion = 0, .width = c(.66, .95))
