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
    agg_png(here::here("icons", paste0(name, ".png")), width = 60, height = 60) #, type = "cairo")

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
make_icon(stat_histinterval, data.frame(x = qnorm(ppoints(20))), aes(x = x), breaks = 10)


# dotsinterval family
make_icon(stat_dotsinterval, quantiles = 20, names = "geom_dotsinterval", x_expansion = 0.1)


# lineribbon family
set.seed(1234)
x = 1:70/1000
y = rnorm(70, x / 2)
m = lm(y ~ x)
line_data = bind_cols(data.frame(x, y), predict(m, se.fit = TRUE))
line_aes = aes(x, ydist = dist_student_t(df, fit, se.fit))

make_icon(stat_lineribbon, line_data, line_aes, y_expansion = 0, .width = c(.66, .95), names = c("stat_lineribbon", "geom_lineribbon"))
make_icon(stat_ribbon, line_data, line_aes, y_expansion = 0, .width = c(.66, .95))
