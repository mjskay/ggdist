# Charger le package
library(devtools)
library(dplyr)
library(ggplot2)
library(ggridges)
library(gtable)

devtools::load_all("../GGdist_issue_regler")
devtools::load_all()
library(ggdist)

tibble(
  x = c( rgamma( 1e5 , 1^2 / 3^2 , 1 / 3^2 ),
         rgamma( 1e5 , 2^2 / 3^2 , 2 / 3^2 ) ),
  group = rep(c("a", "b"), each = 1e5)
) %>%
  ggplot() +
  geom_density(aes(x = x, colour = group)) + # comment out as needed
  geom_violin(aes(x = x, y = group)) +
  geom_density_ridges(aes(x = x, y = group),
                      from = 0, to = 0.5) +
  stat_slab(aes(x = x, y = group), height = 2,
            limits = c(0, 0.5)) +
  stat_eye(aes(x = x, y = group),
           limits = c(0, 0.5), point_interval = NULL) +
  scale_x_continuous(limits = c(0, 0.5),
                     oob = scales::oob_keep) +
  theme_minimal()
