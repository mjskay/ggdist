# Pre-build models used in testing
# These models can take more time to build than we want to spend for rapid testing, so we pre-build them.
#
# Author: mjskay
###############################################################################

library(dplyr)
library(magrittr)
library(rstanarm)
library(brms)


mtcars_tbl = mtcars %>%
  set_rownames(1:nrow(.)) %>%
  as_data_frame()


brms.m_hp = brm(mpg ~ log(hp)*am, data = mtcars_tbl, chains = 1, iter = 1000, family = "lognormal")
saveRDS(brms.m_hp, "tests/testthat/models.brms.m_hp.rds", compress = FALSE)

brms.m_hp_sigma = brm(
  bf(mpg ~ log(hp), sigma ~ hp),
  prior = c(prior(normal(0, 1), class = b)),
  data = mtcars_tbl, chains = 1, iter = 4000, family = lognormal
)
saveRDS(brms.m_hp_sigma, "tests/testthat/models.brms.m_hp_sigma.rds", compress = FALSE)


rstanarm.m_hp_wt = stan_glm(mpg ~ hp*wt, data = mtcars_tbl, chains = 1, iter = 500)
saveRDS(rstanarm.m_hp_wt, "tests/testthat/models.rstanarm.m_hp_wt.rds", compress = FALSE)

rstanarm.m_cyl = stan_glmer(mpg ~ (1|cyl), data = mtcars_tbl, chains = 1, iter = 1000, warmup = 750)
saveRDS(rstanarm.m_cyl, "tests/testthat/models.rstanarm.m_cyl.rds", compress = FALSE)


brms.m_cyl_mpg = brm(ordered(cyl) ~ mpg, data = mtcars_tbl, chains = 1, iter = 500,
  family = cumulative("logit"), threshold = "flexible",
  prior = prior(normal(0,1), class = b))
saveRDS(brms.m_cyl_mpg, "tests/testthat/models.brms.m_cyl_mpg.rds", compress = FALSE)
