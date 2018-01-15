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
  set_rownames(seq_len(nrow(.))) %>%
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


brms.m_cyl_mpg = brm(ordered(paste0("c", cyl)) ~ mpg, data = mtcars_tbl, chains = 1, iter = 500,
  family = cumulative("logit"),
  prior = prior(normal(0,1), class = b))
saveRDS(brms.m_cyl_mpg, "tests/testthat/models.brms.m_cyl_mpg.rds", compress = FALSE)


# simple nlpars brms model
set.seed(1234)
b = c(2, 0.75)
x = rnorm(100)
y = rnorm(100, mean = b[1] * exp(b[2] * x))
df_nlpar = data.frame(x, y)
prior_nlpar = c(prior(normal(1, 2), nlpar = "b1"), prior(normal(0, 2), nlpar = "b2"))
brms.m_nlpar <- brm(bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE), data = df_nlpar, prior = prior_nlpar)
saveRDS(brms.m_nlpar, "tests/testthat/models.brms.m_nlpar.rds", compress = FALSE)


# simple brms model with multiple dpars
set.seed(1234)
df_dpars <- data.frame(
  count = rpois(236, lambda = 20),
  visit = rep(1:4, each = 59),
  patient = factor(rep(1:59, 4)),
  Age = rnorm(236),
  Trt = factor(sample(0:1, 236, TRUE)),
  AgeSD = abs(rnorm(236, 1)),
  Exp = sample(1:5, 236, TRUE),
  volume = rnorm(236)
)
brms.m_dpars <- brm(
  bf(count ~ Age + (1|visit), mu2 ~ Age), data = df_dpars,
  family = mixture(gaussian, exponential),
  prior = c(prior(normal(0, 10), Intercept, dpar = mu1),
    prior(normal(0, 1), Intercept, dpar = mu2),
    prior(normal(0, 1), dpar = mu2)),
  warmup = 150, iter = 200, chains = 2
)
saveRDS(brms.m_dpars, "tests/testthat/models.brms.m_dpars.rds", compress = FALSE)
