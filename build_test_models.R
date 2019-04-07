# Pre-build models used in testing

# These models can take more time to build than we want to spend for rapid testing, so we pre-build them.
# We also build them with a small number of iterations because we aren't really interested in the results,
# just in making sure the data manipulations of draws from the models work. They should also be run
# with save_warmup = FALSE, save_dso = FALSE and compressed with compress = "xz" to save space on disk
# (the save_dso argument does not apply to rstanarm models). We also employ a bit of a hack by
# NULLing out the saved dso within the returned fits, since save_dso doesn't seem to do this (saves
# about half a megabyte of space per model on disk).
#
# Author: mjskay
###############################################################################

library(dplyr)
library(magrittr)
library(rstanarm)
library(brms)
library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


mtcars_tbl = mtcars %>%
  set_rownames(seq_len(nrow(.))) %>%
  as_tibble()



# brms models -------------------------------------------------------------

# this function removes unnecessary parts of brms fits that we don't use
# during testing and which make the model files large (to decrease the
# size of the package)
strip_brms_model = function(m) {
  slot(m$fit, "stanmodel", check = FALSE) = NULL
  m
}


set.seed(94)
brms.m_hp = brm(mpg ~ log(hp)*am, data = mtcars_tbl, chains = 2,
  warmup = 950, iter = 1000, family = "lognormal",
  save_warmup = FALSE, stan_model_args = list(save_dso = FALSE)
)
saveRDS(strip_brms_model(brms.m_hp), "tests/models/models.brms.m_hp.rds", compress = "xz")


set.seed(943)
brms.m_hp_sigma = brm(
  bf(mpg ~ log(hp), sigma ~ hp),
  prior = c(prior(normal(0, 1), class = b)),
  data = mtcars_tbl, chains = 2, warmup = 950, iter = 1000, family = lognormal,
  save_warmup = FALSE, stan_model_args = list(save_dso = FALSE)
)
saveRDS(strip_brms_model(brms.m_hp_sigma), "tests/models/models.brms.m_hp_sigma.rds", compress = "xz")


set.seed(943943)
brms.m_cyl_mpg = brm(ordered(paste0("c", cyl)) ~ mpg, data = mtcars_tbl,
  chains = 2, iter = 500, warmup = 450,
  family = cumulative("logit"),
  prior = prior(normal(0,1), class = b),
  save_warmup = FALSE, stan_model_args = list(save_dso = FALSE)
)
saveRDS(strip_brms_model(brms.m_cyl_mpg), "tests/models/models.brms.m_cyl_mpg.rds", compress = "xz")


# simple nlpars brms model
set.seed(1234)
b = c(2, 0.75)
x = rnorm(100)
y = rnorm(100, mean = b[1] * exp(b[2] * x))
df_nlpar = data.frame(x, y)
prior_nlpar = c(prior(normal(1, 2), nlpar = "b1"), prior(normal(0, 2), nlpar = "b2"))
brms.m_nlpar = brm(bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE), data = df_nlpar,
  prior = prior_nlpar,
  chains = 2, warmup = 150, iter = 200,
  save_warmup = FALSE, stan_model_args = list(save_dso = FALSE)
)
saveRDS(strip_brms_model(brms.m_nlpar), "tests/models/models.brms.m_nlpar.rds", compress = "xz")


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
  family = mixture(gaussian, brms::exponential),
  prior = c(prior(normal(0, 10), Intercept, dpar = mu1),
    prior(normal(0, 1), Intercept, dpar = mu2),
    prior(normal(0, 1), dpar = mu2)),
  warmup = 150, iter = 200, chains = 2,
  save_warmup = FALSE, stan_model_args = list(save_dso = FALSE)
)
saveRDS(strip_brms_model(brms.m_dpars), "tests/models/models.brms.m_dpars.rds", compress = "xz")


# brms model with random intercept
set.seed(3932)
ranef_data = tibble(
  group = rep(c("a","b","c","d","e"), each = 10),
  group_mean = rep(rnorm(5), each = 10),
  x = rep(1:10, 5),
  y = rnorm(50, group_mean + x)
)
brms.m_ranef = brm(
  y ~ x + (1|group) + 0 + intercept,
  data = ranef_data,
  prior = c(
    prior(normal(0, 1), class = b),
    prior(student_t(3, 0, 4), class = sd),
    prior(student_t(3, 0, 4), class = sigma)
  ),
  control = list(adapt_delta = 0.95),
  warmup = 950, iter = 1000, chains = 2,
  save_warmup = FALSE, stan_model_args = list(save_dso = FALSE)
)
saveRDS(strip_brms_model(brms.m_ranef), "tests/models/models.brms.m_ranef.rds", compress = "xz")


# simple dirichlet model for testing prediction / fit output,
# see https://github.com/mjskay/tidybayes/issues/164
set.seed(1234)
dirich_df = tibble(x = rep(c("A", "B"), each = 10))
dirich_df$Y = as.matrix(rdirichlet(20, c(1,2,1)))
dimnames(dirich_df$Y) = list(NULL, c("y1", "y2", "y3"))

brms.m_dirich = brm(Y ~ x, family = dirichlet(), data = dirich_df,
  warmup = 950, iter = 1000, chains = 2,
  save_warmup = FALSE, stan_model_args = list(save_dso = FALSE)
)
saveRDS(strip_brms_model(brms.m_dirich), "tests/models/models.brms.m_dirich.rds", compress = "xz")


# rstanarm models ---------------------------------------------------------

set.seed(9439)
rstanarm.m_hp_wt = stan_glm(mpg ~ hp*wt, data = mtcars_tbl,
  chains = 2, warmup = 950, iter = 1000,
  save_warmup = FALSE
)
saveRDS(rstanarm.m_hp_wt, "tests/models/models.rstanarm.m_hp_wt.rds", compress = "xz")


set.seed(94394)
rstanarm.m_cyl = stan_glmer(mpg ~ (1|cyl), data = mtcars_tbl,
  chains = 2, iter = 3000, warmup = 2950,
  save_warmup = FALSE
)
saveRDS(rstanarm.m_cyl, "tests/models/models.rstanarm.m_cyl.rds", compress = "xz")

#rstanarm model with random intercept
set.seed(48431)
rstanarm.m_ranef = stan_glmer(
  y ~ x + (1|group),
  data = ranef_data,
  warmup = 150, iter = 200, chains = 2,
  save_warmup = FALSE
)
saveRDS(rstanarm.m_ranef, "tests/models/models.rstanarm.m_ranef.rds", compress = "xz")


# Stan models -----------------------------------------------------------------
set.seed(94302)
ABC_data = list(
  condition = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1,
    2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2,
    3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
  n_condition = 5,
  response = c(-0.420427740393149,
    1.69217967173929, 1.37225406868617, 1.03507138321366, -0.144279563648813,
    -0.301453990727333, 0.763916807416527, 1.68231434373786,
    0.857113182566923, -0.930945887598039, 0.61381517192673,
    0.599110272673588, 1.4598036999863, 0.921232821946559, -1.53588001993896,
    -0.0694930702749228, 0.701343452643537, 0.908016619954207,
    1.12040862796835, -1.12967770336713, 0.450255972666626, 1.47093469693387,
    2.73398095170985, 1.35338054477896, -0.590495534868924, -0.146740924351226,
    1.70929453624297, 2.74938691370324, 0.671458952757168, -1.426397720001,
    0.157957519180733, 1.55484708382943, 3.10773028583902, 1.60855181947867,
    -0.260389106680863, 0.475786916208929, 0.495233677018717,
    0.999763630681023, 0.118907063773971, -1.07130406297753,
    0.775030184741565, 0.598788409142614, 1.96271054005866, 1.94783397736123,
    -1.22828447046022, 0.28111168132151, 0.556495744244286, 1.76987771190241,
    0.637835756966264, -1.03460557791706),
  n = 50)
rstan.m_ABC = stan(model_code = "
  data {
    int<lower=1> n;
    int<lower=1> n_condition;
    int<lower=1, upper=n_condition> condition[n];
    real response[n];
  }
  parameters {
    real overall_mean;
    vector[n_condition] condition_zoffset;
    real<lower=0> response_sd;
    real<lower=0> condition_mean_sd;
  }
  transformed parameters {
    vector[n_condition] condition_mean;
    condition_mean = overall_mean + condition_zoffset * condition_mean_sd;
  }
  model {
    response_sd ~ exponential(1);
    condition_mean_sd ~ exponential(1);
    overall_mean ~ normal(0, 5);
    condition_zoffset ~ normal(0, 1);   // => condition_mean ~ normal(overall_mean, condition_mean_sd)
    for (i in 1:n) {
      response[i] ~ normal(condition_mean[condition[i]], response_sd);
    }
  }", data = ABC_data, control = list(adapt_delta=0.99),
  warmup = 2950, iter = 3000, chains = 2,
  save_warmup = FALSE, save_dso = FALSE
)
saveRDS(rstan.m_ABC, "tests/models/models.rstan.m_ABC.rds", compress = "xz")
