# data processing for generating a smaller version of RankCorr
#
# Author: mjskay
###############################################################################

library(dplyr)
library(coda)

# raw RankCorr mcmc.list
raw = readRDS("data-raw/RankCorr.rds")

# variable names to subset the chains to
var_names = grepl("(b\\[[1-3],[1-4]\\])|tau\\[[1-3]\\]|typical_r", dimnames(raw[[1]])[[2]])

# additional thinning to apply to the chains
thin_extra = 20

# subset the chains to only the desired variables and apply extra thinning
RankCorr = raw %>%
  lapply(function(chain) {
    chain[seq(1, nrow(chain), by = thin_extra), var_names] %>%
      mcmc(mcpar(chain)[[1]], mcpar(chain)[[2]], mcpar(chain)[[3]] * thin_extra)
  }) %>%
  as.mcmc.list()

usethis::use_data(RankCorr, overwrite = TRUE, compress = 'xz')

RankCorr_u_tau = RankCorr %>%
  tidybayes::spread_draws(u_tau[i]) %>%
  as.data.frame()

usethis::use_data(RankCorr_u_tau, overwrite = TRUE, compress = 'xz')
