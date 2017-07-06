# Tests for gather_terms
# 
# Author: mjskay
###############################################################################

library(testthat)
import::from(dplyr, `%>%`, group_by)
library(tidyr)
library(tidybayes)

context("gather_terms")

test_that("gather_terms works on the results of as_sample_tibble", {
    data(RankCorr, package="tidybayes")
    
    ref = RankCorr %>%
        as_sample_tibble() %>%
        gather(term, estimate, -.chain, -.iteration) %>%
        group_by(term, add = TRUE)

    expect_equal(RankCorr %>% as_sample_tibble() %>% gather_terms(), ref)
})


test_that("gather_terms works on the results of spread_samples with multiple params and indices", {
    data(RankCorr, package="tidybayes")
    
    ref = RankCorr %>%
        spread_samples(b[i,v], tau[i]) %>%
        gather(term, estimate, -.chain, -.iteration, -i, -v) %>%
        group_by(term, add = TRUE)
    
    expect_equal(RankCorr %>% spread_samples(b[i,v], tau[i]) %>% gather_terms(), ref)
})
