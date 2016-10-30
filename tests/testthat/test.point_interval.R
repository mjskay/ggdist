# Tests for point_interval
# 
# Author: mjskay
###############################################################################

library(testthat)
import::from(plyr, ldply, llply, .)
import::from(dplyr, `%>%`, group_by, summarise, ungroup)
library(tidyr)
library(tidybayes)

context("point_interval")

ff_labels = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r")

get_samples = function() {
    #observations of tau grouped by the factor ff (with levels ff_labels)
    data(RankCorr, package="tidybayes")
    ldply(1:18, function(i) {
            data.frame(
                .sample = 1:nrow(RankCorr),
                ff = ff_labels[i],
                tau = RankCorr[,paste0("tau[", i, "]")]
            )
        })
}

test_that("mean_qi works on a grouped variable", {
        samples = get_samples()
        
        ref = samples %>%
            group_by(ff) %>%
            summarise(
                tau.lower = quantile(tau, .025),
                tau.upper = quantile(tau, .975),
                tau = mean(tau)
            )
        
        result = samples %>%
            group_by(ff) %>%
            mean_qi(tau)
        
        expect_equal(result$tau, ref$tau)
        expect_equal(result$tau.lower, ref$tau.lower)
        expect_equal(result$tau.upper, ref$tau.upper)
})

test_that("mean_qi works on multiple columns", {
        samples = get_samples() %>%
            group_by(.sample) %>%
            spread(ff, tau) %>%
            ungroup()
        
        ref = samples %>%
            summarise(
                a.lower = as.vector(quantile(a, .025)),
                a.upper = as.vector(quantile(a, .975)),
                a = mean(a),
                b.lower = as.vector(quantile(b, .025)),
                b.upper = as.vector(quantile(b, .975)),
                b = mean(b)
            )
    
        result = samples %>%
            mean_qi(a, b)
        
        expect_equal(result$a, ref$a)
        expect_equal(result$a.lower, ref$a.lower)
        expect_equal(result$a.upper, ref$a.upper)
        expect_equal(result$b, ref$b)
        expect_equal(result$b.lower, ref$b.lower)
        expect_equal(result$b.upper, ref$b.upper)
})
