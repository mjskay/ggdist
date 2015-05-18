# Tests for extract_samples
# 
# Author: mjskay
###############################################################################

library(testthat)
library(plyr)
library(dplyr)
library(tidyr)
library(tidybayes)

context("compare_levels")

ff_labels = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r")

get_samples = function() {
    data(RankCorr, package="tidybayes")
    ldply(1:18, function(i) {
            data.frame(
                .sample = 1:nrow(RankCorr),
                ff = ff_labels[i],
                tau = RankCorr[,paste0("tau[", i, "]")]
            )
        })
}

test_that("pairwise level comparison works", {
        samples = get_samples()
        
        samples_wide = spread(samples, ff, tau)
        ref = ldply(combn(levels(samples$ff), 2, simplify=FALSE), function(levels.) {
                samples_wide$ff = factor(paste(levels.[[2]], "-", levels.[[1]]))
                samples_wide$tau = samples_wide[[levels.[[2]]]] - samples_wide[[levels.[[1]]]]  
                samples_wide
            }) %>% select(-one_of(ff_labels))

        expect_equal(compare_levels(samples, tau, by=ff, comparison=pairwise), ref)
    })

test_that("ordered level comparison works", {
        samples = get_samples()
        
        samples_wide = spread(samples, ff, tau)
        ref = ldply(llply(2:18, function(i) c(ff_labels[[i]], ff_labels[[i-1]])), function(levels.) {
                samples_wide$ff = factor(paste(levels.[[1]], "-", levels.[[2]]))
                samples_wide$tau = samples_wide[[levels.[[1]]]] - samples_wide[[levels.[[2]]]]  
                samples_wide
            }) %>% select(-one_of(ff_labels))
        
        expect_equal(compare_levels(samples, tau, by=ff, comparison=ordered), ref)
    })

test_that("control level comparison works", {
        samples = get_samples()
        
        samples_wide = spread(samples, ff, tau)
        ref = ldply(llply(2:18, function(i) c(ff_labels[[i]], ff_labels[[1]])), function(levels.) {
                samples_wide$ff = factor(paste(levels.[[1]], "-", levels.[[2]]))
                samples_wide$tau = samples_wide[[levels.[[1]]]] - samples_wide[[levels.[[2]]]]  
                samples_wide
            }) %>% select(-one_of(ff_labels))
        
        expect_equal(compare_levels(samples, tau, by=ff, comparison=control), ref)
    })

test_that("default level comparison selects the correct comparison depending on if `by` is ordered", {
        samples = get_samples()

        expect_equal(compare_levels(samples, tau, by=ff, comparison=default),
            compare_levels(samples, tau, by=ff, comparison=pairwise))

        samples$ff = ordered(samples$ff)
        
        expect_equal(compare_levels(samples, tau, by=ff, comparison=default),
            compare_levels(samples, tau, by=ff, comparison=ordered))
    })

test_that("named functions are supported and named with their own name", {
        samples = get_samples()
        
        samples_wide = spread(samples, ff, tau)
        ref = ldply(llply(2:18, function(i) c(ff_labels[[i]], ff_labels[[1]])), function(levels.) {
                samples_wide$ff = factor(paste(levels.[[1]], "+", levels.[[2]]))
                samples_wide$tau = samples_wide[[levels.[[1]]]] + samples_wide[[levels.[[2]]]]  
                samples_wide
            }) %>% select(-one_of(ff_labels))
    
        expect_equal(compare_levels(samples, tau, by=ff, fun=`+`, comparison=control), ref)
    })

test_that("anonymous functions are supported and named with `:`", {
        samples = get_samples()
        
        samples_wide = spread(samples, ff, tau)
        ref = ldply(llply(2:18, function(i) c(ff_labels[[i]], ff_labels[[1]])), function(levels.) {
                samples_wide$ff = factor(paste(levels.[[1]], ":", levels.[[2]]))
                samples_wide$tau = samples_wide[[levels.[[1]]]] + samples_wide[[levels.[[2]]]]  
                samples_wide
            }) %>% select(-one_of(ff_labels))
            
        expect_equal(compare_levels(samples, tau, by=ff, fun=function(x, y) x + y, comparison=control), ref)
    })

test_that("custom comparisons of lists of character vectors are supported", {
        samples = get_samples()
        
        samples_wide = spread(samples, ff, tau)
        ref = ldply(list(c("a", "b"), c("c", "f")), function(levels.) {
                samples_wide$ff = factor(paste(levels.[[1]], "-", levels.[[2]]))
                samples_wide$tau = samples_wide[[levels.[[1]]]] - samples_wide[[levels.[[2]]]]  
                samples_wide
            }) %>% select(-one_of(ff_labels))
        
        expect_equal(compare_levels(samples, tau, by=ff, comparison=list(c("a", "b"), c("c", "f"))), ref)
    })

test_that("custom comparisons of lists of unevaluated expressions are supported", {
        samples = get_samples()
        
        samples_wide = spread(samples, ff, tau)
        ref = ldply(.(a + b, exp(c - f)), function(levels.) {
                samples_wide$ff = factor(deparse(levels.))
                samples_wide$tau = eval(levels., samples_wide)  
                samples_wide
            }) %>% select(-one_of(ff_labels))
        
        expect_equal(compare_levels(samples, tau, by=ff, comparison=.(a + b, exp(c - f))), ref)
    })
