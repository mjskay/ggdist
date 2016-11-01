# Tests for gather_samples
# 
# Author: mjskay
###############################################################################

library(testthat)
import::from(plyr, ldply, .)
import::from(dplyr, `%>%`)
library(tidyr)
library(tidybayes)

context("gather_samples")

test_that("gather_samples works on a simple parameter with no indices", {
        data(RankCorr, package="tidybayes")
        
        ref = data.frame(
            .sample = 1:nrow(RankCorr),
            typical_r = RankCorr[,"typical_r"]
            )
        expect_equal(gather_samples(RankCorr, typical_r[]), ref)
        expect_equal(gather_samples(RankCorr, typical_r), ref)
    })


test_that("gather_samples works on a parameter with one unnamed index", {
        data(RankCorr, package="tidybayes")
        
        ref = ldply(1:18, function(i) {
                data.frame(
                    .sample = 1:nrow(RankCorr),
                    i = i,
                    tau = RankCorr[,paste0("tau[", i, "]")]
                    )
            })
        
        expect_equal(gather_samples(RankCorr, tau[i]) %>% arrange(i), ref)
    })

test_that("gather_samples works on a parameter with one named index", {
        i_labels = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r")
        data(RankCorr, package="tidybayes")
        RankCorr = apply_prototypes(RankCorr, list(i=factor(i_labels)))
        
        ref = ldply(1:18, function(i) {
                data.frame(
                    .sample = 1:nrow(RankCorr),
                    i = i_labels[i],
                    tau = RankCorr[,paste0("tau[", i, "]")]
                )
            })
        
        expect_equal(gather_samples(RankCorr, tau[i]) %>% arrange(i), ref)
    })

test_that("gather_samples works on a parameter with one anonymous wide index", {
        data(RankCorr, package="tidybayes")
        
        ref = data.frame(.sample = 1:nrow(RankCorr)) 
        for (i in 1:18) {
            refcol = data.frame(RankCorr[,paste0("tau[", i, "]")])
            names(refcol) = paste0("tau", i)
            ref = cbind(ref, refcol)
        }
        
        expect_equal(gather_samples(RankCorr, tau[..]), ref)
    })


test_that("gather_samples works on a parameter with one named wide index", {
        i_labels = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r")
        data(RankCorr, package="tidybayes")
        RankCorr = apply_prototypes(RankCorr, list(i=factor(i_labels)))
        
        ref = data.frame(.sample = 1:nrow(RankCorr)) 
        for (i in 1:18) {
            refcol = data.frame(RankCorr[,paste0("tau[", i, "]")])
            names(refcol) = i_labels[i]
            ref = cbind(ref, refcol)
        }
        
        expect_equal(gather_samples(RankCorr, tau[i] | i), ref)
    })


test_that("gather_samples works on a parameter with two named indices", {
        i_labels = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r")
        j_labels = c("A","B","C","D")
        data(RankCorr, package="tidybayes")
        RankCorr = apply_prototypes(RankCorr, 
            list(i = factor(i_labels), j = factor(j_labels)))
        

        ref = ldply(1:4, function(j) {
                ldply(1:18, function(i) {
                    data.frame(
                        .sample = 1:nrow(RankCorr),
                        i = i_labels[i],
                        j = j_labels[j],
                        b = RankCorr[,paste0("b[", i, ",", j, "]")]
                    )
                })
            })
        
        expect_equal(gather_samples(RankCorr, b[i,j]) %>% arrange(j,i), ref)
    })


test_that("gather_samples works on a parameter with two named indices, one that is wide", {
        i_labels = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r")
        j_labels = c("A","B","C","D")
        data(RankCorr, package="tidybayes")
        RankCorr = apply_prototypes(RankCorr, 
            list(i = factor(i_labels), j = factor(j_labels)))
        
        
        ref = ldply(1:4, function(j) {
                ldply(1:18, function(i) {
                    data.frame(
                        .sample = 1:nrow(RankCorr),
                        i = i_labels[i],
                        j = j_labels[j],
                        b = RankCorr[,paste0("b[", i, ",", j, "]")]
                    )
                })
            }) %>%
            spread(j, b)
        
        expect_equal(gather_samples(RankCorr, b[i,j] | j) %>% arrange(.sample), ref)
    })

test_that("gather_samples works on a parameter with one named index and one wide anonymous index", {
        i_labels = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r")
        data(RankCorr, package="tidybayes")
        RankCorr = apply_prototypes(RankCorr, 
            list(i = factor(i_labels)))
        
        
        ref = ldply(1:4, function(j) {
                ldply(1:18, function(i) {
                    data.frame(
                        .sample = 1:nrow(RankCorr),
                        i = i_labels[i],
                        j = paste0("b", j),
                        b = RankCorr[,paste0("b[", i, ",", j, "]")]
                    )
                })
            }) %>%
            spread(j, b)
        
        expect_equal(gather_samples(RankCorr, b[i,..]) %>% arrange(.sample), ref)
    })

test_that("gather_samples does not allow extraction of two variables simultaneously with a wide index", {
        data(RankCorr, package="tidybayes")

        error_message = "Cannot extract samples of multiple variables in wide format."
        expect_error(gather_samples(RankCorr, c(tau, typical_mu)[..]), error_message)
        expect_error(gather_samples(RankCorr, c(tau, typical_mu)[i] | i), error_message)
    })

test_that("gather_samples correctly extracts multiple variables simultaneously", {
        i_labels = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r")
        data(RankCorr, package="tidybayes")
        RankCorr = apply_prototypes(RankCorr, 
            list(i = factor(i_labels)))
        
        expect_equal(gather_samples(RankCorr, c(tau, typical_mu)[i]), 
            gather_samples(RankCorr, tau[i]) %>%
                left_join(gather_samples(RankCorr, typical_mu[i]), by=c(".sample","i"))
        )
        expect_equal(gather_samples(RankCorr, c(tau, typical_mu, u_tau)[i]), 
            gather_samples(RankCorr, tau[i]) %>%
                left_join(gather_samples(RankCorr, typical_mu[i]), by=c(".sample","i")) %>%
                left_join(gather_samples(RankCorr, u_tau[i]), by=c(".sample","i"))
        )
    })

test_that("gather_samples correctly extracts multiple variables simultaneously when those variables have no indices", {
        data(RankCorr, package="tidybayes")
        dimnames(RankCorr)[[2]][[1]] <- "tr2"

        ref1 = gather_samples(RankCorr, typical_r[])
        expect_equal(gather_samples(RankCorr, c(typical_r)[]), ref1)
        expect_equal(gather_samples(RankCorr, c(typical_r)), ref1)

        ref2 = gather_samples(RankCorr, tr2[]) %>%
            left_join(gather_samples(RankCorr, typical_r[]), by=c(".sample"))
        expect_equal(gather_samples(RankCorr, c(tr2, typical_r)[]), ref2)
        expect_equal(gather_samples(RankCorr, c(tr2, typical_r)), ref2)
    })
