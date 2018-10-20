# Tests for combine_chains
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)

context("combine_chains")


test_that("combine_chains works on a simple example", {
  data(line, package = "coda")

  ref = line %>%
    tidy_draws() %>%
    mutate(
      .draw = as.integer(.iteration + (.chain - 1) * max(.iteration))
    )

  line %>%
    tidy_draws() %>%
    combine_chains() %>%
    expect_equal(ref)
})

test_that("combine_chains works with a named output column", {
  data(line, package = "coda")

  ref = line %>%
    tidy_draws() %>%
    mutate(d = as.integer(.iteration + (.chain - 1) * max(.iteration)))

  line %>%
    tidy_draws() %>%
    combine_chains(into = "d") %>%
    expect_equal(ref)
})
