# Tests for combine_chains
#
# Author: mjskay
###############################################################################

library(testthat)
import::from(dplyr, `%>%`, mutate)
library(tidyr)
library(tidybayes)

context("combine_chains")

test_that("combine_chains works on a simple example", {
  data(line, package = "coda")

  ref = line %>%
    as_sample_tibble() %>%
    mutate(
      .iteration = as.integer((.chain - 1) * max(.iteration) + .iteration),
      .chain = as.integer(NA)
    )

  line %>%
    as_sample_tibble() %>%
    combine_chains() %>%
    expect_equal(ref)
})
