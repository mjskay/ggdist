# Tests for combine_chains
#
# Author: mjskay
###############################################################################

import::from(dplyr, `%>%`, mutate)
library(tidyr)

context("combine_chains")


test_that("combine_chains works on a simple example", {
  data(line, package = "coda")

  ref = line %>%
    as_sample_tibble() %>%
    mutate(
      .iteration = as.integer(.iteration + (.chain - 1) * max(.iteration)),
      .chain = as.integer(NA)
    )

  line %>%
    as_sample_tibble() %>%
    combine_chains() %>%
    expect_equal(ref)
})
