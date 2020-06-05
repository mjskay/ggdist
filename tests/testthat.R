# This is necessary because some tests fail otherwise; see https://github.com/hadley/testthat/issues/144
Sys.setenv("R_TESTS" = "")

library(testthat)
library(ggdist)

test_check("ggdist")
