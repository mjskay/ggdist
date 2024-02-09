# Tests for student_t
#
# Author: mjskay
###############################################################################




test_that("student_t functions work", {
  median = 1
  scale = 2
  df = 4

  p = ppoints(10)
  log_p = log(p)
  x = qt(p, df) * scale + median
  d = dt((x - median)/scale, df) / scale
  log_d = dt((x - median)/scale, df, log = TRUE) - log(scale)
  set.seed(1234)
  r = rt(10, df) * scale + median

  expect_equal(dstudent_t(x, df, median, scale), d)
  expect_equal(dstudent_t(x, df, median, scale, log = TRUE), log_d)
  expect_equal(pstudent_t(x, df, median, scale), p)
  expect_equal(pstudent_t(x, df, median, scale, log.p = TRUE), log_p)
  expect_equal(qstudent_t(p, df, median, scale), x)
  expect_equal(qstudent_t(log_p, df, median, scale, log.p = TRUE), x)
  set.seed(1234)
  expect_equal(rstudent_t(10, df, median, scale), r)
})
