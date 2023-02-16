# Tests for point_interval
#
# Author: mjskay
###############################################################################

library(dplyr)
library(tidyr)
library(distributional)



ff_labels = c("a", "b", "c")

get_draws = function() {
  #observations of tau grouped by the factor ff (with levels ff_labels)
  data(RankCorr, package = "ggdist")
  rank_corr = RankCorr[[1]]
  purrr::map_dfr(1:3, function(i) {
    data.frame(
      .chain = as.integer(1),
      .iteration = seq_len(nrow(rank_corr)),
      .draw = seq_len(nrow(rank_corr)),
      ff = ff_labels[i],
      tau = as.vector(rank_corr[, paste0("tau[", i, "]")])
    )
  })
}

test_that("median_qi works on a grouped variable", {
  draws = get_draws()

  ref = draws %>%
    group_by(ff) %>%
    summarise(
      tau.lower = as.vector(quantile(tau, .025)),
      tau.upper = as.vector(quantile(tau, .975)),
      tau = median(tau)
    )

  result.simple = draws %>%
    group_by(ff) %>%
    median_qi(tau)

  result = draws %>%
    group_by(ff) %>%
    median_qi(tau, .simple_names = FALSE)

  expect_equal(result.simple$tau, ref$tau)
  expect_equal(result.simple$.lower, ref$tau.lower)
  expect_equal(result.simple$.upper, ref$tau.upper)
  expect_equal(result$tau, ref$tau)
  expect_equal(result$tau.lower, ref$tau.lower)
  expect_equal(result$tau.upper, ref$tau.upper)
})

test_that("mean_qi works on multiple columns", {
  draws = get_draws() %>%
    group_by(.iteration) %>%
    spread(ff, tau) %>%
    ungroup()

  ref = draws %>%
    summarise(
      a.lower = as.vector(quantile(a, .025)),
      a.upper = as.vector(quantile(a, .975)),
      a = mean(a),
      b.lower = as.vector(quantile(b, .025)),
      b.upper = as.vector(quantile(b, .975)),
      b = mean(b)
    )

  result = draws %>%
    mean_qi(a, b)

  expect_equal(result$a, ref$a)
  expect_equal(result$a.lower, ref$a.lower)
  expect_equal(result$a.upper, ref$a.upper)
  expect_equal(result$b, ref$b)
  expect_equal(result$b.lower, ref$b.lower)
  expect_equal(result$b.upper, ref$b.upper)
})

test_that("mean_qi works on non-95% probs", {
  draws = get_draws()

  ref = draws %>%
    summarise(
      tau.lower = as.vector(quantile(tau, .25)),
      tau.upper = as.vector(quantile(tau, .75)),
      tau = mean(tau)
    )

  result = draws %>%
    mean_qi(tau, .width = .5)

  expect_equal(result$tau, ref$tau)
  expect_equal(result$.lower, ref$tau.lower)
  expect_equal(result$.upper, ref$tau.upper)
})

test_that("mean_qi works on multiple probs with groups", {
  draws = get_draws()

  ref95 = draws %>%
    group_by(ff) %>%
    summarise(
      .lower = as.vector(quantile(tau, .025)),
      .upper = as.vector(quantile(tau, .975)),
      tau = mean(tau),
      .width = .95,
      .point = "mean",
      .interval = "qi"
    ) %>%
    select(ff, tau, .lower, .upper, .width, .point, .interval)

  ref50 = draws %>%
    group_by(ff) %>%
    summarise(
      .lower = as.vector(quantile(tau, .25)),
      .upper = as.vector(quantile(tau, .75)),
      tau = mean(tau),
      .width = .5,
      .point = "mean",
      .interval = "qi"
    ) %>%
    select(ff, tau, .lower, .upper, .width, .point, .interval)

  ref = bind_rows(ref50, ref95)

  result = draws %>%
    group_by(ff) %>%
    mean_qi(tau, .width = c(.5, .95))

  result_list = draws %>%
    group_by(ff) %>%
    summarise_at("tau", list) %>%
    mean_qi(tau, .width = c(.5, .95))

  expect_equal(as.data.frame(result), as.data.frame(ref))
  expect_equal(as.data.frame(result_list), as.data.frame(ref))
})

test_that("mean_qi works on multiple probs with multiple vars", {
  draws = get_draws() %>%
    mutate(tau2 = tau * 2)

  ref95 = draws %>%
    group_by(ff) %>%
    summarise(
      tau.lower = as.vector(quantile(tau, .025)),
      tau.upper = as.vector(quantile(tau, .975)),
      tau = mean(tau),
      tau2.lower = as.vector(quantile(tau2, .025)),
      tau2.upper = as.vector(quantile(tau2, .975)),
      tau2 = mean(tau2),
      .width = .95,
      .point = "mean",
      .interval = "qi"
    ) %>%
    select(ff, tau, tau.lower, tau.upper, tau2, tau2.lower, tau2.upper, .width, .point, .interval)

  ref50 = draws %>%
    group_by(ff) %>%
    summarise(
      tau.lower = as.vector(quantile(tau, .25)),
      tau.upper = as.vector(quantile(tau, .75)),
      tau = mean(tau),
      tau2.lower = as.vector(quantile(tau2, .25)),
      tau2.upper = as.vector(quantile(tau2, .75)),
      tau2 = mean(tau2),
      .width = .50,
      .point = "mean",
      .interval = "qi"
    ) %>%
    select(ff, tau, tau.lower, tau.upper, tau2, tau2.lower, tau2.upper, .width, .point, .interval)

  ref = bind_rows(ref50, ref95)

  result = draws %>%
    group_by(ff) %>%
    mean_qi(tau, tau2, .width = c(.5, .95))

  result_list = draws %>%
    group_by(ff) %>%
    summarise_at(c("tau", "tau2"), list) %>%
    mean_qi(tau, tau2, .width = c(.5, .95))

  expect_equal(as.data.frame(result), as.data.frame(ref))
  expect_equal(as.data.frame(result_list), as.data.frame(ref))
})

test_that("mean_qi correctly identifies the desired columns when ... is empty", {
  testdf = tibble(
    .chain = 1:1000,
    .iteration = 1:1000,
    .draw = 1:1000,
    .row = 1:1000,
    .x = c(qnorm(ppoints(500)), qnorm(ppoints(500), 1)),
    y = c(qnorm(ppoints(500), 2), qnorm(ppoints(500), 3)),
    g = c(rep("a", 500), rep("b", 500))
  ) %>%
    group_by(g)

  expect_equal(mean_qi(testdf, .x, y), mean_qi(testdf))
})

test_that("multiple-response intervals work", {
  set.seed(1234)
  dd = tibble(
    x = c(rnorm(1000), rnorm(1000, mean = 5))
  )

  ref = dd %>%
    summarise(
      .lower = list(hdi(x, .width = .5)[, 1]),
      .upper = list(hdi(x, .width = .5)[, 2]),
      x = Mode(x),
      .width = .5,
      .point = "mode",
      .interval = "hdi"
    ) %>%
    unnest(c(.lower, .upper)) %>%
    select(x, everything())

  expect_equal(mode_hdi(dd, x, .width = .5), ref)
})

test_that("point_interval errors if there are no columns to summarise", {
  expect_error(median_hdi(data.frame()),
    "No columns found to calculate point and interval summaries for\\.")
})

test_that("point_interval works on vectors", {
  set.seed(1234)
  x = rnorm(100, mean = 5)

  ref = data.frame(
    y = mean(x),
    ymin = as.vector(quantile(x, probs = .025)),
    ymax = as.vector(quantile(x, probs = .975)),
    .width = .95,
    .point = "mean",
    .interval = "qi",
    stringsAsFactors = FALSE
  )

  expect_equal(mean_qi(x), ref)
})

test_that("various point summaries and intervals give correct numbers", {
  expect_equal(
    median_hdci(c(0:6, 1:5, 2:4, 2), .width = .6),
    data.frame(y = 3, ymin = 2, ymax = 4, .width = 0.6, .point = "median", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_qi(c(0:6, 1:5, 2:4, 2), .width = .6),
    data.frame(y = 2.9375, ymin = 2, ymax = 4, .width = 0.6, .point = "mean", .interval = "qi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdi(c(0:6, 1:5, 2:4, 2), .width = .6, .simple_names = TRUE),
    data.frame(.value = 2, .lower = 2, .upper = 4, .width = 0.6, .point = "mode", .interval = "hdi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdci(c(0:6, 1:5, 2:4, 2), .width = .6, .simple_names = TRUE),
    data.frame(.value = 2, .lower = 2, .upper = 4, .width = 0.6, .point = "mode", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_hdci(c(0:6, 1:5, 2:4, 2), .width = .6),
    data.frame(y = 2.9375, ymin = 2, ymax = 4, .width = 0.6, .point = "mean", .interval = "hdci", stringsAsFactors = FALSE)
  )

})

test_that("attempting to use hdi with multiple multimodal columns simultaneously fails", {
  expect_error(
    mode_hdi(data.frame(x = c(1:5, 1, 5), y = c(1:5, 1, 5)), .width = .2),
    "You are summarizing a multimodal distribution using a method that returns\nmultiple intervals"
  )
})

test_that("NAs are handled correctly in point_interval", {
  expect_equal(
    median_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = TRUE),
    data.frame(y = 3, ymin = 2, ymax = 4, .width = 0.6, .point = "median", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_qi(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = TRUE),
    data.frame(y = 2.9375, ymin = 2, ymax = 4, .width = 0.6, .point = "mean", .interval = "qi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdi(c(0:6, 1:5, 2:4, 2, NA), .width = .6, .simple_names = TRUE, na.rm = TRUE),
    data.frame(.value = 2, .lower = 2, .upper = 4, .width = 0.6, .point = "mode", .interval = "hdi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, .simple_names = TRUE, na.rm = TRUE),
    data.frame(.value = 2, .lower = 2, .upper = 4, .width = 0.6, .point = "mode", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = TRUE),
    data.frame(y = 2.9375, ymin = 2, ymax = 4, .width = 0.6, .point = "mean", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    median_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = FALSE),
    data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_, .width = 0.6, .point = "median", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_qi(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = FALSE),
    data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_, .width = 0.6, .point = "mean", .interval = "qi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdi(c(0:6, 1:5, 2:4, 2, NA), .width = .6, .simple_names = TRUE, na.rm = FALSE),
    data.frame(.value = NA_real_, .lower = NA_real_, .upper = NA_real_, .width = 0.6, .point = "mode", .interval = "hdi", stringsAsFactors = FALSE)
  )

  expect_equal(
    mode_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, .simple_names = TRUE, na.rm = FALSE),
    data.frame(.value = NA_real_, .lower = NA_real_, .upper = NA_real_, .width = 0.6, .point = "mode", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_hdci(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = FALSE),
    data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_, .width = 0.6, .point = "mean", .interval = "hdci", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_ll(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = FALSE),
    data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_, .width = 0.6, .point = "mean", .interval = "ll", stringsAsFactors = FALSE)
  )

  expect_equal(
    mean_ul(c(0:6, 1:5, 2:4, 2, NA), .width = .6, na.rm = FALSE),
    data.frame(y = NA_real_, ymin = NA_real_, ymax = NA_real_, .width = 0.6, .point = "mean", .interval = "ul", stringsAsFactors = FALSE)
  )

})

test_that("automatic partial evaluation works", {
  expect_equal(point_interval(.point = mean)(1:10), point_interval(1:10, .point = mean))
})


# upper/lower limits (ul/ll) ----------------------------------------------

test_that("ll and ul work", {
  df = data.frame(x = ppoints(100, a = 1))

  ref = tibble(x = 0.5, .lower = c(0.25, 0), .upper = 1, .width = c(0.75, 1), .point = "mean", .interval = "ll")

  expect_equal(mean_ll(df, x, .width = c(.75, 1)), ref)
  expect_equal(median_ll(df, x, .width = c(.75, 1)), mutate(ref, .point = "median"))
  expect_equal(mode_ll(df, x, .width = c(.75, 1)) %>% mutate(x = round(x, 2)), mutate(ref, .point = "mode"))

  ref = tibble(x = 0.5, .lower = 0, .upper = c(0.75, 1), .width = c(0.75, 1), .point = "mean", .interval = "ul")

  expect_equal(mean_ul(df, x, .width = c(.75, 1)), ref)
  expect_equal(median_ul(df, x, .width = c(.75, 1)), mutate(ref, .point = "median"))
  expect_equal(mode_ul(df, x, .width = c(.75, 1)) %>% mutate(x = round(x, 2)), mutate(ref, .point = "mode"))
})

# rvars -------------------------------------------------

test_that("pointintervals work on rvars", {
  skip_if_not_installed("posterior")

  x = c(posterior::rvar(c(0:6, 1:5, 2:4, 2)), posterior::rvar(c(0:6, 1:5, 2:4, 2) + 2))

  expect_equal(
    median_qi(x, .width = 0.6),
    tibble(.value = c(3,5), .lower = c(2,4), .upper = c(4,6), .width = 0.6, .point = "median", .interval = "qi")
  )
  expect_equal(
    mean_hdi(tibble(x), .width = 0.6),
    tibble(x = c(2.9375,4.9375), .lower = c(2,4), .upper = c(4,6), .width = 0.6, .point = "mean", .interval = "hdi")
  )
  expect_equal(
    mode_hdci(tibble(x), .width = 0.6),
    tibble(x = c(2,4), .lower = c(2,4), .upper = c(4,6), .width = 0.6, .point = "mode", .interval = "hdci")
  )
})

test_that("non-scalar rvars throw appropriate warnings", {
  skip_if_not_installed("posterior")

  x = posterior::rvar(matrix(1:6, nrow = 2))
  expect_error(hdi(x), "HDI for non-scalar rvars is not implemented")
  expect_error(hdci(x), "HDCI for non-scalar rvars is not implemented")
})

test_that("point_interval works on NA rvars", {
  skip_if_not_installed("posterior")

  ref = tibble(
    .value = NA_real_,
    .lower = NA_real_,
    .upper = NA_real_,
    .width = 0.95
  )

  x = posterior::rvar(NA_real_)
  expect_equal(median_qi(x), mutate(ref, .point = "median", .interval = "qi"))
  expect_equal(mean_hdi(x), mutate(ref, .point = "mean", .interval = "hdi"))
  expect_equal(mode_hdci(x), mutate(ref, .point = "mode", .interval = "hdci"))
})

test_that("multivariate rvars work", {
  skip_if_not_installed("posterior")

  x = c(
    posterior::rvar(c(qnorm(ppoints(50)), qnorm(ppoints(50), 5))),
    posterior::rvar(c(qnorm(ppoints(100), 3))),
    posterior::rvar(c(qnorm(ppoints(100), 2))),
    posterior::rvar(c(qnorm(ppoints(100), 4)))
  )
  dim(x) = c(2,2)

  df = tibble(g = c("a", "b"), x = x)


  # build qi ref
  qis_50 = rbind(
    qi(posterior::draws_of(x[1,1]), .width = .5),
    qi(posterior::draws_of(x[1,2]), .width = .5),
    qi(posterior::draws_of(x[2,1]), .width = .5),
    qi(posterior::draws_of(x[2,2]), .width = .5)
  )
  qis_90 = rbind(
    qi(posterior::draws_of(x[1,1]), .width = .9),
    qi(posterior::draws_of(x[1,2]), .width = .9),
    qi(posterior::draws_of(x[2,1]), .width = .9),
    qi(posterior::draws_of(x[2,2]), .width = .9)
  )

  expect_equal(dim(qis_50), c(4,2))
  expect_equal(dim(qis_90), c(4,2))

  ref = tibble(
    g = rep(c("a", "a", "b", "b"), 2),
    x = rep(c(2.5, 2, 3, 4), 2),
    .index = rep(1:2, 4),
    .lower = c(qis_50[,1], qis_90[,1]),
    .upper = c(qis_50[,2], qis_90[,2]),
    .width = rep(c(0.5, 0.9), each = 4),
    .point = "median",
    .interval = "qi"
  )

  expect_equal(median_qi(df, x, .width = c(.5, .9)), ref)


  # build hdi ref
  hdis_50 = rbind(
    hdi(posterior::draws_of(x[1,1]), .width = .5),
    hdi(posterior::draws_of(x[1,2]), .width = .5),
    hdi(posterior::draws_of(x[2,1]), .width = .5),
    hdi(posterior::draws_of(x[2,2]), .width = .5)
  )
  hdis_90 = rbind(
    hdi(posterior::draws_of(x[1,1]), .width = .9),
    hdi(posterior::draws_of(x[1,2]), .width = .9),
    hdi(posterior::draws_of(x[2,1]), .width = .9),
    hdi(posterior::draws_of(x[2,2]), .width = .9)
  )

  expect_equal(dim(hdis_50), c(5,2))
  expect_equal(dim(hdis_90), c(5,2))

  ref = tibble(
    g = rep(c("a", "a", "a", "b", "b"), 2),
    x = rep(c(2.5, 2.5, 2, 3, 4), 2),
    .index = rep(c(1,1,2,1,2), 2),
    .lower = c(hdis_50[,1], hdis_90[,1]),
    .upper = c(hdis_50[,2], hdis_90[,2]),
    .width = rep(c(0.5, 0.9), each = 5),
    .point = "median",
    .interval = "hdi"
  )

  expect_equal(median_hdi(df, x, .width = c(.5, .9)), ref)


  # > 2 dims
  x_draws = array(c(
    rep(qi(ppoints(100, a = 1)), 12) + rep(1:12, each = 100)
  ), dim = c(100, 2, 3, 2))
  df = tibble(i = 1:2, x = posterior::rvar(x_draws))

  .index = paste0(rep(1:3, 4), ",", rep(1:2, each = 3, times = 2))
  .index = ordered(.index, levels = paste0(rep(1:3, 2), ",", rep(1:2, each = 3)))
  .x = c(seq(1, 11, by = 2), seq(2, 12, by = 2))
  ref = tibble(
    i = rep(1:2, each = 6),
    x = .x + 0.5,
    .index = .index,
    .lower = .x + .025,
    .upper = .x + .975,
    .width = .95,
    .point = "median",
    .interval = "qi"
  )

  expect_equal(median_qi(df, x), ref)
})


# distributional objects --------------------------------------------------

test_that("pointintervals work on distributional objects", {
  x = dist_gamma(1:2,2:3)

  expect_equal(
    median_qi(x, .width = 0.6),
    tibble(.value = qgamma(0.5, 1:2, 2:3), .lower = qgamma(0.2, 1:2, 2:3), .upper = qgamma(0.8, 1:2, 2:3), .width = 0.6, .point = "median", .interval = "qi")
  )
  expect_equal(
    mean_qi(tibble(x), .width = 0.6),
    tibble(x = 1:2 / 2:3, .lower = qgamma(0.2, 1:2, 2:3), .upper = qgamma(0.8, 1:2, 2:3), .width = 0.6, .point = "mean", .interval = "qi")
  )
  expect_equal(
    mode_qi(tibble(x), .width = 0.6),
    tibble(x = 0:1 / 2:3, .lower = qgamma(0.2, 1:2, 2:3), .upper = qgamma(0.8, 1:2, 2:3), .width = 0.6, .point = "mode", .interval = "qi"),
    tolerance = 1e-05
  )
})

test_that("Mode on dist_sample uses the numeric method", {
  x_values = dgamma(ppoints(100), 2, 2)
  x = dist_sample(list(x_values))

  expect_equal(Mode(x), Mode(x_values))
})

test_that("Mode on discrete distributions works", {
  x = c(dist_poisson(3.5), dist_binomial(10, 0.4))

  expect_equal(Mode(x), c(3, 4))
})

test_that("non-scalar distributions throw appropriate warnings", {
  x = dist_normal(0:1)
  expect_error(hdi(x), "HDI for non-scalar distribution objects is not implemented")
  expect_error(hdci(x), "HDCI for non-scalar distribution objects is not implemented")
})

test_that("multivariate distributions throw appropriate warnings", {
  skip_if_not_installed("mvtnorm")  # needed for dist_multivariate_normal()

  x = dist_multivariate_normal(list(0:1), list(diag(2)))
  expect_error(hdi(x), "HDI for multivariate distribution objects is not implemented")
  expect_error(hdci(x), "HDCI for multivariate distribution objects is not implemented")
})

test_that("point_interval works on NA dists", {
  ref = tibble(
    .value = NA_real_,
    .lower = NA_real_,
    .upper = NA_real_,
    .width = 0.95
  )

  x = dist_missing()
  expect_equal(median_qi(x), mutate(ref, .point = "median", .interval = "qi"))
  expect_equal(mean_hdi(x), mutate(ref, .point = "mean", .interval = "hdi"))
  expect_equal(mode_hdci(x), mutate(ref, .point = "mode", .interval = "hdci"))
})

test_that("multivariate distributions work", {
  skip_if_not_installed("mvtnorm")  # needed for dist_multivariate_normal()

  x = c(dist_multivariate_normal(list(1:3), list(diag(3))), dist_normal(0,0.5))

  df = tibble(g = c("a", "b"), x = x)

  ref = tibble(
    g = rep(c("a", "a", "a", "b"), 2),
    x = rep(c(1, 2, 3, 0), 2),
    .index = rep(c(1, 2, 3, NA), 2),
    .lower = c(
      qnorm(0.25, c(1, 2, 3, 0), c(1, 1, 1, 0.5)),
      qnorm(0.05, c(1, 2, 3, 0), c(1, 1, 1, 0.5))
    ),
    .upper = c(
      qnorm(0.75, c(1, 2, 3, 0), c(1, 1, 1, 0.5)),
      qnorm(0.95, c(1, 2, 3, 0), c(1, 1, 1, 0.5))
    ),
    .width = rep(c(0.5, 0.9), each = 4),
    .point = "median",
    .interval = "qi"
  )

  expect_equal(median_qi(df, x, .width = c(.5, .9)), ref)
})

test_that("flattened indices retain index order", {
  skip_if_no_vdiffr()
  skip_if_not_installed("mvtnorm")  # needed for dist_multivariate_normal()

  vdiffr::expect_doppelganger("flattened indices with geom_pointinterval",
    tibble(x = dist_multivariate_normal(list(c(1:10)), list(diag(10)))) %>%
      median_qi(x, .width = c(.66, .95)) %>%
      ggplot(aes(x, xmin = .lower, xmax = .upper, y = .index)) +
      geom_pointinterval()
  )

  vdiffr::expect_doppelganger("flattened indices with stat_pointinterval",
    tibble(
      x = c(
        dist_multivariate_normal(list(c(1:10)), list(diag(10))),
        dist_normal()
      ),
      y = c("a","b")
    )  %>%
    ggplot(aes(xdist = x, y = y, group = after_stat(.index))) +
    stat_pointinterval(position = "dodge")
  )

})


# 100% intervals ----------------------------------------------------------

test_that("100% intervals work on sample data", {
  x = seq(0, 1, length.out = 10)

  ref = data.frame(
    .value = 0.5,
    .lower = 0,
    .upper = 1,
    .width = 1,
    .point = "median",
    .interval = "qi",
    stringsAsFactors = FALSE
  )

  expect_equal(median_qi(x, .width = 1, .simple_names = TRUE), ref)
  expect_equal(median_hdi(x, .width = 1, .simple_names = TRUE), mutate(ref, .interval = "hdi"))
  expect_equal(median_hdci(x, .width = 1, .simple_names = TRUE), mutate(ref, .interval = "hdci"))
  expect_equal(median_ll(x, .width = 1, .simple_names = TRUE), mutate(ref, .interval = "ll"))
  expect_equal(median_ul(x, .width = 1, .simple_names = TRUE), mutate(ref, .interval = "ul"))
})

test_that("100% intervals work on distributions", {
  x = dist_exponential(1)

  ref = tibble(
    .value = 1,
    .lower = 0,
    .upper = Inf,
    .width = 1,
    .point = "mean",
    .interval = "qi"
  )

  expect_equal(mean_qi(x, .width = 1), ref)
  expect_equal(mean_hdi(x, .width = 1), mutate(ref, .interval = "hdi"))
  expect_equal(mean_hdci(x, .width = 1), mutate(ref, .interval = "hdci"))
  expect_equal(mean_ll(x, .width = 1), mutate(ref, .interval = "ll"))
  expect_equal(mean_ul(x, .width = 1), mutate(ref, .interval = "ul"))
})

test_that("100% intervals work on rvars", {
  skip_if_not_installed("posterior")

  x = posterior::rvar(seq(0, 1, length.out = 10))

  ref = tibble(
    .value = 0.5,
    .lower = 0,
    .upper = 1,
    .width = 1,
    .point = "median",
    .interval = "qi"
  )

  expect_equal(median_qi(x, .width = 1), ref)
  expect_equal(median_hdi(x, .width = 1), mutate(ref, .interval = "hdi"))
  expect_equal(median_hdci(x, .width = 1), mutate(ref, .interval = "hdci"))
  expect_equal(median_ll(x, .width = 1), mutate(ref, .interval = "ll"))
  expect_equal(median_ul(x, .width = 1), mutate(ref, .interval = "ul"))
})
