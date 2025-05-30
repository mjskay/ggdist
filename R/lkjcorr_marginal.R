# LKJCorr marginal distribution
#
# Author: mjskay
###############################################################################


#' Marginal distribution of a single correlation from an LKJ distribution
#'
#' Marginal distribution for the correlation in a single cell from a correlation
#' matrix distributed according to an LKJ distribution.
#'
#' The LKJ distribution is a distribution over correlation matrices with a single parameter, \eqn{\eta}{eta}.
#' For a given \eqn{\eta}{eta} and a \eqn{K \times K}{KxK} correlation matrix \eqn{R}{R}:
#'
#' \deqn{R \sim \textrm{LKJ}(\eta)}{R ~ LKJ(eta)}
#'
#' Each off-diagonal entry of \eqn{R}{R}, \eqn{r_{ij}: i \ne j}{r[i,j]: i != j}, has the
#' following marginal distribution (Lewandowski, Kurowicka, and Joe 2009):
#'
#' \deqn{\frac{r_{ij} + 1}{2} \sim \textrm{Beta}\left(\eta - 1 + \frac{K}{2}, \eta - 1 + \frac{K}{2}\right)
#' }{(r[i,j] + 1)/2 ~ Beta(eta - 1 + K/2, eta - 1 + K/2)}
#'
#' In other words, \eqn{r_{ij}}{r[i,j]} is marginally distributed according to the above Beta
#' distribution scaled into \eqn{(-1,1)}{(-1,1)}.
#'
#' @inheritParams stats::dnorm
#' @param K <[numeric]> Dimension of the correlation matrix. Must be greater than or equal to 2.
#' @param eta <[numeric]> Parameter controlling the shape of the distribution
#' @return
#' - `dlkjcorr_marginal` gives the density
#' - `plkjcorr_marginal` gives the cumulative distribution function (CDF)
#' - `qlkjcorr_marginal` gives the quantile function (inverse CDF)
#' - `rlkjcorr_marginal` generates random draws.
#'
#' The length of the result is determined by `n` for `rlkjcorr_marginal`, and is the maximum of the lengths of
#' the numerical arguments for the other functions.
#'
#' The numerical arguments other than `n` are recycled to the length of the result. Only the first elements
#' of the logical arguments are used.
#'
#' @references
#'   Lewandowski, D., Kurowicka, D., & Joe, H. (2009). Generating random correlation matrices based on vines
#'   and extended onion method. *Journal of Multivariate Analysis*, 100(9), 1989--2001.
#'   \doi{10.1016/j.jmva.2009.04.008}.
#' @seealso [parse_dist()] and [marginalize_lkjcorr()] for parsing specs that use the
#' LKJ correlation distribution and the [stat_slabinterval()] family of stats for visualizing them.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' theme_set(theme_ggdist())
#'
#' expand.grid(
#'   eta = 1:6,
#'   K = 2:6
#' ) %>%
#'   ggplot(aes(y = ordered(eta), dist = "lkjcorr_marginal", arg1 = K, arg2 = eta)) +
#'   stat_slab() +
#'   facet_grid(~ paste0(K, "x", K)) +
#'   scale_y_discrete(limits = rev) +
#'   labs(
#'     title = paste0(
#'       "Marginal correlation for LKJ(eta) prior on different matrix sizes:\n",
#'       "dlkjcorr_marginal(K, eta)"
#'     ),
#'     subtitle = "Correlation matrix size (KxK)",
#'     y = "eta",
#'     x = "Marginal correlation"
#'   ) +
#'   theme(axis.title = element_text(hjust = 0))
#'
#' @name lkjcorr_marginal
#' @importFrom stats dbeta pbeta qbeta rbeta
#' @export
dlkjcorr_marginal = function(x, K, eta, log = FALSE) {
  alpha = lkjcorr_marginal_alpha(K, eta)
  if (log) {
    dbeta((x + 1)/2, alpha, alpha, log = TRUE) - log(2)
  } else {
    dbeta((x + 1)/2, alpha, alpha) / 2
  }
}

#' @rdname lkjcorr_marginal
#' @export
plkjcorr_marginal = function(q, K, eta, lower.tail = TRUE, log.p = FALSE) {
  alpha = lkjcorr_marginal_alpha(K, eta)
  pbeta((q + 1)/2, alpha, alpha, lower.tail = lower.tail, log.p = log.p)
}

#' @rdname lkjcorr_marginal
#' @export
qlkjcorr_marginal = function(p, K, eta, lower.tail = TRUE, log.p = FALSE) {
  alpha = lkjcorr_marginal_alpha(K, eta)
  qbeta(p, alpha, alpha, lower.tail = lower.tail, log.p = log.p) * 2 - 1
}

#' @rdname lkjcorr_marginal
#' @export
rlkjcorr_marginal = function(n, K, eta) {
  alpha = lkjcorr_marginal_alpha(K, eta)
  rbeta(n, alpha, alpha) * 2 - 1
}

#' @importFrom rlang is_integerish
lkjcorr_marginal_alpha = function(K, eta) {
  if (!is_integerish(K) || K < 2) {
    cli_abort("Correlation matrix dimension (K) must be an integer greater than or equal to 2")
  }
  eta - 1 + K/2
}



# marginalize_lkjcorr -----------------------------------------------------

#' Turn spec for LKJ distribution into spec for marginal LKJ distribution
#'
#' Turns specs for an LKJ correlation matrix distribution as returned by
#' [parse_dist()] into specs for the marginal distribution of
#' a single cell in an LKJ-distributed correlation matrix (i.e., [lkjcorr_marginal()]).
#' Useful for visualizing prior correlations from LKJ distributions.
#'
#' The LKJ(eta) prior on a correlation matrix induces a marginal prior on each correlation
#' in the matrix that depends on both the value of `eta` *and* `K`, the dimension
#' of the \eqn{K \times K}{KxK} correlation matrix. Thus to visualize the marginal prior
#' on the correlations, it is necessary to specify the value of `K`, which depends
#' on what your model specification looks like.
#'
#' Given a data frame representing parsed distribution specifications (such
#' as returned by [parse_dist()]), this function updates any rows with `.dist == "lkjcorr"`
#' so that the first argument to the distribution (stored in `.args`) is equal to the specified dimension
#' of the correlation matrix (`K`), changes the distribution name in `.dist` to `"lkjcorr_marginal"`,
#' and assigns a \pkg{distributional} object representing this distribution to `.dist_obj`.
#' This allows the distribution to be easily visualized using the [stat_slabinterval()]
#' family of ggplot2 stats.
#'
#' @inheritParams lkjcorr_marginal
#' @param data <[data.frame]> A data frame containing a column with distribution names (`".dist"` by default)
#' and a list column of distribution arguments (`".args"` by default), such as output by
#' [parse_dist()].
#' @param predicate <bare [language] | [NULL]> Expression for selecting the rows of `data` to modify.
#' This is useful if `data` contains more than one row with an LKJ prior in it and you only want
#' to modify some of the distributions; if this is the case, give row a predicate expression that
#' evaluates to `TRUE` on the rows you want to modify.
#'
#' If `NULL` (the default), all `lkjcorr` distributions in `data` are modified.
#' @param dist <[string][character]> The name of the column containing distribution names. See [parse_dist()].
#' @param args <[string][character]> The name of the column containing distribution arguments. See [parse_dist()].
#' @param dist_obj <[string][character]> The name of the output column to contain a \pkg{distributional}
#' object representing the distribution. See [parse_dist()].
#' @return
#' A data frame of the same size and column names as the input, with the `dist`, and `args`,
#' and `dist_obj` columns modified on rows where `dist == "lkjcorr"` such that they represent a
#' marginal LKJ correlation distribution with name `lkjcorr_marginal` and `args` having
#' `K` equal to the input value of `K`.
#' @seealso [parse_dist()], [lkjcorr_marginal()]
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' # Say we have an LKJ(3) prior on a 2x2 correlation matrix. We can visualize
#' # its marginal distribution as follows...
#' data.frame(prior = "lkjcorr(3)") %>%
#'   parse_dist(prior) %>%
#'   marginalize_lkjcorr(K = 2) %>%
#'   ggplot(aes(y = prior, xdist = .dist_obj)) +
#'   stat_halfeye() +
#'   xlim(-1, 1) +
#'   xlab("Marginal correlation for LKJ(3) prior on 2x2 correlation matrix")
#'
#' # Say our prior list has multiple LKJ priors on correlation matrices
#' # of different sizes, we can supply a predicate expression to select
#' # only those rows we want to modify
#' data.frame(coef = c("a", "b"), prior = "lkjcorr(3)") %>%
#'   parse_dist(prior) %>%
#'   marginalize_lkjcorr(K = 2, coef == "a") %>%
#'   marginalize_lkjcorr(K = 4, coef == "b")
#'
#' @importFrom rlang quo_get_expr
#' @importFrom distributional dist_wrap
#' @export
marginalize_lkjcorr = function(data, K, predicate = NULL, dist = ".dist", args = ".args", dist_obj = ".dist_obj") {
  li = !is.na(data[[dist]]) & data[[dist]] == "lkjcorr"

  .predicate = enquo(predicate)
  if (!is.null(quo_get_expr(.predicate))) {
    li = li & eval_tidy(.predicate, data)
  }

  li = which(li)
  if (length(li) > 0) {
    data[[args]][li] = lapply(data[[args]][li], function(x) c(list(K), x))
    data[[dist]][li] = "lkjcorr_marginal"
    data[[dist_obj]][li] = list_unchop(lapply(data[[args]][li], function(x) {
      do.call(dist_wrap, c(list(dist = "lkjcorr_marginal", package = "ggdist"), x))
    }))
  }

  data
}
