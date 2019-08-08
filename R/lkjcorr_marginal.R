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
#' \deqn{\frac{r_{ij} + 1}{2} \sim \textrm{Beta}(\eta - 1 + K/2, \eta - 1 + K/2)
#' }{(r[i,j] + 1)/2 ~ Beta(eta - 1 + K/2, eta - 1 + K/2)}
#'
#' In other words, \eqn{r_{ij}}{r[i,j]} is marginally distributed according to the above Beta
#' distribution scaled into \eqn{(-1,1)}{(-1,1)}.
#'
#' @inheritParams stats::dnorm
#' @param eta Parameter controlling the shape of the distribution
#' @param K Dimension of the correlation matrix. Must be greater than or equal to 2.
#' @references
#'   Lewandowski, D., Kurowicka, D., & Joe, H. (2009). Generating random correlation matrices based on vines
#'   and extended onion method. \emph{Journal of Multivariate Analysis}, 100(9), 1989--2001.
#'   \doi{10.1016/j.jmva.2009.04.008}.
#' @seealso \code{\link{parse_dist}} and \code{\link{marginalize_lkjcorr}} for parsing specs that use the
#' LKJ correlation distribution and the \code{\link{stat_dist_slabinterval}} family of stats for visualizing them.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' data.frame(prior = "lkjcorr_marginal(2, 3)") %>%
#'   parse_dist(prior) %>%
#'   ggplot(aes(y = prior, dist = .dist, args = .args)) +
#'   stat_dist_halfeyeh() +
#'   xlim(-1, 1) +
#'   xlab("Marginal correlation for LKJ(3) prior on 2x2 correlation matrix")
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
    stop("Correlation matrix dimension (K) must be an integer greater than or equal to 2")
  }
  eta - 1 + K/2
}



# marginalize_lkjcorr -----------------------------------------------------

#' Turn spec for LKJ distribution into spec for marginal LKJ distribution
#'
#' Turns specs for an LKJ correlation matrix distribution as returned by
#' \code{\link{parse_dist}} into specs for the marginal distribution of
#' a single cell in an LKJ-distributed correlation matrix (i.e., \code{\link{lkjcorr_marginal}}).
#'
#' Given a data frame representing parsed distribution specifications (such
#' as returned by \code{\link{parse_dist}}), updates any rows with \code{.dist == "lkjcorr"}
#' so that the first argument to the distribution is equal to the dimension
#' of the matrix (\code{K}) and changes the distribution name to \code{"lkjcorr_marginal"},
#' allowing the distribution to be easily visualized using the \code{\link{stat_dist_slabinterval}}
#' family of ggplot2 stats.
#'
#' @inheritParams lkjcorr_marginal
#' @param data A data frame containing a column with distribution names (\code{".dist"} by default)
#' and a list column of distribution arguments (\code{".args"} by default), such as output by
#' \code{\link{parse_dist}}.
#' @param dist The name of the column containing distribution names. See \code{\link{parse_dist}}.
#' @param args The name of the column containing distribution arguments. See \code{\link{parse_dist}}.
#' @seealso \code{\link{parse_dist}}, \code{\link{lkjcorr_marginal}}
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' data.frame(prior = "lkjcorr(3)") %>%
#'   parse_dist(prior) %>%
#'   marginalize_lkjcorr(K = 2) %>%
#'   ggplot(aes(y = prior, dist = .dist, args = .args)) +
#'   stat_dist_halfeyeh() +
#'   xlim(-1, 1) +
#'   xlab("Marginal correlation for LKJ(3) prior on 2x2 correlation matrix")
#'
#' @export
marginalize_lkjcorr = function(data, K, dist = ".dist", args = ".args") {
  li = !is.na(data[[dist]]) & data[[dist]] == "lkjcorr"
  data[[args]][li] = lapply(data[[args]][li], function(x) c(list(K), x))
  data[[dist]][li] = "lkjcorr_marginal"
  data
}
