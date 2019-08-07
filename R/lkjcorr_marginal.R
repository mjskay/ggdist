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
#' @seealso \code{\link{parse_dist}} for parsing distribution specs and the \code{\link{stat_dist_slabinterval}}
#' family of stats for visualizing them.
#' @examples
#'
#' # TODO
#'
#' @name lkjcorr_marginal
#' @export
dlkjcorr_marginal = function(x, eta, K, log = FALSE) {
  alpha = lkjcorr_marginal_alpha(eta, K)
  if (log) {
    dbeta((x + 1)/2, alpha, alpha, log = TRUE) - log(2)
  } else {
    dbeta((x + 1)/2, alpha, alpha) / 2
  }
}

#' @rdname lkjcorr_marginal
#' @export
plkjcorr_marginal = function(q, eta, K, lower.tail = TRUE, log.p = FALSE) {
  alpha = lkjcorr_marginal_alpha(eta, K)
  if (log.p) {
    pbeta((q + 1)/2, alpha, alpha, lower.tail = lower.tail, log.p = TRUE)
  } else {
    pbeta((q + 1)/2, alpha, alpha, lower.tail = lower.tail)
  }
}

#' @rdname lkjcorr_marginal
#' @export
qlkjcorr_marginal = function(p, eta, K, lower.tail = TRUE, log.p = FALSE) {
  alpha = lkjcorr_marginal_alpha(eta, K)
  qbeta(p, alpha, alpha, lower.tail = lower.tail, log.p = log.p) * 2 - 1
}

#' @rdname lkjcorr_marginal
#' @export
rlkjcorr_marginal = function(n, eta, K) {
  alpha = lkjcorr_marginal_alpha(eta, K)
  rbeta(n, alpha, alpha, lower.tail = lower.tail, log.p = log.p) * 2 - 1
}

#' @importFrom rlang is_integerish
lkjcorr_marginal_alpha = function(eta, K) {
  if (!is_integerish(K) || K < 2) {
    stop("correlation matrix dimension, K, must be an integer greater than or equal to 2")
  }
  eta - 1 + K/2
}
