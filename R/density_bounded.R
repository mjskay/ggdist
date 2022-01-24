# Bounded density estimator
#
# Author: mjskay
###############################################################################



# bounded density estimator -----------------------------------------------

#' Bounded density estimator using beta kernels from Chen (1999)
#' @noRd
density_bounded = function(
  x,
  n = 512, bandwidth = "bw.nrd0", adjust = 1,
  trim = TRUE,
  limits = c(0, 1), corrected = FALSE,
  ...
) {
  if (missing(x)) return(partial_self("density_bounded"))

  name = deparse1(substitute(x))

  if (any(x < limits[[1]] | x > limits[[2]])) {
    stop0(
      "Cannot calculate bounded density:\n",
      "  Some values are outside the specified limits: ", deparse0(limits)
    )
  }

  # calculate bandwidth on data scale
  if (!is.numeric(bandwidth)) {
    bandwidth = match.fun(bandwidth)(x)
  }

  # scale x into [0,1]
  # infinite limits are placed at 3*bandwidth beyond the data;
  # since bandwidth is on SD scale this should allow the kernel to go back to
  # roughly zero by the time it hits the limit
  limits[limits == -Inf] = min(x) - 3 * bandwidth
  limits[limits == Inf] = max(x) + 3 * bandwidth
  x = (x - limits[[1]])/diff(limits)

  # bandwidth parameter per Chen (1999) is not on the standard deviation scale,
  # so translate sd-scale bandwidth to the parameter used by Chen
  b = sd_to_beta_bandwidth(bandwidth / abs(diff(limits)))

  # determine the grid we will evaluate the density estimator over
  at_limits = if (trim) range(x) else c(0, 1)
  at = seq(at_limits[[1]], at_limits[[2]], length.out = n)

  # determine the alpha and beta parameters of the Beta kernels
  if (corrected) {
    alpha = at/b
    beta = (1 - at)/b
    # bias corrections per Chen (1999)
    alpha_i = at < 2*b
    alpha[alpha_i] = rho_Chen(at[alpha_i], b)
    beta_i = at > 1 - 2*b
    beta[beta_i] = rho_Chen(1 - at[beta_i], b)
  } else {
    alpha = at/b + 1
    beta = (1 - at)/b + 1
  }

  # Evaluate the kernels over the grid
  # The "simple" version would be something like:
  #
  #   densities = rowMeans(matrix(dbeta(rep(x, each = n), alpha, beta), nrow = n))
  #
  # However, this would create a very large matrix and be rather slow.
  # So we dance around it a bit. Very likely still could be better.
  densities = mean_beta_densities(x, alpha, beta)
  # Sometimes the approach above fails (usually for 0/1), in which case use the
  # slower / more memory intensive approach just for the NA points
  density_na = is.na(densities)
  if (any(density_na)) {
    n_na = sum(density_na)
    densities[density_na] = rowMeans(matrix(
      dbeta(rep(x, each = n_na), alpha[density_na], beta[density_na]),
      nrow = n_na
    ))
  }

  structure(
    list(
      x = at * diff(limits) + limits[1],
      y = densities / abs(diff(limits)),
      bw = bandwidth,
      n = length(x),
      call = match.call(),
      data.name = name,
      has.na = FALSE
    ),
    class = "density"
  )
}

#' Bias correction function for parameters of the Beta kernel
#' per Chen (1999)
#' @noRd
rho_Chen = function(x, b) {
  2*b^2 + 2.5 - sqrt(4*b^4 + 6*b^2 + 2.25 - x^2 - x/b)
}

#' Convert a bandwidth expressed as the standard deviation of a kernel
#' (as with the base `bw` functions, e.g. [bw.nrd0()], [bw.SJ()], etc)
#' into *b* bandwidth parameters as used in Chen (1999). We do this by
#' determining *b* such that the beta kernel will have the requested
#' standard deviation when centered at roughly 0.15 or 0.85 (i.e. the bandwidth
#' will be a bit more than this in the middle and less towards the edges).
#' @noRd
sd_to_beta_bandwidth = function(s) {
  # 1/(s^(-2) / 4 - 1) #would be at x = 0.5
  # 4 * s^2
  # This function has an asymptote at sqrt(3/16), above which the bandwidth
  # is so large that it implies the density should just be flat over the
  # entire domain (i.e. b = Inf)
  s = min(s, sqrt(3/16))
  1/(s^(-2) * 3/16 - 1)
}

#' Faster version of sapply(seq_len(n), \(i) mean(dbeta(x, alpha[i], beta[i])))
#'
#' We calculate mean(exp(log(dbeta(...)))), pre-calculating log(x) and
#' log(1 - x) terms in the definition of log(dbeta(...)). This yields about a
#' 10x speedup. Use of C++ for the mean doesn't yield a huge speedup but
#' vastly reduces memory pressure by avoiding the creation of many intermediate
#' vectors.
#'
#' A pure R implementation might be:
#'
#'   mean_beta_densities = function(x, alpha, beta) {
#'     log_x = log(x)
#'     log_one_minus_x = log1p(-x)
#'     log_beta_a_b = lbeta(alpha, beta)
#'     n = length(alpha)
#'     vapply(seq_len(n), function(i) {
#'       mean(exp(log_x*(alpha[i] - 1) + log_one_minus_x*(beta[i] - 1) - log_beta_a_b[i]))
#'     }, numeric(1))
#'   }
#'
#' @noRd
mean_beta_densities = function(x, alpha, beta) {
  log_x = log(x)
  log_one_minus_x = log1p(-x)
  log_beta_a_b = lbeta(alpha, beta)
  mean_beta_densities_log(log_x, log_one_minus_x, alpha, beta, log_beta_a_b)
}
