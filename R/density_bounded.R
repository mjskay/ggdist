# Bounded density estimator
#
# Author: mjskay
###############################################################################



# bounded density estimator -----------------------------------------------

#' Bounded density estimator using Beta kernels
#'
#' Bounded density estimator using Beta kernels, based on Chen (1999), but using
#' a different approach to specifying the bandwidth (see *Bandwidth specification*).
#' Supports [partial function application][partial-functions].
#'
#' @param x Numeric vector containing a sample to compute a density estimate for.
#' @param n The number of equally-spaced points to use as a grid for density estimation.
#' @param bandwidth The bandwidth of the density estimator or a function to compute the
#' bandwidth. The bandwidth is expressed as the desired standard deviation of the Beta
#' kernel when its mean is 0.25 or 0.75 times the range of the
#' `limits`; because the bandwidth is adaptive this implies a slightly larger bandwidth
#' towards the center of the range and a smaller bandwidth towards the boundaries
#' (See *Bandwidth specification* below).
#' This approach allows bandwidths to be specified on the scale of the original data
#' and for existing bandwidth estimators, such as [bw.nrd0()] or [bw.SJ()], to be used.
#' Can be:
#'  - a function that takes a numeric vector containing the sample and returns a bandwidth.
#'  - a string naming such a function.
#'  - a single numeric value giving the desired bandwidth.
#' @param adjust The bandwidth used is actually `bandwidth*adjust`. This makes it easy
#' to specify values like "half the default bandwidth".
#' @param trim Should the result be trimmed to the range of `x` (`trim = TRUE`), or extend
#' all the way to the range of `limits` (`trim = FALSE`)?
#' @param limits Numeric vector of size 2 giving the boundaries of the density estimator.
#' Boundaries can be infinite; infinite boundaries are set at `3*bandwidth` beyond the
#' range of the input data (because `bandwidth` is on a standard deviation scale this
#' should allow the density estimator to go approximately to 0 before hitting the limit).
#' This allows density estimation of data with one finite limit, e.g. by setting
#' `limits = c(0, Inf)`.
#' @param corrected Should the bias-corrected form of Chen's (1999) estimator be used?
#' If `FALSE`, the uncorrected estimator (\eqn{\hat{f}_1}{f_1} in Chen) is used; if
#' `TRUE` the bias-corrected estimator (\eqn{\hat{f}_2}{f_2} in Chen) is used.
#' @param ... Other arguments passed on to other methods (currently ignored).
#'
#' @section Bounded Beta kernels:
#'
#' The kernel density estimate \eqn{\hat(f)(x)} using Beta kernels on a bounded support takes the
#' following general form:
#'
#' \deqn{\hat{f}(x) = \frac{1}{n} \sum_{i=1}^n f_\textrm{Beta}(X_i|\alpha(x),\beta(x))}
#'
#' \eqn{X_1, ..., X_n} is the sample (`x`) and \eqn{b} is
#' a bandwidth parameter derived from `bandwidth` (see *Bandwidth specification* below).
#' We assume the support is \[0,1\]; if not, we scale from `limits` into \[0,1\].
#'
#' Which estimator is used is determined by the functions \eqn{\alpha(x)} and \eqn{\beta(x)}.
#'
#' For `corrected = FALSE` (\eqn{\hat{f}_1}{f_1} in Chen), these are:
#'
#' \deqn{\alpha_1(x) = \frac{x}{b}+1}
#' \deqn{\beta_1(x) = \frac{1 - x}{b}+1}
#'
#' For `corrected = TRUE` (\eqn{\hat{f}_2}{f_2} in Chen), these are:
#'
#' \deqn{\alpha_2(x) = \left\{
#'   \begin{array}{ c l }
#'     \rho(x,b)   & \quad \textrm{if } x < 2b \\
#'     \frac{x}{b} & \quad \textrm{if } x \geq 2b
#'   \end{array}
#' \right.}
#' \deqn{\beta_2(x) = \left\{
#'   \begin{array}{ c l }
#'     \frac{1 - x}{b} & \quad \textrm{if } x \leq 1 - 2b\\
#'     \rho(1 - x,b)   & \quad \textrm{if } x > 1 - 2b
#'   \end{array}
#' \right.}
#'
#' Where:
#'
#' \deqn{\rho(x,b) = 2b^2 + 2.5 - \sqrt{4b^4 + 6b^2 + 2.25 - x^2 - \frac{x}{b}}}
#'
#' In practice, for moderate or large samples this bias correction is small, and for some
#' bandwidths the correction results in a discontinuous density estimate, so
#' the current default is `corrected = FALSE`.
#'
#' @section Bandwidth specification:
#'
#' Unlike the \eqn{b} parameter for bandwidths defined in Chen (1999),
#' `bandwidth` is expressed as the desired standard deviation of the Beta
#' kernel when its mean is 0.25 or 0.75 times the range of
#' the `limits`. This is intended to allow more natural specification of
#' bandwidths on the scale of the original data.
#'
#' Specifically, given a `bandwidth` expressed as a standard deviation \eqn{s},
#' we translate this to the bandwidth parameter \eqn{b} via:
#'
#' \deqn{b = \frac{1}{\frac{3}{16}s^{-2} - 1}}{b = 1/(s^(-2) * 3/16 - 1)}
#'
#' This is derived from the formula for the standard deviation of a Beta
#' distribution with \eqn{\alpha_1(0.25)} and \eqn{\beta_1(0.25)}.
#'
#' @return An object of class `"density"`, mimicking the output format of
#' `stats:density()`, with the following components:
#'
#' \describe{
#'   \item x The grid of points at which the density was estimated.
#'   \item y The estimated density values.
#'   \item bw The `bandwidth`.
#'   \item n The sample size of the `x` input argument.
#'   \item call The call used to produce the result, as a quoted expression.
#'   \item data.name The deparsed name of the `x` input argument.
#'   \item has.na Always `FALSE` (for compatibility).
#' }
#'
#' This allows existing methods (like `print()` and `plot()`) to work if desired.
#' This output format (and in particular, the `x` and `y` components) is also
#' the format expected by the `density` argument of the [stat_slabinterval()].
#'
#' @references
#' Chen, Song Xi. (1999). "Beta kernel estimators for density functions".
#' *Computational Statistics and Data Analysis* 31 (2): 131--145.
#' \doi{10.1016/S0167-9473(99)00010-9}.
#'
#' @examples
#'
#' library(distributional)
#' library(dplyr)
#' library(ggplot2)
#'
#' # For compatibility with existing code, the return type of density_bounded()
#' # is the same as stats::density(), ...
#' set.seed(123)
#' x = rbeta(5000, 1, 3)
#' d = density_bounded(x)
#' d
#'
#' # ... thus, while designed for use with the `density` argument of
#' # stat_slabinterval(), output from density_bounded() can also be used with
#' # base::plot():
#' plot(d)
#'
#' # here we'll use the same data as above, but pick either density_bounded()
#' # or density_unbounded() (which is equivalent to stats::density()). Notice
#' # how the bounded density (green) is biased near the boundary of the support,
#' # while the unbounded density is not.
#' data.frame(x) %>%
#'   ggplot() +
#'   stat_slab(
#'     aes(xdist = dist), data = data.frame(dist = dist_beta(1, 3)),
#'     alpha = 0.25
#'   ) +
#'   stat_slab(aes(x), density = "bounded", fill = NA, color = "#d95f02", alpha = 0.5) +
#'   stat_slab(aes(x), density = "unbounded", fill = NA, color = "#1b9e77", alpha = 0.5) +
#'   theme_ggdist()
#'
#' @importFrom rlang as_label enexpr get_expr
density_bounded = function(
  x,
  n = 512, bandwidth = "bw.nrd0", adjust = 1,
  trim = TRUE,
  limits = c(0, 1), corrected = FALSE,
  ...
) {
  if (missing(x)) return(partial_self("density_bounded"))

  name = as_label(enexpr(x))

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
  bandwidth = bandwidth * adjust

  # scale x into [0,1]
  # infinite limits are placed at 3*bandwidth beyond the data;
  # since bandwidth is on SD scale this should allow the kernel to go back to
  # roughly zero by the time it hits the limit
  limits[limits == -Inf] = min(x) - 3 * bandwidth
  limits[limits == Inf] = max(x) + 3 * bandwidth
  x = (x - limits[[1]])/diff(limits)

  # determine the grid we will evaluate the density estimator over
  at_limits = if (trim) range(x) else c(0, 1)
  at = seq(at_limits[[1]], at_limits[[2]], length.out = n)

  # bandwidth parameter per Chen (1999) is not on the standard deviation scale,
  # so translate sd-scale bandwidth to the parameter used by Chen
  b = sd_to_beta_bandwidth(bandwidth / abs(diff(limits)), at)
  b = rep(b, length.out = length(at))  # ensure b is length of at in case it is a constant

  # determine the alpha and beta parameters of the Beta kernels
  if (corrected) {
    alpha = at/b
    beta = (1 - at)/b
    # bias corrections per Chen (1999)
    alpha_i = at < 2*b
    alpha[alpha_i] = rho_Chen(at[alpha_i], b[alpha_i])
    beta_i = at > 1 - 2*b
    beta[beta_i] = rho_Chen(1 - at[beta_i], b[alpha_i])
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
      # need to apply get_expr over match.call() instead of just using match.call()
      # to remove tildes from the call created by partial application
      call = as.call(lapply(match.call(), get_expr)),
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
sd_to_beta_bandwidth_old = function(s, at = .25) {
  # at x = 0.5 (instead of x = 0.25) this would be:
  # 1/(s^(-2) / 4 - 1)
  # This function has an asymptote at sqrt(at * (1 - at)), above which the bandwidth
  # is so large that it should imply the density should just be flat over the
  # entire domain (i.e. b = Inf), so cap s at sqrt(at * (1 - at)).
  at = .25 # at parameter is currently ignored but may be used for adaptive bandwidth later
  s = min(s, sqrt(at * (1 - at)))
  1/(s^(-2) * at * (1 - at) - 1)
}

# TODO: something adaptive for bandwidth?
sd_to_beta_bandwidth = function(s, at = .5) {
  # this ensures bandwidth at 0.5 is requested sd, and bandwidth at 0 roughly corresponds
  # to bandwidth that would be the requested sd at 0.05 (or 0.95).
  # sd_to_beta_bandwidth_at(s, 0.5)
  sd_to_beta_bandwidth_at = function(s, at) 1/(s^(-2) * at * (1 - at) - 1)
  b_at_0.5 = sd_to_beta_bandwidth_at(s, 0.5)
  b_at_0.1 = sd_to_beta_bandwidth_at(s, 0.1)
  (0.1/(at*(1 - at) + 0.1) - 0.1/0.35) / (1 - 0.1/0.35) * abs(b_at_0.1 - b_at_0.5) + b_at_0.5
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
