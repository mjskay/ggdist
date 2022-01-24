#include <cpp11.hpp>
using namespace cpp11;

/**
 * Faster / lower-memory version of sapply(seq_len(n), \(i) mean(dbeta(x, alpha[i], beta[i])))
 *
 * Uses pre-calculated log terms and avoids intermediate vectors in calculating
 * means to speed up the calculation and reduce memory pressure.
 *
 * @param log_xs log(x)
 * @param log_one_minus_xs log(1 - x)
 * @param as,bs alpha and beta (must be same length)
 * @param log_beta_a_bs lbeta(alpha, beta)
 *
 * @return Numeric vector of same length as `as` and `bs` where the entry at `i` is
 * the mean of dbeta(x, as[i], bs[i]) for all xs.
 */
[[cpp11::register]]
doubles mean_beta_densities_log(
  doubles log_xs, doubles log_one_minus_xs, doubles as, doubles bs, doubles log_beta_a_bs
) {
  int n_x = log_xs.size();
  int n_at = as.size();
  double log_n = log(n_x);
  writable::doubles mean_dbetas(n_at);

  for (int at_i = 0; at_i < n_at; ++at_i) {
    mean_dbetas[at_i] = 0;
    for (int x_i = 0; x_i < n_x; ++x_i) {
      mean_dbetas[at_i] += exp(
        log_xs[x_i]*(as[at_i] - 1) + log_one_minus_xs[x_i]*(bs[at_i] - 1) - log_beta_a_bs[at_i] - log_n
      );
    }
  }

  return mean_dbetas;
}
