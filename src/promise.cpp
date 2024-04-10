#include <Rcpp.h>
using namespace Rcpp;

/**
 * Unwrap an SEXP that may be a promise to a promise to a promise (etc) into a
 * single promise. Non-promises are left as-is.
 * @param x an SEXP that might be a promise
 * @returns
 * - If `x` is a promise: a promise whose code is not a promise
 * - If `x` is not a promise: `x`
 */
// [[Rcpp::export]]
SEXP unwrap_promise_(SEXP x) {
  SEXP expr = x;
  while (TYPEOF(expr) == PROMSXP) {
    x = expr;
    expr = PRCODE(x);
  }
  return x;
}

/**
 * Find a promise by name
 * @param name name of a variable
 * @param env environment to search
 * @returns an unwrapped promise (if `name` refers to a promise) or an object
 * (if `name` does not refer to a promise).
 */
// [[Rcpp::export]]
SEXP find_promise_(Symbol name, Environment env) {
  return unwrap_promise_(Rf_findVar(name, env));
}

// [[Rcpp::export]]
SEXP promise_expr_(Promise promise) {
  return PRCODE(unwrap_promise_(promise));
}

// [[Rcpp::export]]
SEXP promise_env_(Promise promise) {
  return PRENV(unwrap_promise_(promise));
}

// identical(x, quote(waiver()))
bool is_waiver_call(SEXP x) {
  if (TYPEOF(x) == LANGSXP) {
    Language call = x;
    if (call.size() == 1 && TYPEOF(call[0]) == SYMSXP) {
      Symbol symbol = call[0];
      return symbol == "waiver";
    }
  }

  return false;
}

// [[Rcpp::export]]
bool is_waived_(SEXP x) {
  if (TYPEOF(x) != PROMSXP) {
    return Rf_inherits(x, "waiver");
  }

  //TODO: fix this so we can use it instead of the R implementation
  // the problem is bytecode (I think...); need to fix promise_expr
  x = unwrap_promise_(x);
  Shield<SEXP> expr = PRCODE(x);

  if (is_waiver_call(expr)) return true;

  if (TYPEOF(expr) == SYMSXP) {
    Shield<SEXP> var = Rf_eval(expr, PRENV(x));
    return is_waived_(var);
  }

  return false;
}

/**
 * Convert a dotted pairlist to a `list()`
 * @param dots a dotted pair list as passed to an R function via `...`
 * @returns the values of `dots` converted to a list of promises. Nested
 * promises are unwrapped so that they are only a single promise.
 */
// [[Rcpp::export]]
List dots_to_list_(DottedPair dots) {
  int n = dots.size();
  List list(n);

  list.names() = dots.attr("names");

  for (int i = 0; i < n; i++) {
    list[i] = unwrap_promise_(dots[0]);
    dots.remove(0);
  }

  return list;
}
