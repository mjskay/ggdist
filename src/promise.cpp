#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP find_var_(Symbol name, Environment env) {
  return Rf_findVar(name, env);
}

// [[Rcpp::export]]
Promise find_promise_(Symbol name, Environment env) {
  Shield<SEXP> var = Rf_findVar(name, env);
  Promise promise(var);
  return promise;
}

// [[Rcpp::export]]
SEXP promise_expr_(Promise promise) {
  return PRCODE(promise);
}

// [[Rcpp::export]]
SEXP promise_env_(Promise promise) {
  return PRENV(promise);
}

// [[Rcpp::export]]
List dots_to_list_(DottedPair dots) {
  List list(dots.size());
  for (int i = 0; i < dots.size(); i++) {
    list[i] = dots[i];
  }
  list.names() = dots.attr("names");
  return list;
}
