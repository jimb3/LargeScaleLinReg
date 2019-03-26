#include <RcppArmadillo.h>

// [[Rcpp::export]]
int XLQR(arma::mat &ql, arma::mat &rtl, arma::mat &xl) {
  if (qr_econ(ql, rtl, xl) == false)
    return 1;
  return 0;
}