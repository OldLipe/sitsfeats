#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// global variables
arma::vec P_025 = {0.25};
arma::vec P_050 = {0.50};
arma::vec P_075 = {0.75};

// [[Rcpp::export]]
arma::vec max_calc(const arma::mat& mtx) {

  return arma::max(mtx, 1);
}

// [[Rcpp::export]]
arma::vec min_calc(const arma::mat& mtx) {

  return arma::min(mtx, 1);
}

// [[Rcpp::export]]
arma::vec mean_calc(const arma::mat& mtx) {

  return arma::mean(mtx, 1);
}

// [[Rcpp::export]]
arma::vec median_calc(const arma::mat& mtx) {

  return arma::median(mtx, 1);
}

// [[Rcpp::export]]
arma::vec sum_calc(const arma::mat& mtx) {

  return arma::sum(mtx, 1);
}

// [[Rcpp::export]]
arma::vec std_calc(const arma::mat& mtx) {

  return arma::stddev(mtx, 0, 1);
}

// [[Rcpp::export]]
arma::vec skew_calc(const arma::mat& mtx) {

  // skewness based on adjusted Fisher-Pearson coefficient

  const int n = mtx.n_cols;
  const double expS = 1.5;

  // adjusted factor
  double adj_factor = sqrt((n*(n-1)))/n-2;

  arma::vec m3 = arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 3), 1)/n;
  arma::vec s = arma::pow(arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 2), 1)/n, expS);

  return (m3/s)*adj_factor;
}

// [[Rcpp::export]]
arma::vec kurt_calc(const arma::mat& mtx) {

  // kurtosis based on pearson’s definition is used (normal ==> 3.0)

  const int n = mtx.n_cols;
  //const double expS = 1.5;

  arma::vec m4 = arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 4), 1);
  arma::vec m2 = arma::pow(arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 2), 1), 2);

  return n*m4/m2;
}

// [[Rcpp::export]]
arma::vec amplitude_calc(const arma::mat& mtx) {

  return arma::max(mtx, 1) - arma::min(mtx, 1);
}

// [[Rcpp::export]]
arma::vec fslope_calc(const arma::mat& mtx) {

  return arma::max(arma::abs(arma::diff(mtx, 1, 1)), 1);
}

// [[Rcpp::export]]
arma::vec abs_sum_calc(const arma::mat& mtx) {

  return arma::sum(arma::abs(mtx), 1);
}

// [[Rcpp::export]]
arma::vec amd_calc(const arma::mat& mtx) {

  return arma::mean(arma::abs(arma::diff(mtx, 1, 1)), 1);
}

// [[Rcpp::export]]
arma::vec mse_calc(const arma::mat& mtx) {

  arma::mat metrics = mtx.t();

  return arma::mean(arma::pow(arma::abs(arma::trans(arma::fft(metrics))), 2), 1);
}

// [[Rcpp::export]]
arma::vec fqr_calc(const arma::mat& mtx) {

  return arma::quantile(mtx, P_025, 1);
}

// [[Rcpp::export]]
arma::vec sqr_calc(const arma::mat& mtx) {

  return arma::quantile(mtx, P_050, 1);
}

// [[Rcpp::export]]
arma::vec tqr_calc(const arma::mat& mtx) {

  return arma::quantile(mtx, P_075, 1);
}

// [[Rcpp::export]]
arma::vec iqr_calc(const arma::mat& mtx) {

  arma::vec res = tqr_calc(mtx) - fqr_calc(mtx);

  return (res);
}
