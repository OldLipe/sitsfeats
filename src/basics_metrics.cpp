#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// global variables
arma::vec P_025 = {0.25};
arma::vec P_050 = {0.50};
arma::vec P_075 = {0.75};

// [[Rcpp::export]]
arma::vec max_ts(const arma::mat& mtx) {

  return arma::max(mtx, 1);
}

// [[Rcpp::export]]
arma::vec min_ts(const arma::mat& mtx) {

  return arma::min(mtx, 1);
}

// [[Rcpp::export]]
arma::vec sum_ts(const arma::mat& mtx) {

  return arma::sum(mtx, 1);
}

// [[Rcpp::export]]
arma::vec mean_ts(const arma::mat& mtx) {

  return arma::mean(mtx, 1);
}

// [[Rcpp::export]]
arma::vec std_ts(const arma::mat& mtx) {

  return arma::stddev(mtx, 0, 1);
}

// [[Rcpp::export]]
arma::vec skew_ts(const arma::mat& mtx) {

  // skewness based on Fisher-Pearson coefficient

  const int n = mtx.n_cols;
  const double expS = 1.5;

  arma::vec m3 = arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 3), 1)/n;
  arma::vec s = arma::pow(arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 2), 1)/n, expS);

  return m3/s;
}

// [[Rcpp::export]]
arma::vec kurt_ts(const arma::mat& mtx) {

  // kurtosis based on pearson’s definition is used (normal ==> 3.0)

  const int n = mtx.n_cols;
  const double expS = 1.5;

  arma::vec m4 = arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 4), 1);
  arma::vec m2 = arma::pow(arma::sum(arma::pow(mtx.each_col()- arma::mean(mtx, 1), 2), 1), 2);

  return n*m4/m2;
}

// [[Rcpp::export]]
arma::vec amplitude_ts(const arma::mat& mtx) {

  return arma::max(mtx, 1) - arma::min(mtx, 1);
}

// [[Rcpp::export]]
arma::vec fslope_ts(const arma::mat& mtx) {

  return arma::max(arma::abs(arma::diff(mtx, 1, 1)), 1);
}

// [[Rcpp::export]]
arma::vec abs_sum_ts(const arma::mat& mtx) {

  return arma::sum(arma::abs(mtx), 1);
}

// [[Rcpp::export]]
arma::vec amd_ts(const arma::mat& mtx) {

  return arma::mean(arma::abs(arma::diff(mtx, 1, 1)), 1);
}

// [[Rcpp::export]]
arma::vec mse_ts(const arma::mat& mtx) {

  arma::mat metrics = mtx.t();

  return arma::mean(arma::pow(arma::abs(arma::trans(arma::fft(metrics))), 2), 1);
}

// [[Rcpp::export]]
arma::vec fqr_ts(const arma::mat& mtx) {

  return arma::quantile(mtx, P_025, 1);
}

// [[Rcpp::export]]
arma::vec tqr_ts(const arma::mat& mtx) {

  return arma::quantile(mtx, P_075, 1);
}

// [[Rcpp::export]]
arma::vec sqr_ts(const arma::mat& mtx) {

  return arma::quantile(mtx, P_050, 1);
}

// [[Rcpp::export]]
arma::vec iqr_ts(const arma::mat& mtx) {

  arma::vec res = tqr_ts(mtx) - fqr_ts(mtx);

  return (res);
}

// [[Rcpp::export]]
arma::mat row_wide_loop(arma::mat& x) {

  // for debug only

  int nrows = x.n_rows;

  for (int i = 0; i < nrows; i++) {
    arma::rowvec v = x.row(i);
    Rcpp::Rcout << "The value is " <<v<< std::endl;

    Rcpp::Rcout << "max " <<arma::max(v)<< std::endl;

    Rcpp::Rcout << "min " <<arma::min(v)<< std::endl;

    Rcpp::Rcout << "mean " <<arma::min(v)<< std::endl;

    Rcpp::Rcout << "std " <<arma::stddev(v)<< std::endl;

    Rcpp::Rcout << "amplitude_ts " << arma::max(v) - arma::min(v)<< std::endl;

    Rcpp::Rcout << "fslope_ts " << arma::max(arma::abs(arma::diff(v))) << std::endl;

    Rcpp::Rcout << "abs_sum_ts " << arma::sum(arma::abs(v)) << std::endl;

    Rcpp::Rcout << "amd_ts " << arma::mean(arma::abs(arma::diff(v))) << std::endl;

    Rcpp::Rcout << "mse_ts " << arma::mean(arma::square(arma::abs(arma::fft(v)))) << std::endl;


    arma::vec P_025 = {0.25};
    arma::vec P_050 = {0.50};
    arma::vec P_075 = {0.75};
    Rcpp::Rcout << "fqr_ts " << arma::quantile(v, P_025) << std::endl;

    Rcpp::Rcout << "tqr_ts " << arma::quantile(v, P_075) << std::endl;

    Rcpp::Rcout << "sqr_ts " << arma::quantile(v, P_050) << std::endl;

    Rcpp::Rcout << "iqr_ts " << arma::quantile(v, P_075) - arma::quantile(v, P_025) << std::endl;
  }
  return x;
}
