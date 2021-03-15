#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


// [[Rcpp::export]]
arma::mat calculate_vec(const arma::vec& timeseries) {


  arma::mat pts(timeseries.n_rows, 2, arma::fill::zeros);

  //Rcpp::Rcout << timeseries.n_elem << std::endl;
  for (arma::uword i = 0; i < timeseries.n_elem; i++) {
    //double a = timeseries[i] * cos((2 * arma::datum::pi * i) / timeseries.n_elem);
    //double o = timeseries[i] * sin((2 * arma::datum::pi * i) / timeseries.n_elem);

    pts(i,0) = timeseries[i] * cos((2 * arma::datum::pi * i) / timeseries.n_elem);;
    pts(i,1) = timeseries[i] * sin((2 * arma::datum::pi * i) / timeseries.n_elem);;
  }
  return pts;
}
// [[Rcpp::export]]
arma::mat calculate_vec_v2(const arma::mat& timeseries) {


  arma::mat pts(timeseries.n_rows, timeseries.n_cols, arma::fill::zeros);

  //Rcpp::Rcout << timeseries.n_elem << std::endl;
  for (arma::uword i = 0; i < timeseries.n_rows; i++) {
    for (arma::uword c = 0; c < timeseries.n_cols; c++) {
      //double a = timeseries[i] * cos((2 * arma::datum::pi * i) / timeseries.n_elem);
      //double o = timeseries[i] * sin((2 * arma::datum::pi * i) / timeseries.n_elem);

      pts(i,c) = timeseries(i,c) * cos((2 * arma::datum::pi * c) / timeseries.n_cols);;
      pts(i,c) = timeseries(i,c) * sin((2 * arma::datum::pi * c) / timeseries.n_cols);;
    }
  }
  return pts;
}
// [[Rcpp::export]]
Rcpp::List calculate_vec_v3(arma::mat& timeseries) {

  timeseries = arma::abs(timeseries);

  arma::vec x(timeseries.n_cols + 1, arma::fill::zeros);
  arma::vec y(timeseries.n_cols + 1, arma::fill::zeros);
  Rcpp::List pts (timeseries.n_rows);

  for (arma::uword i = 0; i < timeseries.n_rows; i++) {
    for (arma::uword c = 0; c < timeseries.n_cols; c++) {

      x(c) = timeseries(i,c) * cos((2 * arma::datum::pi * c) / timeseries.n_cols);
      y(c) = timeseries(i,c) * sin((2 * arma::datum::pi * c) / timeseries.n_cols);
    }

    // add the first points into last position
    x(timeseries.n_cols) = x(0);
    y(timeseries.n_cols) = y(0);

    pts[i] = arma::join_rows(x,y);
  }
  return pts;
}

// [[Rcpp::export]]
arma::vec calc_distance(const arma::mat& line, const arma::rowvec& pts_cent) {
  //Rcpp::Rcout << line.col(0) << std::endl;
  //Rcpp::Rcout << line.col(1) << std::endl;
  return arma::sqrt(arma::square((line.col(0) - pts_cent.at(0))) +  arma::square((line.col(1) - pts_cent.at(1))));
}

// [[Rcpp::export]]
arma::vec gr_calc(const arma::mat& pts_cent, const arma::mat& pts_line) {

  // vector to store values
  arma::vec pts_values(pts_cent.n_rows, arma::fill::zeros);
  //arma::vec interval();
  for (arma::uword i = 0; i < pts_cent.n_rows; i++) {
    for (arma::uword c = 0; c < pts_line.n_rows; c = c + 24) {
      //arma::mat line = pts_line.rows(c, c + 24).cols(0,1);
      //Rcpp::Rcout << c << c + 25 << std::endl;
      arma::mat line = pts_line.submat( c, 0, c + 24, 1 );
      pts_values(i) = arma::mean(calc_distance(line, pts_cent.row(i)));

      //c = c + 24;
    }
  }
  return pts_values;
}

// // [[Rcpp::depends(grDevices)]]
// // [[Rcpp::export]]
// Rcpp::List integratecpp(double llim, double ulim)
// {
//   Rcpp::Function p_cubature = R_GetCCallable("cubature", "adapt_integrate");
//
//   Rcpp::List result = p_cubature(integrand, llim, ulim);
//   return(result);
// }

// // [[Rcpp::depends(grDevices)]]
// // [[Rcpp::export]]
// arma::vec integratecpp(arma::mat& llim) {
//   Rcpp::Function p_chull = R_GetCCallable("grDevices", "chull");
//
//   arma::vec result = p_chull(llim);
//   return(result);
// }

// // [[Rcpp::export]]
// arma::vec integratecpp(arma::mat& llim) {
//   //Rcpp::Function p_chull = R_GetCCallable("grDevices", "chull");
//
//   arma::vec result = chull(llim);
//   return(result);
// }


// [[Rcpp::export]]
arma::vec teste_linspace(const arma::mat& r, const arma::uword len) {
  return arma::linspace<arma::vec>(0,r.n_rows, len);
}

// [[Rcpp::export]]
arma::vec linspace_vec(const arma::rowvec& timeseries) {

  return arma::linspace<arma::vec>(0, 2*arma::datum::pi, timeseries.n_elem);
}

// // [[Rcpp::export]]
// arma::vec linspace_vec_2(const arma::vec& timeseries) {
//
//   return arma::linspace<arma::vec>(0, 2*arma::datum::pi, timeseries.n_elem);
// }

// [[Rcpp::export]]
arma::vec calc_angle(const arma::mat& timeseries) {

  arma::vec pts(timeseries.n_rows, arma::fill::zeros);

  for (arma::uword i = 0; i < timeseries.n_rows; i++) {
    pts(i) = linspace_vec(timeseries.row(i)).at(arma::index_max(timeseries.row(i)));
  }
  return pts;
}


// [[Rcpp::export]]
arma::vec calc_csi(const arma::vec& line_length, const arma::vec& poly_area){
  return arma::square(line_length)/(4 * arma::datum::pi * poly_area);
}
