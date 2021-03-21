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
arma::vec reptest(arma::uword x, arma::uword time) {
  //int n = y.size();
  arma::vec myvector(time, arma::fill::zeros);
  for (int i = 0; i < time; ++i) {
    myvector(i) = x;
  }
  return myvector;
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
Rcpp::List calculate_vec_v3_id(arma::mat& timeseries) {

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

    pts[i] = arma::join_rows(x,y, reptest(i, timeseries.n_cols + 1));
  }
  return pts;
}

// // [[Rcpp::export]]
// arma::mat calculate_vec_v4(arma::mat& timeseries) {
//
//   timeseries = arma::abs(timeseries);
//
//   arma::vec x(timeseries.n_cols + 1, arma::fill::zeros);
//   arma::vec y(timeseries.n_cols + 1, arma::fill::zeros);
//   arma::mat xy(timeseries.n_cols + 1, 3, arma::fill::zeros);
//   arma::mat pts (timeseries.n_rows*timeseries.n_cols, 3, arma::fill::zeros);
//
//   for (arma::uword k = 0; k < (timeseries.n_rows*timeseries.n_cols) - timeseries.n_cols; k = k + timeseries.n_cols)
//   for (arma::uword i = 0; i < timeseries.n_rows; i++) {
//     for (arma::uword c = 0; c < timeseries.n_cols; c++) {
//
//       xy(c,0) = timeseries(i,c) * cos((2 * arma::datum::pi * c) / timeseries.n_cols);
//       xy(c,1) = timeseries(i,c) * sin((2 * arma::datum::pi * c) / timeseries.n_cols);
//       xy(c,2) = c;
//
//     }
//
//     pts.insert_rows(k, k + timeseries.n_cols, xy);
//
//     // add the first points into last position
//     // xy(timeseries.n_cols, 0) = x(0,0);
//     // xy(timeseries.n_cols,1) = y(0,1);
//     // xy(timeseries.n_cols,2) = y(0,2);
//
//     //pts = arma::join_vert(pts, xy);
//   }
//   return pts;
// }

// [[Rcpp::export]]
arma::vec calc_distance(const arma::mat& line, const arma::rowvec& pts_cent) {

  return arma::sqrt(arma::square((line.col(0) - pts_cent.at(0))) + arma::square((line.col(1) - pts_cent.at(1))));
}

// [[Rcpp::export]]
arma::vec gr_calc(const arma::mat& pts_cent, const arma::mat& pts_line, const arma::uword size_col) {

  // vector to store values
  arma::vec pts_values(pts_cent.n_rows, arma::fill::zeros);
  arma::uword c = 0;

  for (arma::uword i = 0; i < pts_cent.n_rows; i++) {
      arma::mat line = pts_line.submat( c, 0, c + size_col, 1 );
      pts_values(i) = arma::mean(calc_distance(line, pts_cent.row(i)));

      c = c + size_col + 1;
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
