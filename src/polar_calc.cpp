#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::rowvec calc_bbox(const arma::mat& pts) {
  double minx = -arma::max(arma::abs(pts.col(0)));
  double miny = -arma::max(arma::abs(pts.col(1)));

  double maxx = arma::max(arma::abs(pts.col(0)));
  double maxy = arma::max(arma::abs(pts.col(1)));

  return(arma::rowvec({minx, miny, maxx, maxy}));
}

// [[Rcpp::export]]
arma::mat get_seasons_fast(const arma::mat& pts, const arma::uword colsize) {

  // get original number of matrix rows
  const double nrows_origin = pts.n_rows/(colsize + 1);
  arma::mat pts_bbox(nrows_origin, 4, arma::fill::zeros);

  arma::uword c = 0;
  for (arma::uword i = 0; i < nrows_origin; i++) {

    arma::mat line = pts.submat( c, 0, c + colsize, 1 );
    pts_bbox.row(i) = calc_bbox(line);

    c = c + colsize;
  }

  return pts_bbox;
}

// [[Rcpp::export]]
arma::mat polytopleft(const arma::mat bbox_pts){

  arma::mat pts_ptl(bbox_pts.n_rows*5, 3, arma::fill::zeros);

  arma::uword c = 0;
  for (arma::uword i = 0; i < bbox_pts.n_rows; i++) {

    pts_ptl.row(c + 0) = {bbox_pts(i,0), bbox_pts(i,3), (double) i}; // coord0
    pts_ptl.row(c + 1) = {bbox_pts(i,0), 0, (double) i};             // coord3
    pts_ptl.row(c + 2) = {0, 0, (double) i};                         // coord4
    pts_ptl.row(c + 3) = {0, bbox_pts(i,3), (double) i};             // coord1
    pts_ptl.row(c + 4) = {bbox_pts(i,0), bbox_pts(i,3), (double) i}; // coord0

    c = c + 5;
  }
  return pts_ptl;
}

// [[Rcpp::export]]
arma::mat polytopright(const arma::mat bbox_pts){

  arma::mat pts_ptr(bbox_pts.n_rows*5, 3, arma::fill::zeros);

  arma::uword c = 0;
  for (arma::uword i = 0; i < bbox_pts.n_rows; i++) {

    pts_ptr.row(c + 0) = {0, bbox_pts(i,3), (double) i};             // coord1
    pts_ptr.row(c + 1) = {bbox_pts(i,2), bbox_pts(i,3), (double) i}; // coord2
    pts_ptr.row(c + 2) = {bbox_pts(i,2), 0, (double) i};             // coord5
    pts_ptr.row(c + 3) = {0, 0, (double) i};                         // coord4
    pts_ptr.row(c + 4) = {0, bbox_pts(i,3), (double) i};             // coord1

    c = c + 5;
  }
  return pts_ptr;
}

// [[Rcpp::export]]
arma::mat polybottomleft(const arma::mat bbox_pts){

  arma::mat pts_pbl(bbox_pts.n_rows*5, 3, arma::fill::zeros);

  arma::uword c = 0;
  for (arma::uword i = 0; i < bbox_pts.n_rows; i++) {

    pts_pbl.row(c + 0) = {bbox_pts(i,0), 0, (double) i};             // coord3
    pts_pbl.row(c + 1) = {0, 0, (double) i};                         // coord4
    pts_pbl.row(c + 2) = {0, bbox_pts(i,1), (double) i};             // coord7
    pts_pbl.row(c + 3) = {bbox_pts(i,0), bbox_pts(i,1), (double) i}; // coord6
    pts_pbl.row(c + 4) = {bbox_pts(i,0), 0, (double) i};             // coord3

    c = c + 5;
  }
  return pts_pbl;
}

// [[Rcpp::export]]
arma::mat polybottomright(const arma::mat bbox_pts){

  arma::mat pts_pbr(bbox_pts.n_rows*5, 3, arma::fill::zeros);

  arma::uword c = 0;
  for (arma::uword i = 0; i < bbox_pts.n_rows; i++) {

    pts_pbr.row(c + 0) = {0, 0, (double) i};                         // coord4
    pts_pbr.row(c + 1) = {bbox_pts(i,2), 0, (double) i};             // coord5
    pts_pbr.row(c + 2) = {bbox_pts(i,2), bbox_pts(i,1), (double) i}; // coord8
    pts_pbr.row(c + 3) = {0, bbox_pts(i,1), (double) i};             // coord7
    pts_pbr.row(c + 4) = {0, 0, (double) i};                         // coord4

    c = c + 5;
  }
  return pts_pbr;
}

// [[Rcpp::export]]
arma::vec repvalues(arma::uword x, arma::uword time) {
  //int n = y.size();
  arma::vec myvector(time, arma::fill::zeros);
  for (arma::uword i = 0; i < time; ++i) {
    myvector(i) = x;
  }
  return myvector;
}

// [[Rcpp::export]]
Rcpp::List calculate_polar(arma::mat& timeseries) {

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

    pts[i] = arma::join_rows(x,y, repvalues(i, timeseries.n_cols + 1));
  }
  return pts;
}

// [[Rcpp::export]]
arma::vec calc_distance(const arma::mat& line, const arma::rowvec& pts_cent) {

  return arma::sqrt(arma::square((line.col(0) - pts_cent.at(0))) + arma::square((line.col(1) - pts_cent.at(1))));
}

// [[Rcpp::export]]
arma::vec gr_calc(const arma::mat& pts_cent, const arma::mat& pts_line,
                  const arma::uword size_col) {

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

// [[Rcpp::export]]
arma::mat std_np(const arma::mat& timeseries) {

  return arma::stddev(timeseries, 1, 1);
}

// [[Rcpp::export]]
arma::mat calc_ecc(const arma::mat& bboxmat) {

  arma::mat axis (bboxmat.n_rows, 2, arma::fill::zeros);
  axis.col(0) = bboxmat.col(2) - bboxmat.col(0);
  axis.col(1) = bboxmat.col(3) - bboxmat.col(1);
  return arma::min(axis, 1)/arma::max(axis, 1);
}

// [[Rcpp::export]]
arma::vec linspace_vec(const arma::rowvec& timeseries) {

  return arma::linspace<arma::vec>(0, 2*arma::datum::pi, timeseries.n_elem);
}

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
