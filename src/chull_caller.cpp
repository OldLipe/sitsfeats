// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/geometry.hpp>
#include <boost/geometry/geometries/polygon.hpp>
#include <boost/geometry/geometries/adapted/boost_tuple.hpp>

BOOST_GEOMETRY_REGISTER_BOOST_TUPLE_CS(cs::cartesian)

typedef boost::tuple<double, double> point;
typedef boost::geometry::model::polygon<point, true, true> polygon;

namespace Rcpp {

// as<>() converter from R to Boost.Geometry's polygon type
template <> polygon as(SEXP pointsMatrixSEXP) {
  // the coordinates are the rows of the (n x 2) matrix
  NumericMatrix pointsMatrix(pointsMatrixSEXP);
  polygon poly;
  for (int i = 0; i < pointsMatrix.nrow(); ++i) {
    double x = pointsMatrix(i,0);
    double y = pointsMatrix(i,1);
    point p(x,y);
    poly.outer().push_back(p);
  }
  return (poly);
}

// wrap() converter from Boost.Geometry's polygon to an R(cpp) matrix
// The Rcpp NumericMatrix can be converted to/from a SEXP
template <> SEXP wrap(const polygon& poly) {
  const std::vector<point>& points = poly.outer();
  NumericMatrix rmat(points.size(), 2);
  for (unsigned int i = 0; i < points.size(); ++i) {
    const point& p = points[i];
    rmat(i,0) = p.get<0>();
    rmat(i,1) = p.get<1>();
  }
  return Rcpp::wrap(rmat);
}
}
// [[Rcpp::export]]
Rcpp::NumericMatrix convexHullRcpp(SEXP pointsMatrixSEXP){

  // Conversion of pointsMatrix here to boost::geometry polygon
  polygon poly = Rcpp::as<polygon>(pointsMatrixSEXP);

  polygon hull;
  // Compute the convex hull
  boost::geometry::convex_hull(poly, hull);

  // Convert hull into a NumericMatrixsomething that Rcpp can hand back to R
  return Rcpp::wrap(hull);
}
