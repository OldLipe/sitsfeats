#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

struct point2D { double x, y; };

// check if a point is on the LEFT side of an edge
bool inside(point2D p, point2D p1, point2D p2)
{
  return (p2.y - p1.y) * p.x + (p1.x - p2.x) * p.y + (p2.x * p1.y - p1.x * p2.y) < 0;
}

// calculate intersection point
point2D intersection(point2D cp1, point2D cp2, point2D s, point2D e)
{
  point2D dc = { cp1.x - cp2.x, cp1.y - cp2.y };
  point2D dp = { s.x - e.x, s.y - e.y };

  double n1 = cp1.x * cp2.y - cp1.y * cp2.x;
  double n2 = s.x * e.y - s.y * e.x;
  double n3 = 1.0 / (dc.x * dp.y - dc.y * dp.x);

  return {(n1 * dp.x - n2 * dc.x) * n3, (n1 * dp.y - n2 * dc.y) * n3};
}

// [[Rcpp::export]]
arma::uword mod(arma::uword a, arma::uword n)
{
  return a - floor(a/n)*n;
}

// [[Rcpp::export]]
arma::mat sutherland(const arma::mat& subjectPoly,
                     const arma::mat& clipPoly){

  arma::mat newPoly(subjectPoly.n_rows, 2, arma::fill::zeros);
  arma::uword subjectpolysize = subjectPoly.n_rows;

  for (arma::uword i = 0; i < clipPoly.n_rows; i++) {

    int counter = 0;
    int idx = mod(i + 1, clipPoly.n_rows);

    // get clipping polygon edge
    arma::rowvec cp1_vec = clipPoly.row(i);
    arma::rowvec cp2_vec = clipPoly.row(idx);;

    point2D cp1 = {cp1_vec.at(0), cp1_vec.at(1)};
    point2D cp2 = {cp2_vec.at(0), cp2_vec.at(1)};

    for (arma::uword c = 0; c < subjectpolysize; c++) {
      // get subject polygon edge
      arma::rowvec s_vec = subjectPoly.row(i);
      arma::rowvec e_vec = subjectPoly.row(mod(i + 1, subjectpolysize));

      point2D s = {s_vec.at(0), s_vec.at(1)};
      point2D e = {e_vec.at(0), e_vec.at(1)};

      // Case 1: Both vertices are inside:
      // Only the second vertex is added to the output list
      if(inside(s, cp1, cp2) && inside(e, cp1, cp2)) {
        newPoly.row(counter) = {e.x, e.y};
        counter++;
      }

      // Case 2: First vertex is outside while second one is inside:
      // Both the point of intersection of the edge with the clip boundary
      // and the second vertex are added to the output list
      else if(!inside(s, cp1, cp2) && inside(e, cp1, cp2))
      {
        point2D r = intersection(cp1, cp2, s, e);
        newPoly.row(counter) = {r.x, r.y};
        counter++;
        newPoly.row(counter) = {e.x, e.y};
        counter++;
      }

      // Case 3: First vertex is inside while second one is outside:
      // Only the point of intersection of the edge with the clip boundary
      // is added to the output list
      else if(inside(s, cp1, cp2) && !inside(e, cp1, cp2)) {
        point2D r = intersection(cp1, cp2, s, e);
        newPoly.row(counter) = {r.x, r.y};
        counter++;
      }


      // Case 4: Both vertices are outside
      // else if(!inside(s, cp1, cp2) && !inside(e, cp1, cp2))
      // {
      //   // No vertices are added to the output list
      // }

    }
    subjectpolysize = counter;
  }
  return newPoly;
}

// // Sutherland-Hodgman clipping
// void SutherlandHodgman(point2D *subjectPolygon,
//                        int &subjectPolygonSize,
//                        point2D *clipPolygon,
//                        int &clipPolygonSize,
//                        point2D (&newPolygon)[N],
//                        int &newPolygonSize)
// {
//   point2D cp1, cp2, s, e, inputPolygon[N];
//
//   // copy subject polygon to new polygon and set its size
//   for(int i = 0; i < subjectPolygonSize; i++)
//     newPolygon[i] = subjectPolygon[i];
//
//   newPolygonSize = subjectPolygonSize;
//
//   for(int j = 0; j < clipPolygonSize; j++)
//   {
//     // copy new polygon to input polygon & set counter to 0
//     for(int k = 0; k < newPolygonSize; k++){ inputPolygon[k] = newPolygon[k]; }
//     int counter = 0;
//
//     // get clipping polygon edge
//     cp1 = clipPolygon[j];
//     cp2 = clipPolygon[(j + 1) % clipPolygonSize];
//
//     for(int i = 0; i < newPolygonSize; i++)
//     {
//       // get subject polygon edge
//       s = inputPolygon[i];
//       e = inputPolygon[(i + 1) % newPolygonSize];
//
//       // Case 1: Both vertices are inside:
//       // Only the second vertex is added to the output list
//       if(inside(s, cp1, cp2) && inside(e, cp1, cp2))
//         newPolygon[counter++] = e;
//
//       // Case 2: First vertex is outside while second one is inside:
//       // Both the point of intersection of the edge with the clip boundary
//       // and the second vertex are added to the output list
//       else if(!inside(s, cp1, cp2) && inside(e, cp1, cp2))
//       {
//         newPolygon[counter++] = intersection(cp1, cp2, s, e);
//         newPolygon[counter++] = e;
//       }
//
//       // Case 3: First vertex is inside while second one is outside:
//       // Only the point of intersection of the edge with the clip boundary
//       // is added to the output list
//       else if(inside(s, cp1, cp2) && !inside(e, cp1, cp2))
//         newPolygon[counter++] = intersection(cp1, cp2, s, e);
//
//       // Case 4: Both vertices are outside
//       else if(!inside(s, cp1, cp2) && !inside(e, cp1, cp2))
//       {
//         // No vertices are added to the output list
//       }
//     }
//     // set new polygon size
//     newPolygonSize = counter;
//   }
// }
