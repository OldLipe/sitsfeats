#' @title Polar metrics
#'
#' @name polar_metrics
#'
#' @description
#' The package sitsfeats provides a set of polars metrics:
#' \itemize{
#'   \item The \code{area_q1()}area of the closed shape over the first quadrant.
#'   \item The \code{area_q2()} area of the closed shape over the second
#'    quadrant.
#'   \item The \code{area_q3()} area of the closed shape over the third
#'   quadrant.
#'   \item The \code{area_q4()} area of the closed shape over the fourth
#'    quadrant
#'   \item The \code{polar_balance()} the standard deviation of the areas per
#'   season.
#'   \item The \code{angle()} the angle of the closed shape.
#'   \item The \code{area_ts()} area of the closed shape.
#'   \item The \code{ecc_metric()} return values close to 0 if the shape is a
#'   circle and 1.
#'   \item The \code{gyration_radius()} equals the average distance between each
#'   point inside.
#'   \item The \code{csi()} this is a dimensionless quantitative measure of
#'   morphology.
#' }
#'
#' @param timeseries   a \code{numeric} or \code{matrix} object where the
#'  columns is the point in time.
#'
#' @examples
#' data("timeseries")
#' areas_values <- rbind(sitsfeats::area_q1(timeseries),
#'                       sitsfeats::area_q2(timeseries),
#'                       sitsfeats::area_q3(timeseries),
#'                       sitsfeats::area_q4(timeseries))
#'
#' @return a \code{numeric} vector for each metric in each time series
#'
#' @export
area_q1 <- function(timeseries) {

  # create a sf polygons object
  polygon <- create_polygon(timeseries)

  # get coordinates from points
  pts_poly <- sf::st_coordinates(polygon)

  # get time series instances
  instances_time <- .get_instances(polygon)

  pts_bbox <- get_seasons_fast(pts_poly, instances_time)

  # creates a polygon in top left quadrant
  poly_topleft <- sfheaders::sf_polygon(polytopleft(pts_bbox),
                                        polygon_id = 3)

  cbind(sf::st_area(sf::st_crop(polygon, poly_topleft)))
}

#' @rdname polar_metrics
#' @export
area_q2 <- function(timeseries) {

  # create a sf polygons object
  polygon <- create_polygon(timeseries)

  # get coordinates from points
  pts_poly <- sf::st_coordinates(polygon)

  # get time series instances
  instances_time <- .get_instances(polygon)

  pts_bbox <- get_seasons_fast(pts_poly, instances_time)

  # creates a polygon in top left quadrant
  poly_topright <- sfheaders::sf_polygon(polytopright(pts_bbox),
                                         polygon_id = 3)

  cbind(sf::st_area(sf::st_crop(polygon, poly_topright)))
}

#' @rdname polar_metrics
#' @export
area_q3 <- function(timeseries) {

  # create a sf polygons object
  polygon <- create_polygon(timeseries)

  # get coordinates from points
  pts_poly <- sf::st_coordinates(polygon)

  # get time series instances
  instances_time <- .get_instances(polygon)

  pts_bbox <- get_seasons_fast(pts_poly, instances_time)

  # creates a polygon in top left quadrant
  poly_bottomleft <- sfheaders::sf_polygon(polybottomleft(pts_bbox),
                                           polygon_id = 3)

  cbind(sf::st_area(sf::st_crop(polygon, poly_bottomleft)))
}

#' @rdname polar_metrics
#' @export
area_q4 <- function(timeseries) {

  # create a sf polygons object
  polygon <- create_polygon(timeseries)

  # get coordinates from points
  pts_poly <- sf::st_coordinates(polygon)

  # get time series instances
  instances_time <- .get_instances(polygon)

  pts_bbox <- get_seasons_fast(pts_poly, instances_time)

  # creates a polygon in top left quadrant
  poly_bottomright <- sfheaders::sf_polygon(polybottomright(pts_bbox),
                                            polygon_id = 3)

  cbind(sf::st_area(sf::st_crop(polygon, poly_bottomright)))
}

#' @rdname polar_metrics
#' @export
polar_balance <- function(timeseries) {

  # verify if the time series are corrected
  timeseries <- .verify_timeseries(timeseries)

  get_all_areas(timeseries)
}

#' @rdname polar_metrics
#' @export
angle <- function(timeseries) {

  # verify if the time series are corrected
  timeseries <- .verify_timeseries(timeseries)

  calc_angle(timeseries)
}

#' @rdname polar_metrics
#' @export
area_ts <- function(timeseries) {

  polygon <- create_polygon(timeseries)
  cbind(sf::st_area(polygon))
}

#' @rdname polar_metrics
#' @export
ecc_metric <- function(timeseries) {

  polygon <- create_polygon_geos(timeseries)
  pts_mbr <- geos::geos_minimum_rotated_rectangle(polygon)

  # slow solution
  # TODO: calculute bbox in cpp

  # Transforming to geos to sf
  pts_sf <- sf::st_as_sf(pts_mbr)

  st_bbox_by_feature = function(sf_obj) {
    geom <- sf::st_geometry(sf_obj)
    do.call(rbind, lapply(geom, sf::st_bbox))
  }

  bbox_pts <- st_bbox_by_feature(pts_sf)

  calc_ecc(bbox_pts)
}

#' @rdname polar_metrics
#' @export
gyration_radius <- function(timeseries) {

  polygon <- create_polygon(timeseries)

  # get number of column
  size_col <- .get_instances(polygon)

  pts_cent <- sf::st_coordinates(sf::st_centroid(polygon))
  pts_line <- sf::st_coordinates(sfheaders::sf_cast(polygon, "LINESTRING"))[,1:2]

  gr_calc(pts_cent, pts_line, size_col)
}

#' @rdname polar_metrics
#' @export
csi <- function(timeseries) {

  polygon <- create_polygon(timeseries)
  ls <- sfheaders::sf_cast(polygon, "LINESTRING")

  calc_csi(sf::st_length(ls), sf::st_area(polygon))
}
