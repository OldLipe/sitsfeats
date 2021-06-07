#' @title Creates a Polygon from sf package
#' @name create_polygon
#'
#' @param timeseries   a \code{numeric} or \code{matrix} object where the
#'  columns is the point in time.
#'
#' @return a polygon object from \code{sf} class.
create_polygon <- function(timeseries) {

  # verify if the time series are corrected
  timeseries <- .verify_timeseries(timeseries)

  # transform to polar space
  pts_values <- calculate_polar(timeseries)

  poly_list <- do.call(rbind, pts_values)

  # return only polygon object, not geometry collection
  return(sfheaders::sf_polygon( poly_list, polygon_id = 3))
}

#' @title Creates a polygon from geos
#' @name create_polygon_geos
#'
#' @param timeseries  a \code{numeric} or \code{matrix} object where the
#'  columns is the point in time.
#'
#' @return a polygon object from \code{geos} class.
create_polygon_geos <- function(timeseries) {

  # verify if the time series are corrected
  timeseries <- .verify_timeseries(timeseries)

  # transform to polar space
  pts_values <- calculate_polar(timeseries)

  poly_list <- do.call(rbind, pts_values)

  # return only polygon object, not geometry collection
  return(geos::geos_make_polygon(x = poly_list[,1],
                                 y = poly_list[,2],
                                 feature_id = poly_list[,3]))
}

#' @title Verify time series
#' @name .verify_timeseries
#'
#' @param timeseries  a \code{numeric} or \code{matrix} object where the
#'  columns is the point in time.
#' @return a \code{matrix} from time series.
.verify_timeseries <- function(timeseries) {

  # assert that only supported types are provided
  stopifnot(inherits(timeseries, "numeric") ||
              inherits(timeseries, "matrix"))

  # if not a matrix, just transpose numeric time series
  if (inherits(timeseries, "numeric"))
    timeseries <- t(timeseries)

  return(timeseries)
}

#' @title Get all areas from a closed polar polygon
#' @name get_all_areas
#'
#' @description The standard deviation of the areas per season
#'
#' @param timeseries  a \code{numeric} or \code{matrix} object where the
#'  columns is the point in time.
#'
#' @return a \code{matrix} with each area from each quadrant.
#' @export
get_all_areas <- function(timeseries) {

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

  # creates a polygon in top left quadrant
  poly_topright <- sfheaders::sf_polygon(polytopright(pts_bbox),
                                         polygon_id = 3)

  # creates a polygon in top left quadrant
  poly_bottomleft <- sfheaders::sf_polygon(polybottomleft(pts_bbox),
                                           polygon_id = 3)

  # creates a polygon in top left quadrant
  poly_bottomright <- sfheaders::sf_polygon(polybottomright(pts_bbox),
                                            polygon_id = 3)


  return(std_np(cbind(sf::st_area(sf::st_crop(polygon, poly_topleft)),
                      sf::st_area(sf::st_crop(polygon, poly_topright)),
                      sf::st_area(sf::st_crop(polygon, poly_bottomleft)),
                      sf::st_area(sf::st_crop(polygon, poly_bottomright)))))

}

#' @title Get the number of lines contained in the polygon
#' @name .get_instances
#'
#' @param polygon a \code{sf} object with Polygons.
#'
#' @return a \code{numeric} vector with the number of lines contained in the
#' polygon.
.get_instances <- function(polygon) {
  nrow(sf::st_coordinates(polygon[1,])) - 1
}
