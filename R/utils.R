#' @title ...
#' @name create_polygon
#'
#' @param timeseries ...
#'
#' @return a polygon object from \code{sf} class
create_polygon <- function(timeseries) {

  # verify if the time series are corrected
  timeseries <- .verify_timeseries(timeseries)

  # transform to polar space
  pts_values <- calculate_polar(timeseries)

  poly_list <- do.call(rbind, pts_values)

  # return only polygon object, not geometry collection
  return(sfheaders::sf_polygon( poly_list, polygon_id = 3))
}
#' @title ...
#' @name create_polygon_geos
#'
#' @param timeseries ...
#'
#' @return a polygon object from \code{sf} class
create_polygon_geos <- function(timeseries) {

  # verify if the time series are corrected
  timeseries <- .verify_timeseries(timeseries)

  # TODO: comentar
  pts_values <- calculate_polar(timeseries)

  poly_list <- do.call(rbind, pts_values)

  # return only polygon object, not geometry collection
  return(geos::geos_make_polygon(x = poly_list[,1],
                                 y = poly_list[,2],
                                 feature_id = poly_list[,3]))
}
#' @title ....
#' @name .verify_timeseries
#'
#' @param timeseries ...
#' @return ...
.verify_timeseries <- function(timeseries) {

  # assert that only supported types are provided
  stopifnot(inherits(timeseries, "numeric") ||
              inherits(timeseries, "matrix"))

  # if not a matrix, just transpose numeric time series
  if (inherits(timeseries, "numeric"))
    timeseries <- t(timeseries)

  return(timeseries)
}

#' @title ...
#' @name get_all_areas
#'
#' @description The standard deviation of the areas per season
#'
#' @param timeseries ...
#'
#' @return ...
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

#' @title ...
#' @name .get_instances
#'
#' @param polygon a \code{sf} object...
#'
#' @return a \code{numeric} vector..
.get_instances <- function(polygon) {
  nrow(sf::st_coordinates(polygon[1,])) - 1
}
