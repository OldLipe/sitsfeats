#' @title ...
#' @name create_polygon
#'
#' @param timeseries ...
#'
#' @return a polygon object from \code{sf} class
create_polygon <- function(timeseries) {

  # TODO: comentar
  pts_values <- calculate_vec(abs(timeseries))

  # add the first points into last position
  pts_values <- rbind(pts_values, pts_values[1,])

  poly_obj <- sf::st_linestring(pts_values)

  # return only polygon object, not geometry collection
  return(sf::st_polygonize(poly_obj)[[1]])
}
#' @title ...
#' @name create_polygon_v2
#'
#' @param timeseries ...
#'
#' @return a polygon object from \code{sf} class
create_polygon_v2 <- function(timeseries) {

  # TODO: comentar
  pts_values <- calculate_vec_v3(timeseries)

  poly_list <- purrr::map(pts_values, function(x){sf::st_polygon(list(x))})

  # return only polygon object, not geometry collection
  return(sf::st_as_sfc(poly_list))
}
#' @title ...
#' @name calc_bbox
#'
#' @param x ...
#' @param y ...
#'
#' @return a list ...
calc_bbox <- function(x,y) {

  minX = -max(abs(x))
  minY = -max(abs(y))
  maxX = max(abs(x))
  maxY = max(abs(y))

  c("minX" = minX,
    "minY" = minY,
    "maxX" = maxX,
    "maxY" = maxY)
}
#' @title ...
#' @name create_list_of_angles
#'
#' @param timeseries ...
#'
#' @return ...
create_list_of_angles <- function(timeseries) {

  return(as.vector(linspace_vec(timeseries)))
}
#' @title ...
#' @name get_seasons
#'
#' @param x ...
#' @param y ...
#'
#' @return ...
get_seasons <- function(x,y) {

  pts_poly <- calc_bbox(x,y)

  coord0 <- c(pts_poly[["minX"]], pts_poly[["maxY"]])
  coord1 <- c(0, pts_poly[["maxY"]])
  coord2 <- c(pts_poly[["maxX"]], pts_poly[["maxY"]])
  coord3 <- c(pts_poly[["minX"]], 0)
  coord4 <- c(0, 0)
  coord5 <- c(pts_poly[["maxX"]], 0)
  coord6 <- c(pts_poly[["minX"]], pts_poly[["minY"]])
  coord7 <- c(0, pts_poly[["minY"]])
  coord8 <- c(pts_poly[["maxX"]], pts_poly[["minY"]])

  # compute season polygons
  polyTopLeft <- sf::st_polygon(list(rbind(coord0, coord3, coord4, coord1, coord0)))
  polyTopRight <- sf::st_polygon(list(rbind(coord1, coord2, coord5, coord4, coord1)))
  polyBottomLeft <- sf::st_polygon(list(rbind(coord3, coord4, coord7, coord6, coord3)))
  polyBottomRight <- sf::st_polygon(list(rbind(coord4, coord5, coord8, coord7, coord4)))


  return(list("polyTopLeft" = polyTopLeft,
              "polyTopRight" = polyTopRight,
              "polyBottomLeft" = polyBottomLeft,
              "polyBottomRight" = polyBottomRight))
}
#' @title ...
#' @name area_season
#'
#' @param timeseries ...
#'
#' @return ...
area_season <- function(timeseries) {

  polygon <- create_polygon(timeseries)

  pts_poly <- sf::st_coordinates(polygon)
  polygons <- get_seasons(pts_poly[,1], pts_poly[,2])


  quaterPolyTopLeft = sf::st_intersection(polygons[["polyTopLeft"]], polygon)
  quaterPolyTopRight = sf::st_intersection(polygons[["polyTopRight"]], polygon)
  quaterPolyBottomLeft = sf::st_intersection(polygons[["polyBottomLeft"]], polygon)
  quaterPolyBottomRight = sf::st_intersection(polygons[["polyBottomRight"]], polygon)


  list(quaterPolyTopLeft, quaterPolyTopRight,
       quaterPolyBottomLeft, quaterPolyBottomRight)

  # TODO: retornar a lista de polygons
}
####  Heavy Metrics  ####

#' @title ...
#' @name polar_metrics
#'
#' @note é necessária a instação do pacote grDevices para o método chull
#'
#' @param timeseries ...
#' @param metrics ...
#'
#' @return ...
#' @export
polar_metrics <- function(timeseries, metrics = c()) {

  values_area <- NULL
  values_poli <- NULL
  values_angle <- NULL

  if (metrics == "all")
    metrics <- c("area_q1",
                 "area_q2",
                 "area_q3",
                 "area_q4",
                 "polar_balance",
                 "csi",
                 "gyration_radius",
                 "ecc_metric",
                 "area_ts",
                 "angle")

  if (any(c("area_q1",
            "area_q2",
            "area_q3",
            "area_q4",
            "polar_balance") %in% metrics)) {
    area <- sitsfeats::area_season(timeseries)
    index_metrics <- which(metrics %in% c("area_q1",
                                          "area_q2",
                                          "area_q3",
                                          "area_q4",
                                          "polar_balance"))
    values_area <- sapply(metrics[index_metrics], function(metric) {
      switch(metric,
             area_q1 = sitsfeats::area_q1(area),
             area_q2 = sitsfeats::area_q2(area),
             area_q3 = sitsfeats::area_q3(area),
             area_q4 = sitsfeats::area_q4(area),
             polar_balance = sitsfeats::polar_balance(area))

    })
  }

  if (any(c("csi",
            "gyration_radius",
            "ecc_metric",
            "area_ts") %in% metrics)) {

    index_metrics <- which(metrics %in% c("csi",
                                          "gyration_radius",
                                          "ecc_metric",
                                          "area_ts"))

    polygon <- sitsfeats::create_polygon(timeseries)
    values_poli <- sapply(metrics[index_metrics], function(metric) {
      switch(metric,
             csi = sitsfeats::csi(polygon),
             gyration_radius = sitsfeats::gyration_radius(polygon),
             ecc_metric = sitsfeats::ecc_metric(polygon),
             area_ts = sitsfeats::area_ts(polygon))

    })
  }
  if (any(c("angle") %in% metrics)) {
    values_angle <-  sitsfeats::angle(timeseries)
    names(values_angle) <- "angle"
  }

  return(c(values_area, values_poli, values_angle))
}

# metricas que usam a função area_season
#' @title ...
#' @name area_q1
#'
#' @description Area of the closed shape over the first quadrant
#'
#' @param type ...
#'
#' @return ...
#' @export
area_q1 <- function(type = "list") {
  UseMethod("area_q1", type)
}
#' @title ...
#' @name area_q1.list
#'
#' @description Area of the closed shape over the first quadrant
#'
#' @param list_of_polygons ...
#'
#' @return ...
#' @export
area_q1.list <- function(list_of_polygons) {
  return(sf::st_area(list_of_polygons[[1]]))
}
#' @title ...
#' @name area_q1.numeric
#'
#' @description Area of the closed shape over the first quadrant
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
area_q1.numeric <- function(timeseries) {
  areas <- area_season(timeseries)
  return(sf::st_area(areas[[1]]))
}
#' @title ...
#' @name area_q2
#'
#' @description Area of the closed shape over the second quadrant
#'
#' @param type ...
#'
#' @return ...
#' @export
area_q2 <- function(type = "list") {
  UseMethod("area_q2", type)
}
#' @title ...
#' @name area_q2.list
#'
#' @description Area of the closed shape over the second quadrant
#'
#' @param list_of_polygons ...
#'
#' @return ...
#' @export
area_q2.list <- function(list_of_polygons) {
  return(sf::st_area(list_of_polygons[[2]]))
}
#' @title ...
#' @name area_q2.numeric
#'
#' @description Area of the closed shape over the second quadrant
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
area_q2.numeric <- function(timeseries) {
  areas <- area_season(timeseries)
  return(sf::st_area(areas[[2]]))
}
#' @title ...
#' @name area_q3
#'
#' @description Area of the closed shape over the third quadrant.
#'
#' @param type ...
#'
#' @return ...
#' @export
area_q3 <- function(type = "list") {
  UseMethod("area_q3", type)
}
#' @title ...
#' @name area_q3.list
#'
#' @description Area of the closed shape over the third quadrant.
#'
#' @param list_of_polygons ...
#'
#' @return ...
#' @export
area_q3.list <- function(list_of_polygons) {
  return(sf::st_area(list_of_polygons[[3]]))
}
#' @title ...
#' @name area_q3.numeric
#'
#' @description Area of the closed shape over the third quadrant.
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
area_q3.numeric <- function(timeseries) {
  areas <- area_season(timeseries)
  return(sf::st_area(areas[[3]]))
}
#' @title ...
#' @name area_q4
#'
#' @description Area of the closed shape over the fourth quadrant
#'
#' @param type ...
#'
#' @return ...
#' @export
area_q4 <- function(type = "list") {
  UseMethod("area_q4", type)
}
#' @title ...
#' @name area_q4.list
#'
#' @description Area of the closed shape over the fourth quadrant
#'
#' @param list_of_polygons ...
#'
#' @return ...
#' @export
area_q4.list <- function(list_of_polygons) {
  return(sf::st_area(list_of_polygons[[4]]))
}
#' @title ...
#' @name area_q4.numeric
#'
#' @description Area of the closed shape over the fourth quadrant
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
area_q4.numeric <- function(timeseries) {
  areas = area_season(timeseries)
  return(sf::st_area(areas[[4]]))
}
#' @title ...
#' @name polar_balance
#'
#' @description The standard deviation of the areas per season
#'
#' @param type ...
#'
#' @return ...
#' @export
polar_balance <- function(type = "list") {
  UseMethod("polar_balance", type)
}
#' @title ...
#' @name polar_balance.list
#'
#' @description The standard deviation of the areas per season
#'
#' @param list_of_polygons ...
#'
#' @return ...
#' @export
polar_balance.list <- function(list_of_polygons) {

  areas_list <- lapply(list_of_polygons, sf::st_area)

  std_numpy <- function(x) {
    sd(x) * sqrt((length(x) - 1) / length(x))
  }

  return(std_numpy(unlist(areas_list)))
}
#' @title ...
#' @name polar_balance.numeric
#'
#' @description The standard deviation of the areas per season
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
polar_balance.numeric <- function(timeseries) {
  areas <- area_season(timeseries)

  areas_list <- lapply(areas, sf::st_area)

  std_numpy <- function(x) {
    sd(x) * sqrt((length(x) - 1) / length(x))
  }

  return(std_numpy(unlist(areas_list)))
}

####  Light metrics  ####
# Metricas que não dependem de função repetitivas
#' @title ...
#' @name angle
#'
#' @description Area of the closed shape
#'
#' @param type ...
#'
#' @return ...
#' @export
angle <- function(type = "list") {

  UseMethod("angle", type)
}

#' @title ...
#' @name angle
#'
#' @description The main angle of the closed shape created after transformation
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
angle.numeric <- function(timeseries) {

  timeseries <- abs(timeseries)

  angles <- create_list_of_angles(timeseries)
  return(angles[which.max(timeseries)])
}

#' @title ...
#' @name angle
#'
#' @description The main angle of the closed shape created after transformation
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
angle.matrix <- function(timeseries) {

  calc_angle(timeseries)
}

#### Medium Metrics ####
#' @title ...
#' @name area_ts
#'
#' @description Area of the closed shape
#'
#' @param type ...
#'
#' @return ...
#' @export
area_ts <- function(type = "list") {

  UseMethod("area_ts", type)
}
#' @title ...
#' @name area_ts.XY
#'
#' @description Area of the closed shape
#'
#' @param polygon ...
#'
#' @return ...
#' @export
area_ts.XY <- function(polygon) {
  return(sf::st_area(polygon))
}
#' @title ...
#' @name area_ts.numeric
#'
#' @description Area of the closed shape
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
area_ts.numeric <- function(timeseries) {
  polygon <- create_polygon(timeseries)
  return(sf::st_area(polygon))
}
#' @title ...
#' @name area_ts.matrix
#'
#' @description Area of the closed shape
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
area_ts.matrix <- function(timeseries) {
  polygon <- create_polygon_v2(timeseries)
  return(sf::st_area(polygon))
}
#' @title ...
#' @name area_ts.sfc_POLYGON
#'
#' @description Area of the closed shape
#'
#' @param polygon ...
#'
#' @return ...
#' @export
area_ts.sfc_POLYGON <- function(polygon) {
  return(sf::st_area(polygon))
}
#' @title ...
#' @name ecc_metric
#'
#' @description Return values close to 0 if the shape is a circle and 1
#'
#' @param type ...
#'
#' @return ...
#' @export
ecc_metric <- function(type) {
  UseMethod("ecc_metric", type)
}
#' @title ...
#' @name ecc_metric.POLYGON
#'
#' @description Return values close to 0 if the shape is a circle and 1
#'
#' @param polygon ...
#'
#' @return ...
#' @export
ecc_metric.XY <- function(polygon) {

  pts_mbr <- MBR(sf::st_coordinates(polygon)[,1:2])

  bbox_pts <- sf::st_bbox(pts_mbr)

  axis1 = bbox_pts[["xmax"]] - bbox_pts[["xmin"]]
  axis2 = bbox_pts[["ymax"]] - bbox_pts[["ymin"]]
  stats = c(axis1, axis2)

  return(min(stats) / max(stats))
}
#' @title ...
#' @name ecc_metric.numeric
#'
#' @description Return values close to 0 if the shape is a circle and 1
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
ecc_metric.numeric <- function(timeseries) {

  polygon <- create_polygon(timeseries)

  pts_mbr <- MBR(sf::st_coordinates(polygon)[,1:2])

  bbox_pts <- sf::st_bbox(pts_mbr)

  axis1 = bbox_pts[["xmax"]] - bbox_pts[["xmin"]]
  axis2 = bbox_pts[["ymax"]] - bbox_pts[["ymin"]]
  stats = c(axis1, axis2)

  return(min(stats) / max(stats))

}
#' @title ...
#' @name MBR
#'
#' @note é necessária a instação do pacote grDevices para o método chull
#'
#' @param p ...
#'
#' @return ...
MBR <- function(p) {
  # Analyze the convex hull edges
  browser(0)
  a <- grDevices::chull(p)                        # Indexes of extremal points
  a <- c(a, a[1])                                 # Close the loop
  e <- p[a[-1],] - p[a[-length(a)], ]             # Edge directions
  norms <- sqrt(rowSums(e^2))                     # Edge lengths
  v <- e / norms                                  # Unit edge directions
  w <- cbind(-v[,2], v[,1])                       # Normal directions to the edges

  # Find the MBR
  vertices <- p[a, ]                              # Convex hull vertices
  x <- apply(vertices %*% t(v), 2, range)         # Extremes along edges
  y <- apply(vertices %*% t(w), 2, range)         # Extremes normal to edges
  areas <- (y[1,]-y[2,])*(x[1,]-x[2,])            # Areas
  k <- which.min(areas)                           # Index of the best edge (smallest area)

  # Form a rectangle from the extremes of the best edge
  pts <- cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% rbind(v[k,], w[k,])

  sf::st_polygon(list(pts))
}
# ecc_metric.sfc_POLYGON <- function(polygon) {
#
#   polygon <- create_polygon(timeseries)
#
#   pts_mbr <- MBR(sf::st_coordinates(polygon)[,1:2])
#
#   bbox_pts <- sf::st_bbox(pts_mbr)
#
#   axis1 = bbox_pts[["xmax"]] - bbox_pts[["xmin"]]
#   axis2 = bbox_pts[["ymax"]] - bbox_pts[["ymin"]]
#   stats = c(axis1, axis2)
#
#   return(min(stats) / max(stats))
#
# }
#' @title ...
#' @name gyration_radius
#'
#' @description Equals the average distance between each point inside
#'
#' @param type ...
#'
#' @return ...
#' @export
gyration_radius <- function(type) {
  UseMethod("gyration_radius", type)
}
#' @title ...
#' @name gyration_radius.POLYGON
#'
#' @description Equals the average distance between each point inside
#'
#' @param polygon ...
#'
#' @return ...
#' @export
gyration_radius.XY <- function(polygon) {

  pts_cent <- sf::st_coordinates(sf::st_centroid(polygon))
  lonc <- pts_cent[[1]]
  latc <- pts_cent[[2]]
  pts_line <- sf::st_coordinates(sf::st_cast(polygon, "LINESTRING"))[,1:2]

  dist_values <- apply(pts_line, 1,function(pts) {
    c(sqrt((pts[[1]] - lonc)^2 + (pts[[2]] - latc)^2))
  })
  return(mean(dist_values))
}
#' @title ...
#' @name gyration_radius.sfc_POLYGON
#'
#' @description Equals the average distance between each point inside
#'
#' @param polygon ...
#'
#' @return ...
#' @export
gyration_radius.sfc_POLYGON <- function(polygon) {

  pts_cent <- sf::st_coordinates(sf::st_centroid(polygon))
  # lonc <- pts_cent[[1]]
  # latc <- pts_cent[[2]]
  # TODO: ver se realmente precisa fazer o cast pra linestring
  pts_line <- sf::st_coordinates(sf::st_cast(polygon, "LINESTRING"))



  dist_values <- apply(pts_line, 1,function(pts) {
    c(sqrt((pts[[1]] - lonc)^2 + (pts[[2]] - latc)^2))
  })
  return(mean(dist_values))
}
#' @title ...
#' @name gyration_radius.numeric
#'
#' @description Equals the average distance between each point inside
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
gyration_radius.numeric <- function(timeseries) {
  browser()
  polygon <- create_polygon(timeseries)

  pts_cent <- sf::st_coordinates(sf::st_centroid(polygon))
  lonc <- pts_cent[[1]]
  latc <- pts_cent[[2]]
  pts_line <- sf::st_coordinates(sf::st_cast(polygon, "LINESTRING"))[,1:2]

  dist_values <- apply(pts_line, 1,function(pts) {
    c(sqrt((pts[[1]] - lonc)^2 + (pts[[2]] - latc)^2))
  })
  return(mean(dist_values))
}
#' @title ...
#' @name csi
#'
#' @description This is a dimensionless quantitative measure of morphology
#'
#' @param type ...
#'
#' @return ...
#' @export
csi <- function(type) {
  UseMethod("csi", type)
}
#' @title ...
#' @name csi.POLYGON
#'
#' @description This is a dimensionless quantitative measure of morphology
#'
#' @param polygon ...
#'
#' @return ...
#' @export
csi.XY <- function(polygon) {

  ls <- sf::st_cast(polygon, "LINESTRING")

  (sf::st_length(ls) ^ 2)/(4 * pi * sf::st_area(polygon))
}
#' @title ...
#' @name csi.sfc_POLYGON
#'
#' @description This is a dimensionless quantitative measure of morphology
#'
#' @param polygon ...
#'
#' @return ...
#' @export
csi.sfc_POLYGON <- function(polygon) {

  ls <- sf::st_cast(polygon, "LINESTRING")

  calc_csi(sf::st_length(ls), sf::st_area(polygon))
}
#' @title ...
#' @name csi.numeric
#'
#' @description This is a dimensionless quantitative measure of morphology
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
csi.numeric <- function(timeseries) {

  polygon <- create_polygon(timeseries)
  ls <- sf::st_cast(polygon, "LINESTRING")

  (sf::st_length(ls) ^ 2)/(4 * pi * sf::st_area(polygon))
}

#' @title ...
#' @name csi.matrix
#'
#' @description This is a dimensionless quantitative measure of morphology
#'
#' @param timeseries ...
#'
#' @return ...
#' @export
csi.matrix <- function(timeseries) {

  polygon <- create_polygon_v2(timeseries)
  ls <- sf::st_cast(polygon, "LINESTRING")

  calc_csi(sf::st_length(ls), sf::st_area(polygon))
}
