#' @title Creates a polar plot based on areas ...
#' @name polar_plot
#'
#' @param timeseries   a \code{numeric} or \code{matrix} object where the
#'  columns is the point in time.
#'
#' @return a polygon object from \code{sf} class
#' @export
polar_plot <- function(timeseries) {

  polygon <- create_polygon(timeseries)

  if (nrow(polygon) > 1) {
    warning("Only first time series will be used.")
    polygon <- polygon[1, ]
  }

  # get coordinates from points
  pts_poly <- sf::st_coordinates(polygon)

  # get time series instances
  instances_time <- nrow(sf::st_coordinates(polygon)) - 1

  pts_bbox <- get_seasons_fast(pts_poly, instances_time)

  matrix_poly <- rbind(polytopleft(pts_bbox),
                       polytopright(pts_bbox),
                       polybottomleft(pts_bbox),
                       polybottomright(pts_bbox))

  matrix_poly[,3] <- rep(1:4, each = 5)

  # creating a sf object with 4 quadrants
  poly <- sfheaders::sf_polygon(matrix_poly, polygon_id = 3)
  poly$quadrant <- c("Q1", "Q2", "Q3", "Q4")


  polar_gg <- ggplot2::ggplot(poly) +
    ggplot2::geom_sf(ggplot2::aes_string(fill = "quadrant"), alpha = 0.1) +
    ggplot2::geom_sf(data = polygon, fill = "darkblue", alpha = 0.4) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    cowplot::theme_cowplot() +
    cowplot::panel_border(color = "black") +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 16, colour = "black"),
      strip.text.y = ggplot2::element_text(size = 16, colour = "black"),
      plot.title = ggplot2::element_text(size = 12,
                                         hjust = 0.5,
                                         margin = ggplot2::margin(b = 7)),
      axis.text.x = ggplot2::element_text(angle = 0,
                                          hjust = 0.5),
      strip.background = ggplot2::element_blank()) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(nrow = 3, override.aes = list(size = 2))) +
    ggplot2::labs(fill = "Area")

  polar_gg
}
