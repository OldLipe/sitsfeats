#' @title ...
#' @name polar_plot
#'
#' @param timeseries ...
#'
#' @return a polygon object from \code{sf} class
polar_plot <- function(timeseries) {

  poly <- create_polygon(timeseries)

  list_poly <- get_seasons(sf::st_coordinates(poly)[,1],
                           sf::st_coordinates(poly)[,2])

  list_sfc <- do.call(sf::st_sfc, list_poly)

  g <- sf::st_sf(type = c("Q1",
                          "Q2",
                          "Q3",
                          "Q4"), list_sfc) %>%
    dplyr::mutate(x = c(sf::st_bbox(list_sfc)[["xmin"]] + 0.01,
                        sf::st_bbox(list_sfc)[["xmax"]] - 0.01,
                        sf::st_bbox(list_sfc)[["xmin"]] + 0.01,
                        sf::st_bbox(list_sfc)[["xmax"]] - 0.01),
                  y = c(sf::st_bbox(list_sfc)[["ymax"]] - 0.01,
                        sf::st_bbox(list_sfc)[["ymax"]] - 0.01,
                        sf::st_bbox(list_sfc)[["ymin"]] + 0.01,
                        sf::st_bbox(list_sfc)[["ymin"]] + 0.01))

  polar_gg <- ggplot(g) +
    geom_sf(aes(fill = type), alpha = 0.1) +
    geom_text(data = g, aes(x = x, y = y, label = type),
              label.padding = unit(1, "mm"),
              show.legend = FALSE) +
    geom_sf(data= poly, fill = "darkblue", alpha = 0.4) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    cowplot::theme_cowplot() +
    cowplot::panel_border(color = "black") +
    theme(strip.text.x = element_text(size = 16, colour = "black"),
          strip.text.y = element_text(size = 16, colour = "black"),
          plot.title = element_text(size = 12,
                                    hjust= 0.5,
                                    margin = margin(b = 7)),
          axis.text.x = element_text(angle = 0,
                                     hjust=0.5),
          strip.background = element_blank(),
          legend.text = element_blank()) +
    guides(colour = guide_legend(nrow = 3, override.aes = list(size = 2)))

  polar_gg
}
