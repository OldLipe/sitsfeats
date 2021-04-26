#' @title R package for extracting metrics from time series of satellite images
#'
#' @section The \code{sitsfeats} functions:
#' The sitsfeats package provides two type of metrics:
#' \itemize{
#'   \item \code{basics}: The basic metrics provide basic statistical functions,
#'    such as mean, median.
#'   \item \code{polar}: Polar metrics provide functions based on polar
#'   coordinates, implemented by
#'   Korting, T. S., Fonseca, L. M. G., & Câmara, G. (2013).
#' }
#'
#' @name sitsfeats
"_PACKAGE"
NULL

#' @importFrom sf st_coordinates st_crop st_area st_as_sf st_geometry st_length
#' @importFrom sfheaders sf_polygon sf_cast
#' @importFrom ggplot2 ggplot aes geom_sf scale_x_continuous scale_y_continuous
#'  theme guides
#' @importFrom cowplot theme_cowplot panel_border
NULL

## usethis namespace: start
#' @useDynLib sitsfeats, .registration = TRUE
## usethis namespace: end
NULL
