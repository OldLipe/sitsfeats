#' @title Catch22 metrics
#'
#' @name catch22_metrics
#'
#' @description
#' The package sitsfeats uses Rcatch22 package to provide catch22 metrics:
#' \itemize{
#'   \item ...
#'   \item ...
#'  }
#'
#' @param timeseries   a \code{numeric} or \code{matrix} object where the
#'  columns is the point in time.
#'
#' @examples
#' data("timeseries")
#' mean_values <- sitsfeats::dn_histmode_5(timeseries)
#'
#' @return a \code{numeric} vector for each metric in each time series
#'
#' @export
dn_histmode_5 <- function(timeseries) {

  apply(timeseries, 1, Rcatch22::DN_HistogramMode_5)
}
