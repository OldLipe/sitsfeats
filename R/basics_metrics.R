#' @title Basics metrics
#'
#' @name basics_metrics
#'
#' @description
#' The package sitsfeats provides a set of basics metrics:
#' \itemize{
#'   \item The \code{max_ts()} retrieves the maximum value contained in the time
#'    series.
#'   \item The \code{min_ts()} retrieves the minimum value contained in the time
#'    series.
#'   \item The \code{mean_ts()} retrieves the mean value of the time series.
#'   \item The \code{median_ts()} retrieves the median value of the time series.
#'   \item The \code{sum_ts()} retrieves the sum of all time series points.
#'   \item The \code{std_ts()} retrieves the standard deviation of the time
#'   series.
#'   \item The \code{skew_ts()} retrieves the skewness of the time series.
#'   \item The \code{kurt_ts()} retrieves the kurtosis of the time series.
#'   \item The \code{amplitude_ts()} retrieves the amplitude of the time series.
#'   \item The \code{fslope_ts()} retrieves the first slope of the time series.
#'   \item The \code{abs_sum_ts()} retrieves the absolute sum of the time series
#'    points.
#'   \item The \code{amd_ts()} retrieves the absolute mean of the difference
#'   between each point in the time series.
#'   \item The \code{mse_ts()} retrieves the mean spectral energy of the time
#'   series.
#'   \item The \code{fqr_ts()} retrieves the value of the first quartile of the
#'   time series (0.25).
#'   \item The \code{sqr_ts()} retrieves the value of the second quartile of the
#'    time series (0.50).
#'   \item The \code{tqr_ts()} retrieves the value of the third quartile of the
#'    time series (0.75).
#'   \item The \code{iqr_ts()} retrieves the interquartile range
#'   (difference between the third and first quartile).
#'  }
#'
#' @param timeseries   a \code{numeric} or \code{matrix} object where the
#'  columns is the point in time.
#'
#' @examples
#' data("timeseries")
#' mean_values <- sitsfeats::mean_ts(timeseries)
#'
#' @return a \code{numeric} vector for each metric in each time series
#'
#' @export
max_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  max_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
min_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  min_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
mean_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  mean_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
median_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  median_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
sum_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  sum_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
std_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  std_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
skew_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  skew_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
kurt_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  kurt_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
amplitude_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  amplitude_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
fslope_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  fslope_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
abs_sum_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  abs_sum_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
amd_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  amd_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
mse_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  mse_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
fqr_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  fqr_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
sqr_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  sqr_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
tqr_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  tqr_calc(timeseries)
}

#' @rdname basics_metrics
#' @export
iqr_ts <- function(timeseries) {

  # assert that only supported types are provided
  timeseries <- .verify_timeseries(timeseries)

  iqr_calc(timeseries)
}
