#' @title ...
#' @name add_prefix_column ...
#'
#' @param time_series ...
#' @return ....
add_prefix_column <- function(time_series) {

  time_series <- lapply(names(time_series), function(name) {
    tibble::as_tibble(time_series[[name]]) %>%
      dplyr::rename_with(~ paste0(name, "_", .x)) %>%
      as.matrix()
  })

  time_series <- do.call(cbind, lapply(time_series, list)) %>%
    tibble::as_tibble()

  return(time_series)
}
#' @title ...
#' @name extracting_metrics_basics ...
#'
#' @param sits_timeseries ...
#' @return ....
#' @export
extracting_metrics_basics <- function(sits_timeseries) {

  bands_name <- names(sits_timeseries$time_series[[1]])
  bands_name <- bands_name[!bands_name %in% "Index"]

  metrics_list <- purrr::map(bands_name, function(band) {
    # select the chosen bands for the time series
    sits_timeseries$time_series <- sits_timeseries$time_series %>%
      purrr::map(function(ts) ts[, c(bands_name[[1]])])

    ts <- sits_timeseries$time_series %>%
      purrr::map(function(ts) {
        as.matrix(t(unlist(ts)))
      })

    ts <- do.call(rbind, ts)

    list("max" = max_ts(ts),
         "min" = min_ts(ts),
         "sum" = sum_ts(ts),
         "mean" = mean_ts(ts),
         "std" = std_ts(ts),
         "skew" = skew_ts(ts),
         "kurt" = kurt_ts(ts),
         "amplitude" = amplitude_ts(ts),
         "fslope" = fslope_ts(ts),
         "abs_sum" = abs_sum_ts(ts),
         "amd" = amd_ts(ts),
         "mse" = mse_ts(ts),
         "fqr" = fqr_ts(ts),
         "tqr" = tqr_ts(ts),
         "sqr" = sqr_ts(ts),
         "iqr" = iqr_ts(ts))
  })

  names(metrics_list) <- bands_name

  class(metrics_list) <- c("metrics", class(metrics_list))

  return(metrics_list)
}

#' @title ....
#' @name rrf_selector
#'
#' @param list_metrics
#'
#' @return ....
rrf_selector <- function(list_metrics) {

  if (!"metrics" %in% class(list_metrics))
    stop("add erro")

  #tibble_metrics <- add_prefix_column(metrics_list)
}
#' @title ....
#' @name entropy_selector
#'
#' @param tibble_metrics
#'
#' @return ....
entropy_selector <- function(tibble_metrics) {
  #tibble_metrics <- add_prefix_column(metrics_list)

}

