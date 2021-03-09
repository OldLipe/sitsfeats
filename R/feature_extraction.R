#' @title wrapper filter uses a regularized random forest
#' @name wrapper_filter
#' @param x time series matrix
#' @param y label samples
#' @param flagReg if 1 is generated a RRF tree, otherwise a normal RF
#' @param ntree number of tree
#' @return a time series with subset selected
#' @export
wrapper_filter <- function(list_metrics, y, flagReg = 1, ntree = 500) {

  if (!"metrics" %in% class(list_metrics))
    stop("The list must be a list of metrics provided by sitsfeats.")

  tibble_metrics <- add_prefix_column(list_metrics)

  if (!is.factor(y))
    y <- as.factor(y)

  model_tree <- RRF::RRF(tibble_metrics, y, flagReg = flagReg, ntree = ntree)
  return(list(subset_samples = tibble_metrics[,model_tree$feaSet],
              rrf_model = model_tree))
}
#' @title Entropy filter uses information gain and other methods to determine
#'  more accurately feature selection.
#' @name entropy_filter
#' @param x time series matrix
#' @param y label samples
#' @param type a charactecter corresponding the method to select features
#' @param ntree number of tree
#' @return a time series with subset selected
#' @export
entropy_filter <- function(list_metrics, y, type = "infogain") {

  if (! "metrics" %in% class(list_metrics))
    stop("The list must be a list of metrics provided by sitsfeats.")

  tibble_metrics <- add_prefix_column(list_metrics)

  if (!is.factor(y))
    y <- as.factor(y)

  # extracted importance from attributes
  ig <- FSelectorRcpp::information_gain(x = tibble_metrics,
                                        y = y,
                                        type = type)

  # sorting and taking the first 9 features
  ig <- ig %>% dplyr::arrange(desc(importance))

  return(list(subset_samples = tibble_metrics[,ig$attributes], ig_model = ig))
}
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

  time_series <- do.call(cbind, lapply(time_series, as.data.frame)) %>%
    tibble::as_tibble()

  return(time_series)
}
#' @title ...
#' @name basics_metrics ...
#'
#' @param sits_timeseries ...
#' @return ....
#' @export
basics_metrics <- function(sits_timeseries) {

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

  class(metrics_list) <- c(class(metrics_list), "metrics")

  return(metrics_list)
}

#' #' @title ...
#' #' @name polar_metrics ...
#' #'
#' #' @param sits_timeseries ...
#' #' @return ....
#' #' @export
#' polar_metrics <- function(sits_timeseries) {
#'
#'   bands_name <- names(sits_timeseries$time_series[[1]])
#'   bands_name <- bands_name[!bands_name %in% "Index"]
#'
#'   metrics_list <- purrr::map(bands_name, function(band) {
#'     # select the chosen bands for the time series
#'     sits_timeseries$time_series <- sits_timeseries$time_series %>%
#'       purrr::map(function(ts) ts[, c(bands_name[[1]])])
#'
#'     ts <- sits_timeseries$time_series %>%
#'       purrr::map(function(ts) {
#'         as.matrix(t(unlist(ts)))
#'       })
#'
#'     ts <- do.call(rbind, ts)
#'
#'     list("max" = max_ts(ts),
#'          "min" = min_ts(ts),
#'          "sum" = sum_ts(ts),
#'          "mean" = mean_ts(ts),
#'          "std" = std_ts(ts),
#'          "skew" = skew_ts(ts),
#'          "kurt" = kurt_ts(ts),
#'          "amplitude" = amplitude_ts(ts),
#'          "fslope" = fslope_ts(ts),
#'          "abs_sum" = abs_sum_ts(ts),
#'          "amd" = amd_ts(ts),
#'          "mse" = mse_ts(ts),
#'          "fqr" = fqr_ts(ts),
#'          "tqr" = tqr_ts(ts),
#'          "sqr" = sqr_ts(ts),
#'          "iqr" = iqr_ts(ts))
#'   })
#'
#'   names(metrics_list) <- bands_name
#'
#'   class(metrics_list) <- c(class(metrics_list), "metrics")
#'
#'   return(metrics_list)
#' }
