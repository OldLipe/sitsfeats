test_that("basics metrics - matrix", {
  data("timeseries", package = "sitsfeats")

  # max_ts
  testthat::expect_vector(max_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # min_ts
  testthat::expect_vector(min_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # mean_ts
  testthat::expect_vector(mean_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # median_ts
  testthat::expect_vector(median_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # sum_ts
  testthat::expect_vector(sum_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # std_ts
  testthat::expect_vector(std_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # skew_ts
  testthat::expect_vector(skew_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # kurt_ts
  testthat::expect_vector(kurt_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # amplitude_ts
  testthat::expect_vector(amplitude_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # fslope_ts
  testthat::expect_vector(fslope_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # abs_sum_ts
  testthat::expect_vector(fslope_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # amd_ts
  testthat::expect_vector(amd_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # mse_ts
  testthat::expect_vector(mse_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # fqr_ts
  testthat::expect_vector(fqr_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # sqr_ts
  testthat::expect_vector(sqr_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # tqr_ts
  testthat::expect_vector(tqr_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # iqr_ts
  testthat::expect_vector(iqr_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

})

test_that("basics metrics - numeric", {
  data("timeseries", package = "sitsfeats")

  timeseries <- timeseries[1,]

  # max_ts
  testthat::expect_vector(max_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # min_ts
  testthat::expect_vector(min_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # mean_ts
  testthat::expect_vector(mean_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # median_ts
  testthat::expect_vector(median_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # sum_ts
  testthat::expect_vector(sum_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # std_ts
  testthat::expect_vector(std_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # skew_ts
  testthat::expect_vector(skew_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # kurt_ts
  testthat::expect_vector(kurt_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # amplitude_ts
  testthat::expect_vector(amplitude_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # fslope_ts
  testthat::expect_vector(fslope_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # abs_sum_ts
  testthat::expect_vector(fslope_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # amd_ts
  testthat::expect_vector(amd_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # mse_ts
  testthat::expect_vector(mse_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # fqr_ts
  testthat::expect_vector(fqr_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # sqr_ts
  testthat::expect_vector(sqr_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # tqr_ts
  testthat::expect_vector(tqr_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # iqr_ts
  testthat::expect_vector(iqr_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))


})
