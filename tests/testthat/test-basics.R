test_that("basics metrics - matrix", {
  data("timeseries", package = "sitsfeats")

  # max_ts
  testthat::expect_equal(nrow(max_ts(timeseries)),
                         expected = nrow(timeseries))
  # min_ts
  testthat::expect_equal(nrow(min_ts(timeseries)),
                         expected = nrow(timeseries))
  # mean_ts
  testthat::expect_equal(nrow(mean_ts(timeseries)),
                         expected = nrow(timeseries))
  # median_ts
  testthat::expect_equal(nrow(median_ts(timeseries)),
                         expected = nrow(timeseries))
  # sum_ts
  testthat::expect_equal(nrow(sum_ts(timeseries)),
                         expected = nrow(timeseries))
  # std_ts
  testthat::expect_equal(nrow(std_ts(timeseries)),
                         expected = nrow(timeseries))
  # skew_ts
  testthat::expect_equal(nrow(skew_ts(timeseries)),
                         expected = nrow(timeseries))
  # kurt_ts
  testthat::expect_equal(nrow(kurt_ts(timeseries)),
                         expected = nrow(timeseries))
  # amplitude_ts
  testthat::expect_equal(nrow(amplitude_ts(timeseries)),
                         expected = nrow(timeseries))
  # fslope_ts
  testthat::expect_equal(nrow(fslope_ts(timeseries)),
                         expected = nrow(timeseries))
  # abs_sum_ts
  testthat::expect_equal(nrow(fslope_ts(timeseries)),
                         expected = nrow(timeseries))
  # amd_ts
  testthat::expect_equal(nrow(amd_ts(timeseries)),
                         expected = nrow(timeseries))
  # mse_ts
  testthat::expect_equal(nrow(mse_ts(timeseries)),
                         expected = nrow(timeseries))

  # fqr_ts
  testthat::expect_equal(nrow(fqr_ts(timeseries)),
                         expected = nrow(timeseries))

  # sqr_ts
  testthat::expect_equal(nrow(sqr_ts(timeseries)),
                         expected = nrow(timeseries))

  # tqr_ts
  testthat::expect_equal(nrow(tqr_ts(timeseries)),
                         expected = nrow(timeseries))

  # iqr_ts
  testthat::expect_equal(nrow(iqr_ts(timeseries)),
                         expected = nrow(timeseries))

})

test_that("basics metrics - numeric", {
  data("timeseries", package = "sitsfeats")

  timeseries <- timeseries[1,]

  # max_ts
  testthat::expect_length(max_ts(timeseries), n = 1)

  # min_ts
  testthat::expect_length(min_ts(timeseries), n = 1)

  # mean_ts
  testthat::expect_length(mean_ts(timeseries), n = 1)

  # median_ts
  testthat::expect_length(median_ts(timeseries), n = 1)

  # sum_ts
  testthat::expect_length(sum_ts(timeseries), n = 1)

  # std_ts
  testthat::expect_length(std_ts(timeseries), n = 1)

  # skew_ts
  testthat::expect_length(skew_ts(timeseries), n = 1)

  # kurt_ts
  testthat::expect_length(kurt_ts(timeseries), n = 1)

  # amplitude_ts
  testthat::expect_length(amplitude_ts(timeseries), n = 1)

  # fslope_ts
  testthat::expect_length(fslope_ts(timeseries), n = 1)

  # abs_sum_ts
  testthat::expect_length(abs_sum_ts(timeseries), n = 1)

  # amd_ts
  testthat::expect_length(amd_ts(timeseries), n = 1)

  # mse_ts
  testthat::expect_length(mse_ts(timeseries), n = 1)

  # fqr_ts
  testthat::expect_length(fqr_ts(timeseries), n = 1)

  # sqr_ts
  testthat::expect_length(sqr_ts(timeseries), n = 1)

  # tqr_ts
  testthat::expect_length(tqr_ts(timeseries), n = 1)

  # iqr_ts
  testthat::expect_length(iqr_ts(timeseries), n = 1)
})
