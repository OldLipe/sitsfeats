test_that("General errors", {
  data("timeseries", package = "sitsfeats")

  # error param
  testthat::expect_error(area_q1(as.character(timeseries)))
})

test_that("area metrics - matrix", {
  data("timeseries", package = "sitsfeats")

  # area q1
  testthat::expect_equal(nrow(area_q1(timeseries)),
                          expect = nrow(timeseries))

  # area q2
  testthat::expect_equal(nrow(area_q2(timeseries)),
                          expect = nrow(timeseries))

  # area q3
  testthat::expect_equal(nrow(area_q3(timeseries)),
                          expect = nrow(timeseries))
  # area q4
  testthat::expect_equal(nrow(area_q4(timeseries)),
                          expect = nrow(timeseries))

  # polar metrics
  testthat::expect_equal(nrow(polar_balance(timeseries)),
                          expect = nrow(timeseries))

})

test_that("area metrics - numeric", {
  data("timeseries", package = "sitsfeats")

  timeseries <- timeseries[1,]

  # area q1
  testthat::expect_length(nrow(area_q1(timeseries)), n = 1)

  # area q2
  testthat::expect_length(nrow(area_q2(timeseries)), n = 1)

  # area q3
  testthat::expect_length(nrow(area_q3(timeseries)), n = 1)

  # area q4
  testthat::expect_length(nrow(area_q4(timeseries)), n = 1)

  # polar metrics
  testthat::expect_length(nrow(polar_balance(timeseries)), n = 1)
})

test_that("temporal metrics - matrix", {
  data("timeseries", package = "sitsfeats")

  # angle
  testthat::expect_equal(nrow(angle(timeseries)),
                          expect = nrow(timeseries))

  # area_ts
  testthat::expect_equal(nrow(area_ts(timeseries)),
                          expect = nrow(timeseries))

  # ecc_metric
  testthat::expect_equal(nrow(ecc_metric(timeseries)),
                          expect = nrow(timeseries))

  # gyration_radius
  testthat::expect_equal(nrow(gyration_radius(timeseries)),
                          expect = nrow(timeseries))

  # csi
  testthat::expect_equal(nrow(csi(timeseries)),
                          expect = nrow(timeseries))

})

test_that("temporal metrics - numeric", {
  data("timeseries", package = "sitsfeats")

  timeseries <- timeseries[1,]

  # angle
  testthat::expect_length(nrow(angle(timeseries)), n = 1)

  # area_ts
  testthat::expect_length(nrow(area_ts(timeseries)), n = 1)

  # ecc_metric
  testthat::expect_length(nrow(ecc_metric(timeseries)), n = 1)

  # gyration_radius
  testthat::expect_length(nrow(gyration_radius(timeseries)), n = 1)

  # csi
  testthat::expect_length(nrow(csi(timeseries)), n = 1)
})
test_that("temporal metrics - numeric", {

  data("timeseries", package = "sitsfeats")

  # warning when more than one time series is provided
  testthat::expect_warning(polar_plot(timeseries))

  # dd
  testthat::expect_s3_class(polar_plot(timeseries[1,]), class = "gg")
})
