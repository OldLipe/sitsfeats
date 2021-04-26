test_that("General errors", {
  data("timeseries", package = "sitsfeats")

  # error param
  testthat::expect_error(area_q1(as.character(timeseries)))
})

test_that("area metrics - matrix", {
  data("timeseries", package = "sitsfeats")

  # area q1
  testthat::expect_vector(area_q1(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # area q2
  testthat::expect_vector(area_q2(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # area q3
  testthat::expect_vector(area_q3(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # area q4
  testthat::expect_vector(area_q4(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # polar metrics
  testthat::expect_vector(polar_balance(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

})

test_that("area metrics - numeric", {
  data("timeseries", package = "sitsfeats")

  timeseries <- timeseries[1,]

  # area q1
  testthat::expect_vector(area_q1(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # area q2
  testthat::expect_vector(area_q2(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # area q3
  testthat::expect_vector(area_q3(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # area q4
  testthat::expect_vector(area_q4(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # polar metrics
  testthat::expect_vector(polar_balance(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

})

test_that("temporal metrics - matrix", {
  data("timeseries", package = "sitsfeats")

  # angle
  testthat::expect_vector(angle(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # area_ts
  testthat::expect_vector(area_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # ecc_metric
  testthat::expect_vector(ecc_metric(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # gyration_radius
  testthat::expect_vector(gyration_radius(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # csi
  testthat::expect_vector(csi(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

})

test_that("temporal metrics - numeric", {
  data("timeseries", package = "sitsfeats")

  timeseries <- timeseries[1,]

  # angle
  testthat::expect_vector(angle(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # area_ts
  testthat::expect_vector(area_ts(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))
  # ecc_metric
  testthat::expect_vector(ecc_metric(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # gyration_radius
  testthat::expect_vector(gyration_radius(timeseries),
                          ptype = numeric(),
                          size = nrow(timeseries))

  # csi
  testthat::expect_vector(csi(timeseries),
                           ptype = numeric(),
                           size = nrow(timeseries))
})
