# tests/testthat/test-check_gps.R
# Tests for GPS check functions (Phase 2)

# -- check_gps_accuracy -------------------------------------------------------
test_that("check_gps_accuracy flags readings above threshold", {
  df <- data.frame(
    hh_id    = paste0("H", 1:5),
    accuracy = c(10, 25, 50, 51, 100),
    stringsAsFactors = FALSE
  )

  res <- check_gps_accuracy(df, "hh_id", "accuracy", max_accuracy = 50)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "E03_gps_accuracy")
  expect_equal(res$check_category, "gps")
  expect_equal(res$severity, "warning")
  expect_equal(res$n_flagged, 2L)
  expect_true("H4" %in% res$flagged_ids)
  expect_true("H5" %in% res$flagged_ids)
  expect_false("H3" %in% res$flagged_ids)
})

test_that("check_gps_accuracy returns 0 flagged when all within threshold", {
  df <- data.frame(
    hh_id    = paste0("H", 1:3),
    accuracy = c(5, 10, 49),
    stringsAsFactors = FALSE
  )

  res <- check_gps_accuracy(df, "hh_id", "accuracy", max_accuracy = 50)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_gps_accuracy skips NA values", {
  df <- data.frame(
    hh_id    = paste0("H", 1:3),
    accuracy = c(NA, 100, NA),
    stringsAsFactors = FALSE
  )

  res <- check_gps_accuracy(df, "hh_id", "accuracy", max_accuracy = 50)
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "H2")
  expect_equal(res$n_total, 1L)
})

test_that("check_gps_accuracy summary_stat contains expected fields", {
  df <- data.frame(
    hh_id    = paste0("H", 1:3),
    accuracy = c(10, 30, 60),
    stringsAsFactors = FALSE
  )

  res <- check_gps_accuracy(df, "hh_id", "accuracy", max_accuracy = 50)
  expect_equal(res$summary_stat$max_accuracy, 50)
  expect_equal(res$summary_stat$median_accuracy, 30)
  expect_equal(res$summary_stat$max_observed, 60)
})

# -- check_gps_duplicates -----------------------------------------------------
test_that("check_gps_duplicates flags two points very close together", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    lat   = c(12.0000, 12.00005, 14.0),
    lon   = c(1.0000,  1.00005,  3.0),
    stringsAsFactors = FALSE
  )

  res <- check_gps_duplicates(df, "hh_id", "lat", "lon", min_distance = 0.0001)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "E04_gps_duplicates")
  expect_equal(res$n_flagged, 2L)
  expect_true("H1" %in% res$flagged_ids)
  expect_true("H2" %in% res$flagged_ids)
  expect_false("H3" %in% res$flagged_ids)
})

test_that("check_gps_duplicates returns 0 when points are far apart", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    lat   = c(10.0, 20.0, 30.0),
    lon   = c(1.0,  2.0,  3.0),
    stringsAsFactors = FALSE
  )

  res <- check_gps_duplicates(df, "hh_id", "lat", "lon")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_gps_duplicates handles single row without error", {
  df <- data.frame(hh_id = "H1", lat = 12.0, lon = 1.0, stringsAsFactors = FALSE)
  res <- check_gps_duplicates(df, "hh_id", "lat", "lon")
  expect_equal(res$n_flagged, 0L)
})

# -- check_gps_null_island ----------------------------------------------------
test_that("check_gps_null_island flags point at (0,0)", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    lat   = c(0,  12.5, 14.0),
    lon   = c(0,   1.5,  3.0),
    stringsAsFactors = FALSE
  )

  res <- check_gps_null_island(df, "hh_id", "lat", "lon")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "E07_gps_null_island")
  expect_equal(res$severity, "error")
  expect_true("H1" %in% res$flagged_ids)
  expect_false("H2" %in% res$flagged_ids)
})

test_that("check_gps_null_island flags 999 and -999 default values", {
  df <- data.frame(
    hh_id = paste0("H", 1:4),
    lat   = c(999,  -999, 12.0, 9999),
    lon   = c(10.0, 2.0,  1.5,  3.0),
    stringsAsFactors = FALSE
  )

  res <- check_gps_null_island(df, "hh_id", "lat", "lon")
  expect_equal(res$n_flagged, 3L)
  expect_true("H1" %in% res$flagged_ids)
  expect_true("H2" %in% res$flagged_ids)
  expect_true("H4" %in% res$flagged_ids)
})

test_that("check_gps_null_island returns 0 for valid coordinates", {
  df <- data.frame(
    hh_id = paste0("H", 1:2),
    lat   = c(12.5, 14.0),
    lon   = c(1.5,  3.0),
    stringsAsFactors = FALSE
  )

  res <- check_gps_null_island(df, "hh_id", "lat", "lon")
  expect_equal(res$n_flagged, 0L)
})

# -- check_gps_boundary -------------------------------------------------------
test_that("check_gps_boundary flags point outside bbox", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    lat   = c(12.0, 15.0, 20.0),
    lon   = c(1.0,  2.0,  10.0),
    stringsAsFactors = FALSE
  )

  bbox <- list(xmin = 0, xmax = 5, ymin = 10, ymax = 16)
  res <- check_gps_boundary(df, "hh_id", "lat", "lon", boundary_bbox = bbox)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "E01_gps_boundary")
  expect_equal(res$severity, "error")
  # H3 is outside (lat=20, lon=10)
  expect_true("H3" %in% res$flagged_ids)
  expect_false("H1" %in% res$flagged_ids)
})

test_that("check_gps_boundary returns 0 when all inside bbox", {
  df <- data.frame(
    hh_id = paste0("H", 1:2),
    lat   = c(12.0, 14.0),
    lon   = c(1.0,  2.0),
    stringsAsFactors = FALSE
  )

  bbox <- list(xmin = 0, xmax = 5, ymin = 10, ymax = 16)
  res <- check_gps_boundary(df, "hh_id", "lat", "lon", boundary_bbox = bbox)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_gps_boundary flags invalid global coordinates without bbox", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    lat   = c(12.0, 95.0, -91.0),
    lon   = c(1.0,  2.0,  3.0),
    stringsAsFactors = FALSE
  )

  res <- check_gps_boundary(df, "hh_id", "lat", "lon")
  expect_equal(res$n_flagged, 2L)
  expect_true("H2" %in% res$flagged_ids)
  expect_true("H3" %in% res$flagged_ids)
})

# -- check_gps_centroid_distance -----------------------------------------------
test_that("check_gps_centroid_distance flags distant point", {
  # Two points in cluster A: one nearby centroid, one far away
  df <- data.frame(
    hh_id   = paste0("H", 1:3),
    lat     = c(12.0, 12.001, 14.0),
    lon     = c(1.0,  1.001,  1.0),
    cluster = c("A",  "A",    "A"),
    stringsAsFactors = FALSE
  )

  # Centroid of A will be mean(12, 12.001, 14) ~ 12.667
  # H3 is far from centroid
  res <- check_gps_centroid_distance(df, "hh_id", "lat", "lon",
                                      "cluster", max_distance_km = 5)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "E02_gps_centroid_distance")
  # At least H3 should be flagged (>200 km from centroid)
  expect_true(res$n_flagged >= 1L)
  expect_true("H3" %in% res$flagged_ids)
})

test_that("check_gps_centroid_distance returns 0 when all close", {
  df <- data.frame(
    hh_id   = paste0("H", 1:3),
    lat     = c(12.000, 12.001, 12.002),
    lon     = c(1.000,  1.001,  1.002),
    cluster = c("A", "A", "A"),
    stringsAsFactors = FALSE
  )

  res <- check_gps_centroid_distance(df, "hh_id", "lat", "lon",
                                      "cluster", max_distance_km = 10)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_gps_centroid_distance handles all NA GPS", {
  df <- data.frame(
    hh_id   = paste0("H", 1:3),
    lat     = c(NA, NA, NA),
    lon     = c(NA, NA, NA),
    cluster = c("A", "A", "A"),
    stringsAsFactors = FALSE
  )

  res <- check_gps_centroid_distance(df, "hh_id", "lat", "lon", "cluster")
  expect_equal(res$n_flagged, 0L)
  expect_equal(res$n_total, 0L)
})

# -- check_gps_swap -----------------------------------------------------------
test_that("check_gps_swap flags impossible latitude (abs > 90) without bbox", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    lat   = c(12.0, 120.0, -91.0),
    lon   = c(1.0,  2.0,   3.0),
    stringsAsFactors = FALSE
  )

  res <- check_gps_swap(df, "hh_id", "lat", "lon")

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "E05_gps_swap")
  expect_equal(res$severity, "error")
  expect_equal(res$n_flagged, 2L)
  expect_true("H2" %in% res$flagged_ids)
  expect_true("H3" %in% res$flagged_ids)
})

test_that("check_gps_swap detects swapped coordinates with expected_bbox", {
  # Expected area: lat 10-15, lon 30-40
  # H2 has lat=35, lon=12 -- swapping would give lat=12, lon=35, inside bbox
  df <- data.frame(
    hh_id = paste0("H", 1:2),
    lat   = c(12.0, 35.0),
    lon   = c(35.0, 12.0),
    stringsAsFactors = FALSE
  )

  bbox <- list(lat_min = 10, lat_max = 15, lon_min = 30, lon_max = 40)
  res <- check_gps_swap(df, "hh_id", "lat", "lon", expected_bbox = bbox)
  expect_true("H2" %in% res$flagged_ids)
})

test_that("check_gps_swap returns 0 when latitudes are valid", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    lat   = c(12.0, 45.0, -33.0),
    lon   = c(1.0,  2.0,  150.0),
    stringsAsFactors = FALSE
  )

  res <- check_gps_swap(df, "hh_id", "lat", "lon")
  expect_equal(res$n_flagged, 0L)
})

# -- check_gps_clustering -----------------------------------------------------
test_that("check_gps_clustering flags many surveys at same GPS", {
  df <- data.frame(
    hh_id = paste0("H", 1:6),
    lat   = c(12.0, 12.0, 12.0, 12.0, 14.0, 15.0),
    lon   = c(1.0,  1.0,  1.0,  1.0,  2.0,  3.0),
    stringsAsFactors = FALSE
  )

  res <- check_gps_clustering(df, "hh_id", "lat", "lon", max_same_point = 3)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "E08_gps_clustering")
  expect_equal(res$severity, "warning")
  # H1-H4 are at same point (4 > max_same_point of 3)
  expect_equal(res$n_flagged, 4L)
  expect_true(all(c("H1", "H2", "H3", "H4") %in% res$flagged_ids))
})

test_that("check_gps_clustering returns 0 when within threshold", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    lat   = c(12.0, 12.0, 12.0),
    lon   = c(1.0,  1.0,  1.0),
    stringsAsFactors = FALSE
  )

  res <- check_gps_clustering(df, "hh_id", "lat", "lon", max_same_point = 3)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_gps_clustering respects enum_col filter", {
  df <- data.frame(
    hh_id   = paste0("H", 1:4),
    lat     = c(12.0, 12.0, 12.0, 12.0),
    lon     = c(1.0,  1.0,  1.0,  1.0),
    enum_id = c("E1", "E1", "E2", "E2"),
    stringsAsFactors = FALSE
  )

  # With enum_col: 4 at same point but split across 2 enumerators, so NOT flagged

  res <- check_gps_clustering(df, "hh_id", "lat", "lon",
                               enum_col = "enum_id", max_same_point = 3)
  expect_equal(res$n_flagged, 0L)
})

# -- check_gps_altitude -------------------------------------------------------
test_that("check_gps_altitude flags extreme altitude values", {
  df <- data.frame(
    hh_id    = paste0("H", 1:5),
    altitude = c(100, 500, -600, 7000, 300),
    stringsAsFactors = FALSE
  )

  res <- check_gps_altitude(df, "hh_id", "altitude",
                             min_alt = -500, max_alt = 6000)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "E10_gps_altitude")
  expect_equal(res$severity, "warning")
  expect_equal(res$n_flagged, 2L)
  expect_true("H3" %in% res$flagged_ids)  # -600 < -500
  expect_true("H4" %in% res$flagged_ids)  # 7000 > 6000
})

test_that("check_gps_altitude returns 0 when all within range", {
  df <- data.frame(
    hh_id    = paste0("H", 1:3),
    altitude = c(0, 500, 2000),
    stringsAsFactors = FALSE
  )

  res <- check_gps_altitude(df, "hh_id", "altitude")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_gps_altitude handles all NA values", {
  df <- data.frame(
    hh_id    = paste0("H", 1:3),
    altitude = c(NA_real_, NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )

  res <- check_gps_altitude(df, "hh_id", "altitude")
  expect_equal(res$n_flagged, 0L)
  expect_equal(res$n_total, 0L)
})

test_that("check_gps_altitude flag_reason mentions min/max", {
  df <- data.frame(hh_id = "H1", altitude = -1000, stringsAsFactors = FALSE)
  res <- check_gps_altitude(df, "hh_id", "altitude", min_alt = -500)
  expect_true(grepl("-500", res$flag_reason[1]))
  expect_true(grepl("-1000", res$flag_reason[1]))
})

# -- Edge case: all GPS NA across multiple checks ------------------------------
test_that("check_gps_duplicates handles all NA GPS gracefully", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    lat   = c(NA, NA, NA),
    lon   = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  res <- check_gps_duplicates(df, "hh_id", "lat", "lon")
  expect_equal(res$n_flagged, 0L)
  expect_equal(res$n_total, 0L)
})

test_that("check_gps_null_island handles all NA GPS gracefully", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    lat   = c(NA, NA, NA),
    lon   = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  res <- check_gps_null_island(df, "hh_id", "lat", "lon")
  expect_equal(res$n_flagged, 0L)
})

test_that("check_gps_clustering handles all NA GPS gracefully", {
  df <- data.frame(
    hh_id = paste0("H", 1:3),
    lat   = c(NA, NA, NA),
    lon   = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  res <- check_gps_clustering(df, "hh_id", "lat", "lon")
  expect_equal(res$n_flagged, 0L)
  expect_equal(res$n_total, 0L)
})
