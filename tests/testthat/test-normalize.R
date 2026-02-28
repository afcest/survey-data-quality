# tests/testthat/test-normalize.R
# Tests for detect_platform(), parse_gps_string(), normalize_survey_data()

# -- detect_platform -----------------------------------------------------------
test_that("detect_platform detects KoBo column patterns", {
  kobo_df <- data.frame(
    `_id` = 1, `_uuid` = "u1", `_submission_time` = "2025-01-01",
    `_index` = 1, hh_id = "H1", name = "Test", `_geopoint` = "0 0",
    check.names = FALSE
  )
  result <- detect_platform(kobo_df)
  expect_equal(result, "kobo")
})

test_that("detect_platform detects SurveyCTO column patterns", {
  scto_df <- data.frame(
    KEY = "k1", submissiondate = "2025-01-01", starttime = "12:00",
    endtime = "13:00", deviceid = "d1", hh_id = "H1"
  )
  result <- detect_platform(scto_df)
  expect_equal(result, "surveycto")
})

test_that("detect_platform detects ODK column patterns", {
  odk_df <- data.frame(
    `__id` = "i1", `__system` = "s1", `__created` = "2025-01-01",
    `meta-instanceID` = "m1", hh_id = "H1", gps = "0 0",
    check.names = FALSE
  )
  result <- detect_platform(odk_df)
  expect_equal(result, "odk")
})

test_that("detect_platform detects Survey Solutions column patterns", {
  ss_df <- data.frame(
    interview__id = "i1", interview__key = "k1", sssys_irnd = 1,
    has__errors = FALSE, hh_id = "H1"
  )
  result <- detect_platform(ss_df)
  expect_equal(result, "survey_solutions")
})

test_that("detect_platform detects CSPro column patterns", {
  cspro_df <- data.frame(
    case_id = "c1", PROVINCE = "P1", DISTRICT = "D1",
    EA = "E1", HH_NUM = 1
  )
  result <- detect_platform(cspro_df)
  expect_equal(result, "cspro")
})

test_that("detect_platform returns 'unknown' for unrecognized columns", {
  generic_df <- data.frame(x1 = 1, x2 = 2, x3 = 3, x4 = 4)
  result <- detect_platform(generic_df)
  expect_equal(result, "unknown")
})

# -- parse_gps_string ----------------------------------------------------------
test_that("parse_gps_string parses KoBo-style 'lat lon alt acc' strings", {
  gps_strings <- c(
    "12.3456 -1.5678 300 5",
    "13.0000 -2.0000 250 10",
    NA
  )
  result <- parse_gps_string(gps_strings)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("latitude", "longitude") %in% names(result)))
  expect_equal(nrow(result), 3L)
  expect_equal(result$latitude[1], 12.3456)
  expect_equal(result$longitude[1], -1.5678)
  expect_true(is.na(result$latitude[3]))
})

test_that("parse_gps_string handles empty strings", {
  result <- parse_gps_string(c("", "  "))
  expect_true(all(is.na(result$latitude)))
  expect_true(all(is.na(result$longitude)))
})

# -- normalize_survey_data -----------------------------------------------------
test_that("normalize_survey_data normalizes a mock KoBo dataset", {
  kobo_df <- data.frame(
    `_id`              = 1:3,
    `_uuid`            = paste0("uuid-", 1:3),
    `_submission_time`  = as.character(Sys.time() + 1:3),
    hh_id              = c("HH001", "HH002", "HH003"),
    enumerator_id      = c("E1", "E2", "E1"),
    `_geopoint`         = c("12.34 -1.56 300 5",
                            "13.00 -2.00 250 10",
                            "11.50 -1.00 200 8"),
    income             = c(100, 200, 150),
    check.names        = FALSE,
    stringsAsFactors   = FALSE
  )

  result <- normalize_survey_data(
    kobo_df,
    platform = "kobo",
    id_col = "hh_id",
    enum_col = "enumerator_id",
    gps_col = "_geopoint"
  )

  expect_s3_class(result, "data.frame")
  expect_true(".latitude" %in% names(result))
  expect_true(".longitude" %in% names(result))
  # Original data columns should be preserved
  expect_true("hh_id" %in% names(result))
  expect_true("income" %in% names(result))
})

test_that("normalize_survey_data preserves row count", {
  df <- data.frame(
    `_id`         = 1:5,
    `_uuid`       = paste0("u", 1:5),
    hh_id         = paste0("H", 1:5),
    enumerator_id = rep("E1", 5),
    check.names   = FALSE,
    stringsAsFactors = FALSE
  )

  result <- normalize_survey_data(
    df,
    platform = "kobo",
    id_col = "hh_id",
    enum_col = "enumerator_id"
  )
  expect_equal(nrow(result), 5L)
})
