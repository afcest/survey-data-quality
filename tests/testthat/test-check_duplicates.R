# tests/testthat/test-check_duplicates.R
# Tests for check_duplicate_ids(), check_duplicate_fingerprint(),
# check_missing_ids(), check_id_in_sample()

# -- Helper data ---------------------------------------------------------------
make_dup_data <- function() {
  data.frame(
    hh_id        = c("A01", "A02", "A03", "A01", "A04", "A02"),
    gps_lat      = c(12.1, 12.2, 12.3, 12.1, 12.4, 12.2),
    gps_lon      = c(-1.5, -1.6, -1.7, -1.5, -1.8, -1.6),
    resp_name    = c("Ali", "Binta", "Cisse", "Ali", "Daouda", "Binta"),
    income       = c(100, 200, 300, 100, 400, 200),
    stringsAsFactors = FALSE
  )
}

# -- check_duplicate_ids -------------------------------------------------------
test_that("check_duplicate_ids flags known duplicates", {
  df <- make_dup_data()
  res <- check_duplicate_ids(df, "hh_id")

  expect_s3_class(res, "check_result")
  expect_true(res$check_name == "A01_duplicate_id")
  expect_true(res$severity == "error")
  # A01 and A02 are duplicated

  expect_true(res$n_flagged == 2L)
  expect_true("A01" %in% res$flagged_ids)
  expect_true("A02" %in% res$flagged_ids)
})

test_that("check_duplicate_ids returns zero flags with unique IDs", {
  df <- data.frame(hh_id = c("X1", "X2", "X3"), val = 1:3,
                   stringsAsFactors = FALSE)
  res <- check_duplicate_ids(df, "hh_id")

  expect_true(res$n_flagged == 0L)
  expect_true(length(res$flagged_ids) == 0L)
})

test_that("check_duplicate_ids errors on missing column", {
  df <- data.frame(id = 1:3)
  expect_error(check_duplicate_ids(df, "hh_id"), "Missing")
})

# -- check_duplicate_fingerprint -----------------------------------------------
test_that("check_duplicate_fingerprint flags matching quasi-identifiers", {
  # Same GPS+name but different IDs
  df <- data.frame(
    hh_id     = c("A01", "B01", "C01", "D01"),
    gps_lat   = c(12.1, 12.1, 12.3, 12.4),
    gps_lon   = c(-1.5, -1.5, -1.7, -1.8),
    resp_name = c("Ali", "Ali", "Cisse", "Daouda"),
    stringsAsFactors = FALSE
  )
  res <- check_duplicate_fingerprint(df, "hh_id",
                                     quasi_ids = c("gps_lat", "gps_lon", "resp_name"))

  expect_s3_class(res, "check_result")
  expect_true(res$check_name == "A02_fingerprint_duplicate")
  # A01 and B01 share the same fingerprint

  expect_true(res$n_flagged == 2L)
  expect_true("A01" %in% res$flagged_ids)
  expect_true("B01" %in% res$flagged_ids)
})

test_that("check_duplicate_fingerprint returns zero when all fingerprints unique", {
  df <- data.frame(
    hh_id   = c("A01", "B01"),
    gps_lat = c(12.1, 13.0),
    gps_lon = c(-1.5, -2.0),
    stringsAsFactors = FALSE
  )
  res <- check_duplicate_fingerprint(df, "hh_id",
                                     quasi_ids = c("gps_lat", "gps_lon"))
  expect_true(res$n_flagged == 0L)
})

# -- check_missing_ids ---------------------------------------------------------
test_that("check_missing_ids flags NA and empty IDs", {
  df <- data.frame(
    hh_id = c("A01", NA, "", ".", "A05"),
    val   = 1:5,
    stringsAsFactors = FALSE
  )
  res <- check_missing_ids(df, "hh_id")

  expect_s3_class(res, "check_result")
  expect_true(res$check_name == "A03_missing_id")
  expect_true(res$severity == "error")
  # NA, "", and "." should all be flagged
  expect_true(res$n_flagged == 3L)
  expect_true(identical(res$flagged_ids, c("row_2", "row_3", "row_4")))
})

test_that("check_missing_ids returns zero when no IDs missing", {
  df <- data.frame(hh_id = c("A", "B", "C"), val = 1:3,
                   stringsAsFactors = FALSE)
  res <- check_missing_ids(df, "hh_id")
  expect_true(res$n_flagged == 0L)
})

# -- check_id_in_sample -------------------------------------------------------
test_that("check_id_in_sample flags IDs not in the sampling frame", {
  df <- data.frame(
    hh_id = c("A01", "A02", "A03", "B99"),
    val   = 1:4,
    stringsAsFactors = FALSE
  )
  frame <- c("A01", "A02", "A03", "A04", "A05")

  res <- check_id_in_sample(df, "hh_id", sampling_frame = frame)

  expect_s3_class(res, "check_result")
  expect_true(res$check_name == "A05_id_not_in_sample")
  expect_true(res$severity == "error")
  expect_true(res$n_flagged == 1L)
  expect_true(identical(res$flagged_ids, "B99"))
})

test_that("check_id_in_sample returns zero when all IDs valid", {
  df <- data.frame(hh_id = c("A01", "A02"), val = 1:2,
                   stringsAsFactors = FALSE)
  frame <- c("A01", "A02", "A03")

  res <- check_id_in_sample(df, "hh_id", sampling_frame = frame)
  expect_true(res$n_flagged == 0L)
})

test_that("check_id_in_sample handles NA IDs gracefully", {
  df <- data.frame(hh_id = c("A01", NA, "B99"), val = 1:3,
                   stringsAsFactors = FALSE)
  frame <- c("A01", "A02")

  res <- check_id_in_sample(df, "hh_id", sampling_frame = frame)
  # Only B99 should be flagged (NA is excluded from the comparison)
  expect_true(res$n_flagged == 1L)
  expect_true(identical(res$flagged_ids, "B99"))
})

test_that("check_id_in_sample reports summary stats correctly", {
  df <- data.frame(hh_id = c("A01", "A02", "X99"), val = 1:3,
                   stringsAsFactors = FALSE)
  frame <- c("A01", "A02", "A03")

  res <- check_id_in_sample(df, "hh_id", sampling_frame = frame)
  expect_true(res$summary_stat$n_in_frame == 2L)
  expect_true(res$summary_stat$n_not_in_frame == 1L)
  expect_true(res$summary_stat$frame_size == 3L)
})
