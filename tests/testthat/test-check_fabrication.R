# tests/testthat/test-check_fabrication.R
# Tests for data fabrication detection check functions (Phase 2)

# -- check_benford_first_digit -------------------------------------------------
test_that("check_benford_first_digit does not flag Benford-conforming data", {
  # Generate data that follows Benford's Law using the inverse CDF method
  set.seed(42)
  n <- 2000
  # Benford-distributed first digits via sampling from log-uniform distribution
  benford_vals <- 10^runif(n, 0, 6)
  df <- data.frame(
    hh_id  = paste0("H", seq_len(n)),
    amount = round(benford_vals),
    stringsAsFactors = FALSE
  )

  res <- check_benford_first_digit(df, "hh_id", "amount", alpha = 0.05)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "M01_benford_first_digit")
  expect_equal(res$check_category, "fabrication")
  expect_equal(res$severity, "warning")
  # 1:1000 conforms well to Benford, should NOT be flagged

  expect_equal(res$n_flagged, 0L)
})

test_that("check_benford_first_digit flags non-conforming data", {
  # All values start with digit 5 -- heavily deviates from Benford
  df <- data.frame(
    hh_id  = paste0("H", seq_len(200)),
    amount = rep(c(50, 51, 52, 53, 54, 55, 56, 57, 58, 59), 20),
    stringsAsFactors = FALSE
  )

  res <- check_benford_first_digit(df, "hh_id", "amount", alpha = 0.05)
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "amount")
  expect_true(grepl("Benford", res$flag_reason[1]))
})

test_that("check_benford_first_digit returns n_total = 1 (variable-level check)", {
  df <- data.frame(
    hh_id  = paste0("H", 1:100),
    amount = 1:100,
    stringsAsFactors = FALSE
  )

  res <- check_benford_first_digit(df, "hh_id", "amount")
  expect_equal(res$n_total, 1L)
})

test_that("check_benford_first_digit handles too few observations", {
  df <- data.frame(
    hh_id  = paste0("H", 1:5),
    amount = c(10, 20, 30, 40, 50),
    stringsAsFactors = FALSE
  )

  res <- check_benford_first_digit(df, "hh_id", "amount")
  # Too few observations -> no test, no flag
  expect_equal(res$n_flagged, 0L)
  expect_true(is.na(res$summary_stat$p_value))
})

# -- check_benford_second_digit ------------------------------------------------
test_that("check_benford_second_digit handles natural data without flagging", {
  df <- data.frame(
    hh_id  = paste0("H", seq_len(500)),
    amount = 10:509,
    stringsAsFactors = FALSE
  )

  res <- check_benford_second_digit(df, "hh_id", "amount", alpha = 0.01)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "M02_benford_second_digit")
  expect_equal(res$check_category, "fabrication")
})

test_that("check_benford_second_digit flags uniform second digits", {
  # All values have second digit = 5 (15, 25, 35, ...)
  df <- data.frame(
    hh_id  = paste0("H", seq_len(200)),
    amount = rep(c(15, 25, 35, 45, 55, 65, 75, 85, 95), length.out = 200),
    stringsAsFactors = FALSE
  )

  res <- check_benford_second_digit(df, "hh_id", "amount", alpha = 0.05)
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "amount")
})

test_that("check_benford_second_digit filters out single-digit values", {
  # Only values < 10 should be excluded (need at least 2 digits)
  df <- data.frame(
    hh_id  = paste0("H", 1:5),
    amount = c(1, 2, 3, 4, 5),
    stringsAsFactors = FALSE
  )

  res <- check_benford_second_digit(df, "hh_id", "amount")
  expect_equal(res$n_flagged, 0L)
  expect_equal(res$summary_stat$n_values, 0L)
})

# -- check_digit_preference ---------------------------------------------------
test_that("check_digit_preference flags heaped terminal digits", {
  # All values end in 0 -> extreme heaping
  df <- data.frame(
    hh_id  = paste0("H", seq_len(200)),
    amount = seq(10, 2000, by = 10),
    stringsAsFactors = FALSE
  )

  res <- check_digit_preference(df, "hh_id", "amount", alpha = 0.05)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "M03_digit_preference")
  expect_equal(res$check_category, "fabrication")
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "amount")
  expect_true(grepl("heaping", res$flag_reason[1]))
})

test_that("check_digit_preference does not flag uniform digits", {
  # Values 1-200: terminal digits should be roughly uniform
  df <- data.frame(
    hh_id  = paste0("H", seq_len(200)),
    amount = 1:200,
    stringsAsFactors = FALSE
  )

  res <- check_digit_preference(df, "hh_id", "amount", alpha = 0.05)
  expect_equal(res$n_flagged, 0L)
})

# -- check_straightlining -----------------------------------------------------
test_that("check_straightlining flags all-same-answer survey", {
  df <- data.frame(
    hh_id = c("H1", "H2"),
    q1    = c(3, 1),
    q2    = c(3, 2),
    q3    = c(3, 3),
    q4    = c(3, 4),
    q5    = c(3, 5),
    stringsAsFactors = FALSE
  )

  res <- check_straightlining(df, "hh_id", c("q1", "q2", "q3", "q4", "q5"),
                               max_identical_rate = 0.8)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "B09_straightlining")
  # H1 has 100% identical (all 3), H2 has 20% max
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "H1")
})

test_that("check_straightlining returns 0 for diverse answers", {
  df <- data.frame(
    hh_id = c("H1", "H2"),
    q1    = c(1, 2),
    q2    = c(2, 3),
    q3    = c(3, 4),
    q4    = c(4, 5),
    q5    = c(5, 1),
    stringsAsFactors = FALSE
  )

  res <- check_straightlining(df, "hh_id", c("q1", "q2", "q3", "q4", "q5"),
                               max_identical_rate = 0.8)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_straightlining handles NA values in responses", {
  # Note: NA becomes "NA" string via as.character(), so 9 out of 10 values are "3"
  # giving rate = 9/10 = 0.9 > 0.8
  df <- data.frame(
    hh_id = "H1",
    q1    = 3, q2 = 3, q3 = 3, q4 = 3, q5 = 3,
    q6    = 3, q7 = 3, q8 = 3, q9 = 3, q10 = NA,
    stringsAsFactors = FALSE
  )

  res <- check_straightlining(df, "hh_id",
                               c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10"),
                               max_identical_rate = 0.8)
  expect_true(res$n_flagged == 1L)
})

# -- check_response_entropy ---------------------------------------------------
test_that("check_response_entropy flags low entropy survey", {
  df <- data.frame(
    hh_id = c("H1", "H2"),
    q1    = c(1, 1),
    q2    = c(1, 2),
    q3    = c(1, 3),
    q4    = c(1, 4),
    q5    = c(1, 5),
    stringsAsFactors = FALSE
  )

  res <- check_response_entropy(df, "hh_id",
                                 c("q1", "q2", "q3", "q4", "q5"),
                                 min_entropy_ratio = 0.3)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "M06_response_entropy")
  # H1 has entropy = 0 (all same), H2 has max entropy
  expect_true("H1" %in% res$flagged_ids)
  expect_false("H2" %in% res$flagged_ids)
})

test_that("check_response_entropy returns 0 for high entropy data", {
  df <- data.frame(
    hh_id = c("H1", "H2"),
    q1    = c(1, 5),
    q2    = c(2, 4),
    q3    = c(3, 3),
    q4    = c(4, 2),
    q5    = c(5, 1),
    stringsAsFactors = FALSE
  )

  res <- check_response_entropy(df, "hh_id",
                                 c("q1", "q2", "q3", "q4", "q5"),
                                 min_entropy_ratio = 0.3)
  expect_equal(res$n_flagged, 0L)
})

# -- check_duplicate_response_patterns ----------------------------------------
test_that("check_duplicate_response_patterns flags identical surveys", {
  df <- data.frame(
    hh_id = c("H1", "H2", "H3"),
    q1    = c(1, 1, 2),
    q2    = c(2, 2, 3),
    q3    = c(3, 3, 4),
    stringsAsFactors = FALSE
  )

  res <- check_duplicate_response_patterns(
    df, "hh_id", c("q1", "q2", "q3"), similarity_threshold = 0.95
  )

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "M07_duplicate_response_patterns")
  expect_equal(res$severity, "error")
  # H1 and H2 are identical
  expect_true("H1" %in% res$flagged_ids)
  expect_true("H2" %in% res$flagged_ids)
  expect_false("H3" %in% res$flagged_ids)
})

test_that("check_duplicate_response_patterns returns 0 for unique surveys", {
  df <- data.frame(
    hh_id = c("H1", "H2", "H3"),
    q1    = c(1, 2, 3),
    q2    = c(4, 5, 6),
    q3    = c(7, 8, 9),
    stringsAsFactors = FALSE
  )

  res <- check_duplicate_response_patterns(
    df, "hh_id", c("q1", "q2", "q3"), similarity_threshold = 0.95
  )
  expect_equal(res$n_flagged, 0L)
})

test_that("check_duplicate_response_patterns n_pairs_flagged is correct", {
  df <- data.frame(
    hh_id = c("H1", "H2", "H3"),
    q1    = c(1, 1, 1),
    q2    = c(2, 2, 2),
    q3    = c(3, 3, 3),
    stringsAsFactors = FALSE
  )

  res <- check_duplicate_response_patterns(
    df, "hh_id", c("q1", "q2", "q3"), similarity_threshold = 0.95
  )
  # 3 surveys identical: 3 pairs (H1-H2, H1-H3, H2-H3)
  expect_equal(res$summary_stat$n_pairs_flagged, 3)
  expect_equal(res$n_flagged, 3L)
})

# -- check_icc_enumerator -----------------------------------------------------
test_that("check_icc_enumerator flags high ICC variable", {
  # Enumerator E1 always gives low values, E2 always gives high values
  set.seed(42)
  df <- data.frame(
    hh_id   = paste0("H", 1:20),
    enum_id = rep(c("E1", "E2"), each = 10),
    score   = c(rnorm(10, mean = 10, sd = 0.5), rnorm(10, mean = 50, sd = 0.5)),
    stringsAsFactors = FALSE
  )

  res <- check_icc_enumerator(df, "hh_id", "enum_id", "score", max_icc = 0.15)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "M04_icc_enumerator")
  expect_equal(res$check_category, "fabrication")
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "score")
})

test_that("check_icc_enumerator returns 0 for low ICC", {
  set.seed(42)
  df <- data.frame(
    hh_id   = paste0("H", 1:30),
    enum_id = rep(c("E1", "E2", "E3"), each = 10),
    score   = rnorm(30, mean = 50, sd = 10),
    stringsAsFactors = FALSE
  )

  res <- check_icc_enumerator(df, "hh_id", "enum_id", "score", max_icc = 0.5)
  expect_equal(res$n_flagged, 0L)
})

# -- check_variance_ratio -----------------------------------------------------
test_that("check_variance_ratio flags high between/within ratio", {
  # Identical to ICC test: enumerators with very different means
  set.seed(42)
  df <- data.frame(
    hh_id   = paste0("H", 1:20),
    enum_id = rep(c("E1", "E2"), each = 10),
    score   = c(rnorm(10, mean = 10, sd = 1), rnorm(10, mean = 50, sd = 1)),
    stringsAsFactors = FALSE
  )

  res <- check_variance_ratio(df, "hh_id", "enum_id", "score", max_ratio = 2)

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "M08_variance_ratio")
  expect_equal(res$check_category, "fabrication")
  expect_equal(res$n_flagged, 1L)
  expect_equal(res$flagged_ids, "score")
})

test_that("check_variance_ratio returns 0 for low ratio", {
  set.seed(42)
  df <- data.frame(
    hh_id   = paste0("H", 1:30),
    enum_id = rep(c("E1", "E2", "E3"), each = 10),
    score   = rnorm(30, mean = 50, sd = 10),
    stringsAsFactors = FALSE
  )

  res <- check_variance_ratio(df, "hh_id", "enum_id", "score", max_ratio = 50)
  expect_equal(res$n_flagged, 0L)
})

test_that("check_variance_ratio summary_stat includes ratio_by_variable", {
  set.seed(42)
  df <- data.frame(
    hh_id   = paste0("H", 1:20),
    enum_id = rep(c("E1", "E2"), each = 10),
    score   = rnorm(20, 50, 5),
    income  = rnorm(20, 1000, 100),
    stringsAsFactors = FALSE
  )

  res <- check_variance_ratio(df, "hh_id", "enum_id",
                               c("score", "income"), max_ratio = 50)
  expect_true("ratio_by_variable" %in% names(res$summary_stat))
  expect_equal(nrow(res$summary_stat$ratio_by_variable), 2L)
})
