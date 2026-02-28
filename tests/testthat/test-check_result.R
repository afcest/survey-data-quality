# tests/testthat/test-check_result.R
# Tests for new_check_result(), print.check_result(), is_check_result(),
# bind_check_results()

test_that("new_check_result creates a valid check_result object", {
  res <- new_check_result(
    check_name     = "test_check",
    check_category = "identification",
    n_flagged      = 3L,
    n_total        = 100L,
    flagged_ids    = c("A", "B", "C"),
    flag_reason    = c("reason A", "reason B", "reason C"),
    severity       = "error"
  )

  expect_s3_class(res, "check_result")
  expect_equal(res$check_name, "test_check")
  expect_equal(res$check_category, "identification")
  expect_equal(res$n_flagged, 3L)
  expect_equal(res$n_total, 100L)
  expect_equal(res$flagged_ids, c("A", "B", "C"))
  expect_equal(res$flag_reason, c("reason A", "reason B", "reason C"))
  expect_equal(res$severity, "error")
  expect_true(inherits(res$timestamp, "POSIXct"))
})

test_that("new_check_result defaults to severity 'error'", {
  res <- new_check_result(
    check_name     = "test_default_severity",
    check_category = "completeness"
  )
  expect_equal(res$severity, "error")
})

test_that("severity validation rejects invalid values", {
  expect_error(
    new_check_result(
      check_name     = "bad_severity",
      check_category = "identification",
      severity       = "critical"
    ),
    "arg"
  )
})

test_that("severity accepts all valid values", {
  for (sev in c("error", "warning", "info")) {
    res <- new_check_result(
      check_name     = "sev_test",
      check_category = "identification",
      severity       = sev
    )
    expect_equal(res$severity, sev)
  }
})

test_that("mismatched flagged_ids and flag_reason lengths error", {
  expect_error(
    new_check_result(
      check_name     = "mismatch_test",
      check_category = "identification",
      flagged_ids    = c("A", "B", "C"),
      flag_reason    = c("reason A", "reason B")  # length 2 vs 3
    ),
    "flagged_ids.*flag_reason|length"
  )
})

test_that("empty flag_reason is allowed (length 0)", {
  res <- new_check_result(
    check_name     = "empty_reason",
    check_category = "identification",
    flagged_ids    = c("A", "B"),
    flag_reason    = character()
  )
  expect_equal(length(res$flag_reason), 0L)
})

test_that("n_flagged and n_total are coerced to integer", {
  res <- new_check_result(
    check_name     = "coerce_test",
    check_category = "identification",
    n_flagged      = 5,
    n_total        = 200
  )
  expect_type(res$n_flagged, "integer")
  expect_type(res$n_total, "integer")
})

test_that("summary_stat accepts arbitrary list content", {
  res <- new_check_result(
    check_name     = "stats_test",
    check_category = "identification",
    summary_stat   = list(mean_val = 42.5, details = list(a = 1, b = 2))
  )
  expect_equal(res$summary_stat$mean_val, 42.5)
  expect_equal(res$summary_stat$details$a, 1)
})

test_that("is_check_result returns TRUE for check_result objects", {
  res <- new_check_result(
    check_name     = "is_test",
    check_category = "identification"
  )
  expect_true(is_check_result(res))
})

test_that("is_check_result returns FALSE for non-check_result objects", {
  expect_false(is_check_result(list(check_name = "fake")))
  expect_false(is_check_result("string"))
  expect_false(is_check_result(42))
  expect_false(is_check_result(NULL))
})

test_that("print.check_result prints without error", {
  res <- new_check_result(
    check_name     = "print_test",
    check_category = "identification",
    n_flagged      = 2L,
    n_total        = 50L,
    severity       = "warning"
  )
  expect_output(print(res), "print_test")
  expect_output(print(res), "2/50")
})

test_that("print.check_result returns invisibly", {
  res <- new_check_result(
    check_name     = "invis_test",
    check_category = "identification"
  )
  out <- withVisible(print(res))
  expect_false(out$visible)
})

test_that("bind_check_results combines multiple results into a tibble", {
  r1 <- new_check_result(
    check_name     = "check_A",
    check_category = "identification",
    n_flagged      = 2L,
    n_total        = 100L,
    severity       = "error"
  )
  r2 <- new_check_result(
    check_name     = "check_B",
    check_category = "completeness",
    n_flagged      = 5L,
    n_total        = 100L,
    severity       = "warning"
  )

  tbl <- bind_check_results(r1, r2)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 2L)
  expect_equal(tbl$check_name, c("check_A", "check_B"))
  expect_equal(tbl$n_flagged, c(2L, 5L))
  expect_equal(tbl$severity, c("error", "warning"))
  expect_true("pct_flagged" %in% names(tbl))
  expect_equal(tbl$pct_flagged, c(2.0, 5.0))
})

test_that("bind_check_results handles an empty call", {
  tbl <- bind_check_results()
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 0L)
})

test_that("bind_check_results handles a list of results", {
  results <- list(
    new_check_result("c1", "cat1", n_flagged = 1L, n_total = 10L),
    new_check_result("c2", "cat2", n_flagged = 0L, n_total = 10L)
  )
  tbl <- bind_check_results(results)
  expect_equal(nrow(tbl), 2L)
})

test_that("bind_check_results ignores non-check_result items", {
  r1 <- new_check_result("c1", "cat1", n_flagged = 1L, n_total = 10L)
  tbl <- bind_check_results(r1, "not_a_result", 42)
  expect_equal(nrow(tbl), 1L)
})
