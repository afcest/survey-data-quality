#' Check household composition consistency
#'
#' G.01: Flag records with implausible household size or mismatch between
#' reported size and roster count.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param hh_size_col Character. Name of the household size column
#' @param roster_count_col Character or NULL. Name of the roster count column (default NULL)
#' @param min_size Numeric. Minimum plausible household size (default 1)
#' @param max_size Numeric. Maximum plausible household size (default 30)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_hh_composition <- function(data, id_col, hh_size_col,
                                  roster_count_col = NULL,
                                  min_size = 1, max_size = 30, ...) {
  required <- c(id_col, hh_size_col)
  if (!is.null(roster_count_col)) required <- c(required, roster_count_col)
  assert_columns(data, required, context = "check_hh_composition")
  assert_numeric(data, hh_size_col, context = "check_hh_composition")

  ids <- as_id(data[[id_col]])
  hh_sizes <- data[[hh_size_col]]
  n_total <- nrow(data)

  # Implausible size: outside min_size to max_size range
  implausible <- !is.na(hh_sizes) & (hh_sizes < min_size | hh_sizes > max_size)

  # Roster mismatch
  roster_mismatch <- rep(FALSE, n_total)
  if (!is.null(roster_count_col)) {
    assert_numeric(data, roster_count_col, context = "check_hh_composition")
    roster_counts <- data[[roster_count_col]]
    roster_mismatch <- !is.na(hh_sizes) & !is.na(roster_counts) &
      hh_sizes != roster_counts
  }

  flagged_mask <- implausible | roster_mismatch
  flagged_ids <- ids[flagged_mask]

  flag_reason <- vapply(which(flagged_mask), function(i) {
    reasons <- character()
    if (implausible[i]) {
      reasons <- c(reasons, paste0("HH size ", hh_sizes[i],
                                   " is implausible (expected ",
                                   min_size, "-", max_size, ")"))
    }
    if (roster_mismatch[i]) {
      reasons <- c(reasons, paste0("HH size ", hh_sizes[i],
                                   " != roster count ",
                                   data[[roster_count_col]][i]))
    }
    paste(reasons, collapse = "; ")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "G01_hh_composition",
    check_category = "logic",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      n_implausible    = sum(implausible),
      n_roster_mismatch = sum(roster_mismatch),
      median_hh_size   = stats::median(hh_sizes, na.rm = TRUE)
    )
  )
}

#' Check income-expenditure ratio
#'
#' G.04: Flag records where expenditure exceeds a plausible ratio of income.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param income_col Character. Name of the income column
#' @param expenditure_col Character. Name of the expenditure column
#' @param max_ratio Numeric. Maximum acceptable expenditure/income ratio (default 5)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_income_expenditure <- function(data, id_col, income_col, expenditure_col,
                                      max_ratio = 5, ...) {
  assert_columns(data, c(id_col, income_col, expenditure_col),
                 context = "check_income_expenditure")
  assert_numeric(data, income_col, context = "check_income_expenditure")
  assert_numeric(data, expenditure_col, context = "check_income_expenditure")

  ids <- as_id(data[[id_col]])
  incomes <- data[[income_col]]
  expenditures <- data[[expenditure_col]]
  n_total <- nrow(data)

  # Only flag where both values are non-NA and income > 0
  valid <- !is.na(incomes) & !is.na(expenditures) & incomes > 0
  implausible <- valid & (expenditures > max_ratio * incomes)

  flagged_ids <- ids[implausible]

  flag_reason <- vapply(which(implausible), function(i) {
    ratio <- round(expenditures[i] / incomes[i], 2)
    paste0("Expenditure/income ratio = ", ratio,
           " (", expenditures[i], "/", incomes[i],
           ", max allowed: ", max_ratio, ")")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "G04_income_expenditure",
    check_category = "logic",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      max_ratio        = max_ratio,
      median_income    = stats::median(incomes, na.rm = TRUE),
      median_expenditure = stats::median(expenditures, na.rm = TRUE),
      n_zero_income    = sum(!is.na(incomes) & incomes == 0)
    )
  )
}

#' Check age-date of birth consistency
#'
#' F.09: Flag records where reported age does not match the age computed
#' from date of birth.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param age_col Character. Name of the reported age column
#' @param dob_col Character. Name of the date of birth column
#' @param reference_date Date. Reference date for computing age (default Sys.Date())
#' @param tolerance_years Numeric. Acceptable discrepancy in years (default 1)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_age_date_consistency <- function(data, id_col, age_col, dob_col,
                                        reference_date = Sys.Date(),
                                        tolerance_years = 1, ...) {
  assert_columns(data, c(id_col, age_col, dob_col),
                 context = "check_age_date_consistency")
  assert_numeric(data, age_col, context = "check_age_date_consistency")

  ids <- as_id(data[[id_col]])
  reported_ages <- data[[age_col]]
  dob <- as.Date(data[[dob_col]])
  reference_date <- as.Date(reference_date)
  n_total <- nrow(data)

  # Compute expected age in years
  computed_ages <- as.numeric(difftime(reference_date, dob, units = "days")) / 365.25

  # Flag where discrepancy exceeds tolerance
  valid <- !is.na(reported_ages) & !is.na(computed_ages)
  discrepancy <- abs(reported_ages - computed_ages)
  inconsistent <- valid & discrepancy > tolerance_years

  flagged_ids <- ids[inconsistent]

  flag_reason <- vapply(which(inconsistent), function(i) {
    paste0("Reported age ", reported_ages[i],
           " vs computed age ", round(computed_ages[i], 1),
           " (difference: ", round(discrepancy[i], 1),
           " years, tolerance: ", tolerance_years, ")")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "F09_age_date_consistency",
    check_category = "logic",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      tolerance_years    = tolerance_years,
      mean_discrepancy   = mean(discrepancy[valid], na.rm = TRUE),
      median_discrepancy = stats::median(discrepancy[valid], na.rm = TRUE),
      n_missing_dob      = sum(is.na(dob))
    )
  )
}

#' Check survey end time before start time
#'
#' F.04: Flag surveys where the end timestamp is before the start timestamp.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param start_col Character. Name of the survey start time column
#' @param end_col Character. Name of the survey end time column
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_survey_end_before_start <- function(data, id_col, start_col, end_col, ...) {
  assert_columns(data, c(id_col, start_col, end_col),
                 context = "check_survey_end_before_start")

  ids <- as_id(data[[id_col]])
  start_times <- as.POSIXct(data[[start_col]], tz = "UTC")
  end_times <- as.POSIXct(data[[end_col]], tz = "UTC")
  n_total <- nrow(data)

  valid <- !is.na(start_times) & !is.na(end_times)
  end_before_start <- valid & end_times < start_times

  flagged_ids <- ids[end_before_start]

  flag_reason <- vapply(which(end_before_start), function(i) {
    paste0("End time (", as.character(end_times[i]),
           ") is before start time (", as.character(start_times[i]), ")")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "F04_end_before_start",
    check_category = "timing",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      n_valid_times  = sum(valid),
      n_missing_start = sum(is.na(start_times)),
      n_missing_end   = sum(is.na(end_times))
    )
  )
}

#' Check custom logic condition
#'
#' G.12: Evaluate a user-defined expression and flag records where the
#' condition evaluates to TRUE (indicating a violation).
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param condition_expr Character. An R expression string that evaluates to a
#'   logical vector using column names from data. TRUE means violation.
#' @param description Character. Human-readable description of the check
#'   (default "Custom logic check")
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_custom_logic <- function(data, id_col, condition_expr,
                                description = "Custom logic check", ...) {
  assert_columns(data, id_col, context = "check_custom_logic")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  # Parse and evaluate the expression in the data environment
  parsed_expr <- rlang::parse_expr(condition_expr)
  result <- rlang::eval_tidy(parsed_expr, data = data)

  if (!is.logical(result) || length(result) != n_total) {
    cli::cli_abort(c(
      "condition_expr must evaluate to a logical vector of length {n_total}",
      "x" = "Got {class(result)[1]} of length {length(result)}"
    ))
  }

  # Replace NAs with FALSE (NA means we can't determine violation)
  result[is.na(result)] <- FALSE

  flagged_ids <- ids[result]

  new_check_result(
    check_name     = "G12_custom_logic",
    check_category = "logic",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = rep(description, length(flagged_ids)),
    severity       = "warning",
    summary_stat   = list(
      condition_expr = condition_expr,
      description    = description
    )
  )
}

#' Check roster completeness
#'
#' C.07: Flag records where roster member count does not match the
#' reported household size.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param hh_size_col Character. Name of the reported household size column
#' @param member_count_col Character. Name of the actual roster member count column
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_roster_completeness <- function(data, id_col, hh_size_col,
                                       member_count_col, ...) {
  assert_columns(data, c(id_col, hh_size_col, member_count_col),
                 context = "check_roster_completeness")
  assert_numeric(data, hh_size_col, context = "check_roster_completeness")
  assert_numeric(data, member_count_col, context = "check_roster_completeness")

  ids <- as_id(data[[id_col]])
  hh_sizes <- data[[hh_size_col]]
  member_counts <- data[[member_count_col]]
  n_total <- nrow(data)

  valid <- !is.na(hh_sizes) & !is.na(member_counts)
  mismatch <- valid & hh_sizes != member_counts

  flagged_ids <- ids[mismatch]

  flag_reason <- vapply(which(mismatch), function(i) {
    paste0("Roster count ", member_counts[i],
           " != reported HH size ", hh_sizes[i])
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "C07_roster_completeness",
    check_category = "completeness",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      n_mismatch        = sum(mismatch),
      n_missing_size    = sum(is.na(hh_sizes)),
      n_missing_roster  = sum(is.na(member_counts)),
      mean_difference   = mean(abs(hh_sizes[valid] - member_counts[valid]))
    )
  )
}

#' Check survey duration by household size
#'
#' F.06: Flag surveys that are shorter than expected given the household size.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param duration_col Character. Name of the duration column (in minutes)
#' @param hh_size_col Character. Name of the household size column
#' @param min_minutes_per_member Numeric. Minimum expected minutes per HH member
#'   (default 3)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_duration_by_hh_size <- function(data, id_col, duration_col, hh_size_col,
                                       min_minutes_per_member = 3, ...) {
  assert_columns(data, c(id_col, duration_col, hh_size_col),
                 context = "check_duration_by_hh_size")
  assert_numeric(data, duration_col, context = "check_duration_by_hh_size")
  assert_numeric(data, hh_size_col, context = "check_duration_by_hh_size")

  ids <- as_id(data[[id_col]])
  durations <- data[[duration_col]]
  hh_sizes <- data[[hh_size_col]]
  n_total <- nrow(data)

  expected_min <- hh_sizes * min_minutes_per_member
  valid <- !is.na(durations) & !is.na(hh_sizes) & hh_sizes > 0
  too_short <- valid & durations < expected_min

  flagged_ids <- ids[too_short]

  flag_reason <- vapply(which(too_short), function(i) {
    paste0("Duration ", round(durations[i], 1),
           " min for HH size ", hh_sizes[i],
           " (expected >= ", round(expected_min[i], 1), " min at ",
           min_minutes_per_member, " min/member)")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "F06_duration_by_hh_size",
    check_category = "timing",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      min_minutes_per_member = min_minutes_per_member,
      median_duration        = stats::median(durations, na.rm = TRUE),
      median_hh_size         = stats::median(hh_sizes, na.rm = TRUE),
      median_min_per_member  = stats::median(
        durations[valid] / hh_sizes[valid], na.rm = TRUE
      )
    )
  )
}
