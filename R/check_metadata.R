#' Check consent status
#'
#' A.06: Flag records where consent was not given.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param consent_col Character. Name of the consent column
#' @param consent_value Value indicating consent was given (default 1)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_consent <- function(data, id_col, consent_col, consent_value = 1, ...) {
  assert_columns(data, c(id_col, consent_col), context = "check_consent")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  consent_vals <- data[[consent_col]]
  no_consent <- which((!is.na(consent_vals) & consent_vals != consent_value) |
                        is.na(consent_vals))

  flagged_ids <- ids[no_consent]

  flag_reason <- vapply(no_consent, function(i) {
    val <- consent_vals[i]
    if (is.na(val)) {
      "Consent value is missing"
    } else {
      paste0("Consent not given (got '", val, "', expected '", consent_value, "')")
    }
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "A06_consent",
    check_category = "metadata",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      n_consented    = sum(!is.na(consent_vals) & consent_vals == consent_value),
      n_no_consent   = length(no_consent),
      consent_value  = consent_value
    )
  )
}

#' Check form version
#'
#' A.07: Flag records using an outdated form version.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param version_col Character. Name of the form version column
#' @param expected_version Character or numeric. The expected (current) form version
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_form_version <- function(data, id_col, version_col, expected_version, ...) {
  assert_columns(data, c(id_col, version_col), context = "check_form_version")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  versions <- data[[version_col]]
  # Compare as character to handle both numeric and string versions
  wrong_version <- which(!is.na(versions) &
                           as.character(versions) != as.character(expected_version))

  flagged_ids <- ids[wrong_version]
  flagged_versions <- versions[wrong_version]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0("Form version '", flagged_versions[i],
           "' (expected '", expected_version, "')")
  }, character(1), USE.NAMES = FALSE)

  version_table <- table(as.character(versions[!is.na(versions)]))

  new_check_result(
    check_name     = "A07_form_version",
    check_category = "metadata",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      expected_version  = as.character(expected_version),
      version_counts    = as.list(version_table)
    )
  )
}

#' Check interview completion status
#'
#' A.09: Flag records where the interview was not completed.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param status_col Character. Name of the interview status column
#' @param complete_value Character. Value indicating a completed interview (default "complete")
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_interview_completed <- function(data, id_col, status_col,
                                       complete_value = "complete", ...) {
  assert_columns(data, c(id_col, status_col), context = "check_interview_completed")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  statuses <- data[[status_col]]
  not_complete <- which((!is.na(statuses) &
                          as.character(statuses) != as.character(complete_value)) |
                          is.na(statuses))

  flagged_ids <- ids[not_complete]

  flag_reason <- vapply(not_complete, function(i) {
    val <- statuses[i]
    if (is.na(val)) {
      "Interview status is missing"
    } else {
      paste0("Interview not completed (status: '", val, "')")
    }
  }, character(1), USE.NAMES = FALSE)

  status_table <- table(as.character(statuses[!is.na(statuses)]))

  new_check_result(
    check_name     = "A09_interview_completed",
    check_category = "metadata",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      complete_value = as.character(complete_value),
      status_counts  = as.list(status_table)
    )
  )
}

#' Check survey tracking against targets per stratum
#'
#' A.10: Flag strata where actual survey count is below the target.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param strata_col Character. Name of the stratification column
#' @param target_per_stratum Named numeric vector (stratum -> target) or
#'   a data frame with columns matching strata_col and "target"
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_survey_tracking <- function(data, id_col, strata_col,
                                   target_per_stratum, ...) {
  assert_columns(data, c(id_col, strata_col), context = "check_survey_tracking")

  ids <- as_id(data[[id_col]])
  strata <- as.character(data[[strata_col]])

  # Convert target_per_stratum to a named numeric vector

  if (is.data.frame(target_per_stratum)) {
    assert_columns(target_per_stratum, c(strata_col, "target"),
                   context = "check_survey_tracking (target_per_stratum)")
    targets <- stats::setNames(
      as.numeric(target_per_stratum[["target"]]),
      as.character(target_per_stratum[[strata_col]])
    )
  } else {
    targets <- target_per_stratum
  }

  # Actual counts per stratum
  actual_counts <- table(strata[!is.na(strata)])
  all_strata <- union(names(targets), names(actual_counts))

  tracking <- dplyr::tibble(
    stratum      = all_strata,
    target       = as.numeric(targets[all_strata]),
    actual       = as.numeric(actual_counts[all_strata]),
    pct_complete = ifelse(target > 0, round(actual / target * 100, 1), NA_real_)
  )
  # Replace NAs from missing counts with 0

tracking$actual[is.na(tracking$actual)] <- 0
  tracking$pct_complete <- ifelse(
    !is.na(tracking$target) & tracking$target > 0,
    round(tracking$actual / tracking$target * 100, 1),
    NA_real_
  )

  # Flag strata below target
  below_target <- tracking |>
    dplyr::filter(!is.na(target), actual < target)

  flagged_ids_out <- below_target$stratum

  flag_reason <- vapply(seq_len(nrow(below_target)), function(i) {
    paste0(
      "Stratum '", below_target$stratum[i], "': ",
      below_target$actual[i], "/", below_target$target[i],
      " (", below_target$pct_complete[i], "% complete)"
    )
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "A10_survey_tracking",
    check_category = "metadata",
    n_flagged      = length(flagged_ids_out),
    n_total        = as.integer(length(all_strata)),
    flagged_ids    = as.character(flagged_ids_out),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(tracking = tracking)
  )
}

#' Check ID format against expected pattern
#'
#' A.04: Flag IDs that do not match the expected regex pattern.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param pattern Character. Regular expression pattern that valid IDs must match
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_id_format <- function(data, id_col, pattern, ...) {
  assert_columns(data, id_col, context = "check_id_format")

  ids <- as_id(data[[id_col]])
  n_total <- length(ids)

  # Check non-NA IDs against pattern
  valid_mask <- !is.na(ids)
  matches_pattern <- grepl(pattern, ids)
  bad_format <- valid_mask & !matches_pattern

  flagged_ids <- ids[bad_format]

  flag_reason <- vapply(flagged_ids, function(id) {
    paste0("ID '", id, "' does not match pattern '", pattern, "'")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "A04_id_format",
    check_category = "identification",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      pattern        = pattern,
      n_valid_format = sum(valid_mask & matches_pattern),
      n_bad_format   = sum(bad_format),
      n_na           = sum(is.na(ids))
    )
  )
}

#' Check respondent eligibility
#'
#' A.12: Flag records where the respondent does not meet eligibility criteria
#' based on age and/or gender.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param age_col Character or NULL. Name of the respondent age column (default NULL)
#' @param min_age Numeric. Minimum eligible age (default 18)
#' @param gender_col Character or NULL. Name of the gender column (default NULL)
#' @param eligible_gender Character vector or NULL. Acceptable gender values (default NULL)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_respondent_eligibility <- function(data, id_col, age_col = NULL,
                                          min_age = 18, gender_col = NULL,
                                          eligible_gender = NULL, ...) {
  required <- id_col
  if (!is.null(age_col)) required <- c(required, age_col)
  if (!is.null(gender_col)) required <- c(required, gender_col)
  assert_columns(data, required, context = "check_respondent_eligibility")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  # Track violations per row
  age_violation <- rep(FALSE, n_total)
  gender_violation <- rep(FALSE, n_total)

  # Age eligibility
  if (!is.null(age_col)) {
    assert_numeric(data, age_col, context = "check_respondent_eligibility")
    ages <- data[[age_col]]
    age_violation <- !is.na(ages) & ages < min_age
  }

  # Gender eligibility
  if (!is.null(gender_col) && !is.null(eligible_gender)) {
    genders <- as.character(data[[gender_col]])
    gender_violation <- !is.na(genders) & !(genders %in% as.character(eligible_gender))
  }

  flagged_mask <- age_violation | gender_violation

  flagged_ids <- ids[flagged_mask]

  flag_reason <- vapply(which(flagged_mask), function(i) {
    reasons <- character()
    if (age_violation[i]) {
      reasons <- c(reasons, paste0("Age ", data[[age_col]][i],
                                   " below minimum ", min_age))
    }
    if (gender_violation[i]) {
      reasons <- c(reasons, paste0("Gender '", data[[gender_col]][i],
                                   "' not in eligible set"))
    }
    paste(reasons, collapse = "; ")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "A12_respondent_eligibility",
    check_category = "metadata",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      n_age_violation    = sum(age_violation),
      n_gender_violation = sum(gender_violation),
      min_age            = min_age,
      eligible_gender    = eligible_gender
    )
  )
}
