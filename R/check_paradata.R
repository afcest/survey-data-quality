# ============================================================================
# check_paradata.R — Paradata and text audit checks
# ============================================================================

#' Check text audit question-level duration
#'
#' N.01: Flag question-survey combinations where the time spent on a question
#' is less than \code{min_seconds}. Text audit data is expected in long format:
#' one row per question per survey.
#'
#' @param data Data frame of text audit entries (long format)
#' @param id_col Character. Name of the survey ID column
#' @param question_col Character. Name of the question identifier column
#' @param duration_col Character. Name of the duration column (in seconds)
#' @param min_seconds Numeric. Minimum acceptable seconds per question (default 3)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_text_audit_duration <- function(data, id_col, question_col, duration_col,
                                      min_seconds = 3, ...) {
  assert_columns(data, c(id_col, question_col, duration_col),
                 context = "check_text_audit_duration")
  assert_numeric(data, duration_col, context = "check_text_audit_duration")

  ids <- as_id(data[[id_col]])
  questions <- as.character(data[[question_col]])
  durations <- data[[duration_col]]
  n_total <- length(unique(ids))


  # Identify violations: duration < min_seconds (exclude NAs)
  violation_mask <- !is.na(durations) & durations < min_seconds
  violation_ids <- ids[violation_mask]
  violation_questions <- questions[violation_mask]
  violation_durations <- durations[violation_mask]

  # Build per-violation reason
  violation_reasons <- vapply(seq_along(violation_ids), function(i) {
    paste0("Question '", violation_questions[i], "' answered in ",
           round(violation_durations[i], 1), " seconds (min: ", min_seconds, ")")
  }, character(1), USE.NAMES = FALSE)

  # Flagged IDs: unique survey IDs with at least one violation

  flagged_ids <- unique(violation_ids)

  # Build per-ID reason: aggregate all violations for that survey
  flag_reason <- vapply(flagged_ids, function(fid) {
    idx <- which(violation_ids == fid)
    n_violations <- length(idx)
    paste0(n_violations, " speed violation(s) detected")
  }, character(1), USE.NAMES = FALSE)

  # Summary: violations by question
  n_violations <- sum(violation_mask)
  n_total_rows <- sum(!is.na(durations))
  violation_rate <- if (n_total_rows > 0) n_violations / n_total_rows else 0

  violations_by_question <- if (n_violations > 0) {
    viol_df <- data.frame(
      question = violation_questions,
      stringsAsFactors = FALSE
    )
    counts <- as.data.frame(table(viol_df$question), stringsAsFactors = FALSE)
    names(counts) <- c("question", "n_violations")
    dplyr::as_tibble(counts[order(-counts$n_violations), ])
  } else {
    dplyr::tibble(question = character(), n_violations = integer())
  }

  new_check_result(
    check_name     = "N01_text_audit_duration",
    check_category = "paradata",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      n_violations          = n_violations,
      violation_rate        = round(violation_rate, 4),
      violations_by_question = violations_by_question
    )
  )
}

#' Check text audit question sequence for backward jumps
#'
#' N.02: Detect question revisiting by checking if the question order has
#' backward jumps within each survey. A backward jump means the order value
#' decreases, indicating the enumerator revisited an earlier question.
#'
#' @param data Data frame of text audit entries (long format)
#' @param id_col Character. Name of the survey ID column
#' @param question_col Character. Name of the question identifier column
#' @param order_col Character. Name of the order/sequence column (numeric)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_text_audit_sequence <- function(data, id_col, question_col, order_col,
                                      ...) {
  assert_columns(data, c(id_col, question_col, order_col),
                 context = "check_text_audit_sequence")
  assert_numeric(data, order_col, context = "check_text_audit_sequence")

  ids <- as_id(data[[id_col]])
  orders <- data[[order_col]]
  unique_ids <- unique(ids)
  n_total <- length(unique_ids)

  # Count backward jumps per survey
  jumps_per_survey <- vapply(unique_ids, function(sid) {
    idx <- which(ids == sid)
    ord <- orders[idx]
    # Remove NAs
    ord <- ord[!is.na(ord)]
    if (length(ord) <= 1) return(0L)
    # Count backward jumps: where diff < 0
    sum(diff(ord) < 0)
  }, integer(1))

  # Flag surveys with > 0 backward jumps
  flagged_mask <- jumps_per_survey > 0L
  flagged_ids <- unique_ids[flagged_mask]
  flagged_jumps <- jumps_per_survey[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0(flagged_jumps[i], " backward jump(s) detected")
  }, character(1), USE.NAMES = FALSE)

  n_surveys_with_jumps <- sum(flagged_mask)
  mean_jumps <- if (n_surveys_with_jumps > 0) {
    mean(flagged_jumps)
  } else {
    0
  }

  new_check_result(
    check_name     = "N02_text_audit_sequence",
    check_category = "paradata",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "info",
    summary_stat   = list(
      n_surveys_with_jumps = n_surveys_with_jumps,
      mean_jumps_per_survey = round(mean_jumps, 2)
    )
  )
}

#' Check speed violations based on total survey duration
#'
#' N.03: Flag surveys where total duration is below the expected minimum,
#' computed as \code{total_questions * min_seconds_per_question}. This check
#' works on the main survey data (one row per survey), not text audit data.
#'
#' @param data Data frame of survey submissions (one row per survey)
#' @param id_col Character. Name of the primary key column
#' @param total_questions Integer. Total number of questions in the survey
#' @param total_duration_col Character. Name of the total duration column (seconds)
#' @param min_seconds_per_question Numeric. Minimum expected seconds per question
#'   (default 5)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_speed_violations <- function(data, id_col, total_questions,
                                    total_duration_col,
                                    min_seconds_per_question = 5, ...) {
  assert_columns(data, c(id_col, total_duration_col),
                 context = "check_speed_violations")
  assert_numeric(data, total_duration_col, context = "check_speed_violations")

  ids <- as_id(data[[id_col]])
  durations <- data[[total_duration_col]]
  n_total <- nrow(data)

  expected_min <- total_questions * min_seconds_per_question

  # Flag surveys where total duration < expected minimum
  flagged_mask <- !is.na(durations) & durations < expected_min
  flagged_ids <- ids[flagged_mask]
  flagged_durations <- durations[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0("Total duration ", round(flagged_durations[i], 1),
           "s < expected minimum ", expected_min,
           "s (", total_questions, " questions x ",
           min_seconds_per_question, "s)")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "N03_speed_violations",
    check_category = "paradata",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      expected_min_seconds    = expected_min,
      total_questions         = total_questions,
      min_seconds_per_question = min_seconds_per_question
    )
  )
}

#' Check field comments for supervisor review
#'
#' N.04: Extract and list all non-empty field comments. This is informational:
#' comments need human review and are flagged for supervisor attention.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param comment_col Character. Name of the comment column
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_field_comments <- function(data, id_col, comment_col, ...) {
  assert_columns(data, c(id_col, comment_col),
                 context = "check_field_comments")

  ids <- as_id(data[[id_col]])
  comments <- as.character(data[[comment_col]])
  n_total <- nrow(data)

  # Flag records with non-empty, non-NA comments
  has_comment <- !is.na(comments) & nchar(trimws(comments)) > 0
  flagged_ids <- ids[has_comment]
  flagged_comments <- comments[has_comment]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    # Truncate long comments for the reason string
    comment_preview <- if (nchar(flagged_comments[i]) > 80) {
      paste0(substr(flagged_comments[i], 1, 77), "...")
    } else {
      flagged_comments[i]
    }
    paste0("Comment: \"", comment_preview, "\"")
  }, character(1), USE.NAMES = FALSE)

  comment_values <- dplyr::tibble(
    id      = as.character(flagged_ids),
    comment = flagged_comments
  )

  new_check_result(
    check_name     = "N04_field_comments",
    check_category = "paradata",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "info",
    summary_stat   = list(
      n_with_comments = length(flagged_ids),
      comment_values  = comment_values
    )
  )
}

#' Check photo attachment completeness
#'
#' N.06: Flag records where the photo column is missing, empty, or contains
#' a placeholder value such as "none".
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param photo_col Character. Name of the photo attachment column
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_photo_attachment <- function(data, id_col, photo_col, ...) {
  assert_columns(data, c(id_col, photo_col),
                 context = "check_photo_attachment")

  ids <- as_id(data[[id_col]])
  photos <- as.character(data[[photo_col]])
  n_total <- nrow(data)

  # Flag records where photo is NA, empty, or "none" (case-insensitive)
  missing_photo <- is.na(photos) |
    nchar(trimws(photos)) == 0 |
    tolower(trimws(photos)) == "none"

  flagged_ids <- ids[missing_photo]

  flag_reason <- rep("Missing or empty photo attachment", length(flagged_ids))

  new_check_result(
    check_name     = "N06_photo_attachment",
    check_category = "paradata",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      n_missing = length(flagged_ids),
      pct_missing = if (n_total > 0) round(length(flagged_ids) / n_total * 100, 2) else 0
    )
  )
}

#' Check device consistency per enumerator
#'
#' N.08: Flag enumerators who used more than one device during data collection.
#' Groups by enumerator and counts distinct device identifiers. Enumerators
#' with more than one device are flagged.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param enum_col Character. Name of the enumerator ID column
#' @param device_col Character. Name of the device identifier column
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_device_consistency <- function(data, id_col, enum_col, device_col, ...) {
  assert_columns(data, c(id_col, enum_col, device_col),
                 context = "check_device_consistency")

  ids <- as_id(data[[id_col]])
  enums <- as_id(data[[enum_col]])
  devices <- as_id(data[[device_col]])
  n_total <- nrow(data)

  # Build enumerator-device mapping (exclude NAs)
  valid <- !is.na(enums) & !is.na(devices)
  enum_device_df <- data.frame(
    enum   = enums[valid],
    device = devices[valid],
    id     = ids[valid],
    stringsAsFactors = FALSE
  )

  # Count distinct devices per enumerator
  devices_by_enum <- tapply(enum_device_df$device, enum_device_df$enum,
                            function(x) unique(x))

  device_counts <- vapply(devices_by_enum, length, integer(1))

  # Flag enumerators with > 1 device
  flagged_enums <- names(device_counts[device_counts > 1])

  # Find all survey IDs associated with flagged enumerators
  flagged_ids <- unique(enum_device_df$id[enum_device_df$enum %in% flagged_enums])

  flag_reason <- vapply(flagged_ids, function(fid) {
    enum <- enum_device_df$enum[enum_device_df$id == fid][1]
    n_devices <- device_counts[enum]
    paste0("Enumerator '", enum, "' used ", n_devices, " different devices")
  }, character(1), USE.NAMES = FALSE)

  # Summary tibble: devices per enumerator
  devices_by_enumerator <- dplyr::tibble(
    enumerator = names(device_counts),
    n_devices  = as.integer(device_counts),
    devices    = vapply(devices_by_enum, function(x) paste(x, collapse = ", "),
                        character(1))
  )

  new_check_result(
    check_name     = "N08_device_consistency",
    check_category = "paradata",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      devices_by_enumerator = devices_by_enumerator
    )
  )
}
