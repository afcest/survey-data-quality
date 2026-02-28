#' Check survey duration
#'
#' F.01: Flag surveys shorter than min_duration or longer than max_duration.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param duration_col Character. Name of the column containing duration in minutes
#' @param min_duration Numeric. Minimum acceptable duration in minutes (default 10)
#' @param max_duration Numeric. Maximum acceptable duration in minutes (default 120)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_survey_duration <- function(data, id_col, duration_col,
                                   min_duration = 10, max_duration = 120, ...) {
  assert_columns(data, c(id_col, duration_col), context = "check_survey_duration")
  assert_numeric(data, duration_col, context = "check_survey_duration")

  ids <- as_id(data[[id_col]])
  durations <- data[[duration_col]]
  n_total <- nrow(data)

  # Identify non-NA rows for flagging
  valid <- !is.na(durations)
  too_short <- valid & durations < min_duration
  too_long <- valid & durations > max_duration
  flagged_mask <- too_short | too_long

  flagged_ids <- ids[flagged_mask]
  flagged_durations <- durations[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0(
      "Duration ", round(flagged_durations[i], 1),
      " min (expected ", min_duration, "-", max_duration, " min)"
    )
  }, character(1), USE.NAMES = FALSE)

  sev <- "warning"

  valid_durations <- durations[valid]

  new_check_result(
    check_name     = "F01_survey_duration",
    check_category = "timing",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = sev,
    summary_stat   = list(
      median_duration = stats::median(valid_durations, na.rm = TRUE),
      mean_duration   = mean(valid_durations, na.rm = TRUE),
      sd_duration     = stats::sd(valid_durations, na.rm = TRUE)
    )
  )
}

#' Check collection window
#'
#' F.02: Flag surveys collected outside the expected date window.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param date_col Character. Name of the column containing collection dates
#' @param start_date Character or Date. Start of the valid collection window
#' @param end_date Character or Date. End of the valid collection window
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_collection_window <- function(data, id_col, date_col,
                                     start_date, end_date, ...) {
  assert_columns(data, c(id_col, date_col), context = "check_collection_window")

  ids <- as_id(data[[id_col]])
  dates <- as.Date(data[[date_col]])
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  n_total <- nrow(data)

  valid <- !is.na(dates)
  before <- valid & dates < start_date
  after <- valid & dates > end_date
  flagged_mask <- before | after

  flagged_ids <- ids[flagged_mask]
  flagged_dates <- dates[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0(
      "Collected on ", as.character(flagged_dates[i]),
      ", outside window [", as.character(start_date),
      ", ", as.character(end_date), "]"
    )
  }, character(1), USE.NAMES = FALSE)

  valid_dates <- dates[valid]
  date_range <- if (length(valid_dates) > 0) {
    as.character(range(valid_dates))
  } else {
    c(NA_character_, NA_character_)
  }

  new_check_result(
    check_name     = "F02_collection_window",
    check_category = "timing",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      n_before   = as.integer(sum(before)),
      n_after    = as.integer(sum(after)),
      date_range = date_range
    )
  )
}

#' Check for future dates
#'
#' F.03: Flag records with dates in the future relative to a reference date.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param date_col Character. Name of the column containing dates
#' @param reference_date Date. Reference date to compare against (default Sys.Date())
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_future_dates <- function(data, id_col, date_col,
                                reference_date = Sys.Date(), ...) {
  assert_columns(data, c(id_col, date_col), context = "check_future_dates")

  ids <- as_id(data[[id_col]])
  dates <- as.Date(data[[date_col]])
  reference_date <- as.Date(reference_date)
  n_total <- nrow(data)

  valid <- !is.na(dates)
  future_mask <- valid & dates > reference_date

  flagged_ids <- ids[future_mask]
  flagged_dates <- dates[future_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0("Date ", as.character(flagged_dates[i]), " is in the future")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "F03_future_date",
    check_category = "timing",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "error"
  )
}
