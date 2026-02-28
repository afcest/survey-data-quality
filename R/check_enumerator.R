#' Check enumerator daily productivity
#'
#' B.01: Flag enumerator-days where survey count exceeds the maximum daily limit.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param enum_col Character. Name of the enumerator ID column
#' @param date_col Character. Name of the survey date column (parsed with as.Date())
#' @param max_daily Integer. Maximum expected surveys per enumerator per day (default 10)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_enumerator_productivity <- function(data, id_col, enum_col, date_col,
                                           max_daily = 10, ...) {
  assert_columns(data, c(id_col, enum_col, date_col),
                 context = "check_enumerator_productivity")

  ids <- as_id(data[[id_col]])
  enums <- as_id(data[[enum_col]])
  dates <- as.Date(data[[date_col]])
  n_total <- nrow(data)

  # Count surveys per enumerator per day
 daily_counts <- dplyr::tibble(enum = enums, date = dates) |>
    dplyr::filter(!is.na(enum), !is.na(date)) |>
    dplyr::group_by(enum, date) |>
    dplyr::summarise(n_surveys = dplyr::n(), .groups = "drop")

  team_daily_mean <- mean(daily_counts$n_surveys)

  # Flag enumerator-days exceeding max_daily
  over_limit <- daily_counts |> dplyr::filter(n_surveys > max_daily)
  flagged_enums <- unique(over_limit$enum)

  # Build one reason per flagged enumerator (worst day)
  flag_reasons <- vapply(flagged_enums, function(e) {
    rows <- over_limit |> dplyr::filter(enum == e)
    worst <- rows |> dplyr::arrange(dplyr::desc(n_surveys)) |> dplyr::slice(1L)
    paste0(worst$n_surveys, " surveys on ", worst$date, " (max: ", max_daily, ")")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "B01_enumerator_productivity",
    check_category = "enumerator",
    n_flagged      = length(flagged_enums),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_enums),
    flag_reason    = flag_reasons,
    severity       = "warning",
    summary_stat   = list(
      daily_counts    = daily_counts,
      team_daily_mean = team_daily_mean
    )
  )
}

#' Check enumerator survey duration
#'
#' B.02: Flag enumerators whose mean survey duration is suspiciously low
#' (more than threshold_sd standard deviations below the team mean).
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param enum_col Character. Name of the enumerator ID column
#' @param duration_col Character. Name of the survey duration column (numeric, in minutes)
#' @param threshold_sd Numeric. Number of SDs below team mean to trigger flag (default 2)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_enumerator_duration <- function(data, id_col, enum_col, duration_col,
                                       threshold_sd = 2, ...) {
  assert_columns(data, c(id_col, enum_col, duration_col),
                 context = "check_enumerator_duration")
  assert_numeric(data, duration_col, context = "check_enumerator_duration")

  ids <- as_id(data[[id_col]])
  enums <- as_id(data[[enum_col]])
  durations <- data[[duration_col]]

  # Compute per-enumerator mean duration
  enum_stats <- dplyr::tibble(enum = enums, duration = durations) |>
    dplyr::filter(!is.na(enum), !is.na(duration)) |>
    dplyr::group_by(enum) |>
    dplyr::summarise(
      n_surveys     = dplyr::n(),
      mean_duration = mean(duration),
      .groups       = "drop"
    )

  team_mean <- mean(enum_stats$mean_duration)
  team_sd <- stats::sd(enum_stats$mean_duration)

  # Flag enumerators whose mean duration is too far below team mean
  if (is.na(team_sd) || team_sd == 0) {
    flagged_ids <- character()
    flag_reason <- character()
    n_flag <- 0L
  } else {
    enum_stats$z_score <- (enum_stats$mean_duration - team_mean) / team_sd
    flagged <- enum_stats |> dplyr::filter(z_score < -threshold_sd)
    n_flag <- nrow(flagged)
    if (n_flag > 0L) {
      flagged_ids <- as_id(flagged$enum)
      flag_reason <- paste0(
        "Mean duration ", round(flagged$mean_duration, 1),
        " min vs team avg ", round(team_mean, 1), " min"
      )
    } else {
      flagged_ids <- character()
      flag_reason <- character()
    }
  }

  new_check_result(
    check_name     = "B02_enumerator_duration",
    check_category = "enumerator",
    n_flagged      = n_flag,
    n_total        = nrow(enum_stats),
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      enum_stats = enum_stats,
      team_mean  = team_mean,
      team_sd    = team_sd
    )
  )
}

#' Check don't-know response rate by enumerator
#'
#' B.03: Flag enumerators with an unusually high rate of "don't know"
#' responses (more than threshold_sd SDs above team mean).
#'
#' A cell is counted as DK if it matches dk_value (case-insensitive) or
#' equals -99, -88, or -77.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param enum_col Character. Name of the enumerator ID column
#' @param dk_value Character. The "don't know" string value (default "dk")
#' @param check_cols Character vector. Columns to inspect; if NULL, all columns
#'   except id_col and enum_col are used
#' @param threshold_sd Numeric. Number of SDs above team mean to trigger flag (default 2)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_dk_rate <- function(data, id_col, enum_col, dk_value = "dk",
                           check_cols = NULL, threshold_sd = 2, ...) {
  assert_columns(data, c(id_col, enum_col), context = "check_dk_rate")

  if (is.null(check_cols)) {
    check_cols <- setdiff(names(data), c(id_col, enum_col))
  } else {
    assert_columns(data, check_cols, context = "check_dk_rate")
  }

  enums <- as_id(data[[enum_col]])
  n_cols <- length(check_cols)

  # Vectorized DK detection
  dk_matrix <- matrix(FALSE, nrow = nrow(data), ncol = length(check_cols))
  for (j in seq_along(check_cols)) {
    col_vals <- data[[check_cols[j]]]
    if (is.numeric(col_vals)) {
      dk_matrix[, j] <- !is.na(col_vals) & col_vals %in% c(-99, -88, -77)
    } else {
      char_vals <- tolower(as.character(col_vals))
      dk_matrix[, j] <- !is.na(col_vals) &
        (char_vals %in% tolower(dk_value) | col_vals %in% c("-99", "-88", "-77"))
    }
  }
  dk_counts <- rowSums(dk_matrix)

  # Per-enumerator DK rate
  enum_dk_rates <- dplyr::tibble(enum = enums, dk_count = dk_counts) |>
    dplyr::filter(!is.na(enum)) |>
    dplyr::group_by(enum) |>
    dplyr::summarise(
      n_surveys  = dplyr::n(),
      total_dk   = sum(dk_count),
      total_cells = dplyr::n() * n_cols,
      dk_rate    = total_dk / total_cells,
      .groups    = "drop"
    )

  team_mean <- mean(enum_dk_rates$dk_rate)
  team_sd <- stats::sd(enum_dk_rates$dk_rate)

  if (is.na(team_sd) || team_sd == 0) {
    dk_flagged_ids <- character()
    dk_flag_reason <- character()
    dk_n_flag <- 0L
  } else {
    enum_dk_rates$z_score <- (enum_dk_rates$dk_rate - team_mean) / team_sd
    flagged <- enum_dk_rates |> dplyr::filter(z_score > threshold_sd)
    dk_n_flag <- nrow(flagged)
    if (dk_n_flag > 0L) {
      dk_flagged_ids <- as_id(flagged$enum)
      dk_flag_reason <- paste0(
        "DK rate ", round(flagged$dk_rate * 100, 1),
        "% vs team avg ", round(team_mean * 100, 1), "%"
      )
    } else {
      dk_flagged_ids <- character()
      dk_flag_reason <- character()
    }
  }

  new_check_result(
    check_name     = "B03_dk_rate",
    check_category = "enumerator",
    n_flagged      = dk_n_flag,
    n_total        = nrow(enum_dk_rates),
    flagged_ids    = dk_flagged_ids,
    flag_reason    = dk_flag_reason,
    severity       = "warning",
    summary_stat   = list(
      enum_dk_rates = enum_dk_rates,
      team_mean     = team_mean,
      team_sd       = team_sd
    )
  )
}

#' Check time gaps between consecutive surveys by enumerator
#'
#' B.08: Flag surveys where the gap from the previous survey start time
#' is impossibly short (less than min_gap_minutes).
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param enum_col Character. Name of the enumerator ID column
#' @param start_time_col Character. Name of the survey start time column
#'   (parsed with as.POSIXct())
#' @param min_gap_minutes Numeric. Minimum expected gap in minutes (default 5)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_enumerator_time_gap <- function(data, id_col, enum_col, start_time_col,
                                       min_gap_minutes = 5, ...) {
  assert_columns(data, c(id_col, enum_col, start_time_col),
                 context = "check_enumerator_time_gap")

  ids <- as_id(data[[id_col]])
  enums <- as_id(data[[enum_col]])
  times <- as.POSIXct(data[[start_time_col]], tz = "UTC")
  n_total <- nrow(data)

  work_df <- dplyr::tibble(id = ids, enum = enums, start_time = times) |>
    dplyr::filter(!is.na(enum), !is.na(start_time)) |>
    dplyr::arrange(enum, start_time) |>
    dplyr::group_by(enum) |>
    dplyr::mutate(
      gap_minutes = as.numeric(difftime(start_time, dplyr::lag(start_time),
                                        units = "mins"))
    ) |>
    dplyr::ungroup()

  # Flag surveys with gap below threshold
  short_gaps <- work_df |>
    dplyr::filter(!is.na(gap_minutes), gap_minutes < min_gap_minutes)

  new_check_result(
    check_name     = "B08_enumerator_time_gap",
    check_category = "enumerator",
    n_flagged      = nrow(short_gaps),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(short_gaps$id),
    flag_reason    = if (nrow(short_gaps) > 0) {
      paste0("Only ", round(short_gaps$gap_minutes, 1),
             " minutes after previous survey")
    } else character(),
    severity       = "warning",
    summary_stat   = list(
      gap_details = short_gaps
    )
  )
}
