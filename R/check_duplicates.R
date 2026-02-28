#' Check for duplicate survey IDs
#'
#' A.01: Finds records sharing the same primary key.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_duplicate_ids <- function(data, id_col, ...) {
  assert_columns(data, id_col, context = "check_duplicate_ids")

  ids <- as_id(data[[id_col]])
  n_total <- length(ids)
  duped <- duplicated(ids) | duplicated(ids, fromLast = TRUE)
  duped_ids <- unique(ids[duped & !is.na(ids)])

  # Build flag reasons using table() for O(n) instead of O(n^2)
  id_counts <- table(ids[!is.na(ids)])
  flag_reason <- vapply(duped_ids, function(id) {
    paste0("ID '", id, "' appears ", id_counts[[id]], " times")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "A01_duplicate_id",
    check_category = "identification",
    n_flagged      = length(duped_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(duped_ids),
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      n_unique = length(unique(ids[!is.na(ids)])),
      n_duplicated_groups = length(duped_ids)
    )
  )
}

#' Check for fingerprint duplicates (different IDs, same quasi-identifiers)
#'
#' A.02: Records with different IDs but matching GPS, phone, or name.
#'
#' @param data Data frame
#' @param id_col Primary key column
#' @param quasi_ids Character vector of quasi-identifier column names
#' @param ... Reserved
#' @return A check_result object
#' @export
check_duplicate_fingerprint <- function(data, id_col, quasi_ids, ...) {
  assert_columns(data, c(id_col, quasi_ids), context = "check_duplicate_fingerprint")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  # Build fingerprint hash from quasi-identifiers
  fp <- apply(data[, quasi_ids, drop = FALSE], 1, function(row) {
    paste(as.character(row), collapse = "|")
  })

  # Find duplicate fingerprints with different IDs
  fp_df <- dplyr::tibble(.id = ids, .fp = fp)
  duped_fp <- fp_df |>
    dplyr::group_by(.fp) |>
    dplyr::filter(dplyr::n_distinct(.id) > 1) |>
    dplyr::ungroup()

  flagged_ids <- unique(duped_fp$.id)

  new_check_result(
    check_name     = "A02_fingerprint_duplicate",
    check_category = "identification",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = rep("Different ID but matching quasi-identifiers", length(flagged_ids)),
    severity       = "error",
    summary_stat   = list(
      quasi_identifiers = quasi_ids,
      n_duplicate_groups = dplyr::n_distinct(duped_fp$.fp)
    )
  )
}

#' Check for missing primary IDs
#'
#' A.03: Records with NA or empty ID field.
#'
#' @param data Data frame
#' @param id_col Primary key column
#' @param ... Reserved
#' @return A check_result object
#' @export
check_missing_ids <- function(data, id_col, ...) {
  assert_columns(data, id_col, context = "check_missing_ids")

  ids <- as_id(data[[id_col]])
  n_total <- length(ids)
  missing_mask <- is.na(ids) | ids == "" | ids == "."
  n_missing <- sum(missing_mask)

  # Use row indices as identifiers for missing-ID records
  flagged_rows <- which(missing_mask)

  new_check_result(
    check_name     = "A03_missing_id",
    check_category = "identification",
    n_flagged      = as.integer(n_missing),
    n_total        = as.integer(n_total),
    flagged_ids    = paste0("row_", flagged_rows),
    flag_reason    = rep("Primary ID is missing or empty", n_missing),
    severity       = "error"
  )
}

#' Check IDs against sampling frame
#'
#' A.05: IDs not in the master sampling list.
#'
#' @param data Data frame
#' @param id_col Primary key column
#' @param sampling_frame Character vector of valid IDs from master list
#' @param ... Reserved
#' @return A check_result object
#' @export
check_id_in_sample <- function(data, id_col, sampling_frame, ...) {
  assert_columns(data, id_col, context = "check_id_in_sample")

  ids <- as_id(data[[id_col]])
  frame_ids <- as.character(sampling_frame)
  n_total <- length(ids[!is.na(ids)])

  not_in_frame <- setdiff(ids[!is.na(ids)], frame_ids)

  new_check_result(
    check_name     = "A05_id_not_in_sample",
    check_category = "identification",
    n_flagged      = length(not_in_frame),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(not_in_frame),
    flag_reason    = rep("ID not found in sampling frame", length(not_in_frame)),
    severity       = "error",
    summary_stat   = list(
      n_in_frame = sum(ids[!is.na(ids)] %in% frame_ids),
      n_not_in_frame = length(not_in_frame),
      frame_size = length(frame_ids)
    )
  )
}
