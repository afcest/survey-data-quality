#' Check "other (specify)" write-in responses
#'
#' O.01: Extracts all unique "other (specify)" write-in responses for review.
#' Flags records that have non-empty other_col values so they can be recoded
#' or validated during data cleaning.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param other_col Character. Name of the "other specify" text column
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_other_specify <- function(data, id_col, other_col, ...) {
  assert_columns(data, c(id_col, other_col), context = "check_other_specify")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  values <- as.character(data[[other_col]])
  non_empty <- !is.na(values) & trimws(values) != ""

  flagged_ids <- ids[non_empty]
  flagged_values <- values[non_empty]

  # Truncate display values to 50 characters

  flag_reason <- vapply(flagged_values, function(v) {
    if (nchar(v) > 50) {
      paste0("Other specify: '", substr(v, 1, 50), "...'")
    } else {
      paste0("Other specify: '", v, "'")
    }
  }, character(1), USE.NAMES = FALSE)


  # Build value counts for summary
  value_counts <- dplyr::tibble(value = flagged_values) |>
    dplyr::count(.data$value, name = "n", sort = TRUE)

  new_check_result(
    check_name     = "O01_other_specify",
    check_category = "text_quality",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "info",
    summary_stat   = list(
      value_counts = value_counts,
      n_unique_values = nrow(value_counts)
    )
  )
}

#' Check text response length
#'
#' O.02: Flags text responses that are too short or too long. Responses
#' shorter than min_length or longer than max_length characters are flagged.
#' NA and empty strings are excluded from flagging.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param text_col Character. Name of the text column to check
#' @param min_length Integer. Minimum acceptable length (default 3)
#' @param max_length Integer. Maximum acceptable length (default 500)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_text_length <- function(data, id_col, text_col,
                              min_length = 3, max_length = 500, ...) {
  assert_columns(data, c(id_col, text_col), context = "check_text_length")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  values <- as.character(data[[text_col]])

  # Exclude NA and empty strings from consideration
  has_text <- !is.na(values) & trimws(values) != ""
  text_lengths <- nchar(values)

  too_short <- has_text & text_lengths < min_length
  too_long  <- has_text & text_lengths > max_length

  flagged_mask <- too_short | too_long
  flagged_ids <- ids[flagged_mask]
  flagged_lengths <- text_lengths[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    len <- flagged_lengths[i]
    if (len < min_length) {
      paste0("Text too short: ", len, " chars (min ", min_length, ")")
    } else {
      paste0("Text too long: ", len, " chars (max ", max_length, ")")
    }
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "O02_text_length",
    check_category = "text_quality",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      n_too_short = sum(too_short),
      n_too_long  = sum(too_long),
      n_excluded_na_empty = sum(!has_text)
    )
  )
}

#' Check for duplicated text responses (copy-paste detection)
#'
#' O.03: Flags identical text responses across different surveys.
#' Only considers text values with at least min_length characters
#' to avoid false positives on short common answers.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param text_col Character. Name of the text column to check
#' @param min_length Integer. Minimum text length to consider (default 10)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_text_duplicates <- function(data, id_col, text_col,
                                  min_length = 10, ...) {
  assert_columns(data, c(id_col, text_col), context = "check_text_duplicates")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  values <- as.character(data[[text_col]])

  # Only consider non-empty text above min_length
  eligible <- !is.na(values) & nchar(trimws(values)) >= min_length
  eligible_values <- trimws(values[eligible])
  eligible_ids <- ids[eligible]

  # Find duplicated text values
  text_counts <- table(eligible_values)
  duped_texts <- names(text_counts[text_counts > 1])

  # Flag all IDs that share a duplicated text value
  duped_mask <- eligible_values %in% duped_texts
  flagged_ids <- eligible_ids[duped_mask]
  flagged_texts <- eligible_values[duped_mask]

  flag_reason <- vapply(flagged_texts, function(txt) {
    n <- as.integer(text_counts[[txt]])
    display <- if (nchar(txt) > 50) paste0(substr(txt, 1, 50), "...") else txt
    paste0("Text duplicated ", n, " times: '", display, "'")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "O03_text_duplicate",
    check_category = "text_quality",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      n_duplicate_groups = length(duped_texts),
      n_eligible_texts   = sum(eligible)
    )
  )
}

#' Check name format validity
#'
#' O.04: Flags names that are NA/empty, only digits, known test values,
#' or single characters. Useful for catching enumerator shortcuts and
#' data entry mistakes in respondent name fields.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param name_col Character. Name of the name column to check
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_name_format <- function(data, id_col, name_col, ...) {
  assert_columns(data, c(id_col, name_col), context = "check_name_format")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  values <- as.character(data[[name_col]])
  trimmed <- trimws(tolower(values))

  # Known test / placeholder values

  test_values <- c("test", "xxx", "aaa", "asdf", "na", "none", ".", "-")

  # Build flag masks
  is_missing    <- is.na(values) | trimws(values) == ""
  is_digits     <- !is_missing & grepl("^\\d+$", trimws(values))
  is_test       <- !is_missing & trimmed %in% test_values
  is_single     <- !is_missing & !is.na(values) & nchar(trimws(values)) == 1L

  # Combine and build reasons
  flagged_mask <- is_missing | is_digits | is_test | is_single
  flagged_ids <- ids[flagged_mask]

  flag_reason <- vapply(which(flagged_mask), function(i) {
    if (is_missing[i])    return("Name is missing or empty")
    if (is_digits[i])     return(paste0("Name contains only digits: '", trimws(values[i]), "'"))
    if (is_test[i])       return(paste0("Name is a known test value: '", trimmed[i], "'"))
    if (is_single[i])     return(paste0("Name is a single character: '", trimws(values[i]), "'"))
    "Invalid name format"
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "O04_name_format",
    check_category = "text_quality",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      n_missing     = sum(is_missing),
      n_digits_only = sum(is_digits),
      n_test_value  = sum(is_test),
      n_single_char = sum(is_single)
    )
  )
}

#' Check phone number format
#'
#' O.05: Validates phone numbers by extracting digits and checking
#' length bounds. Optionally validates country code prefix.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param phone_col Character. Name of the phone number column
#' @param country_code Character or NULL. Expected country code prefix
#'   (e.g., "+226"). If provided, numbers not starting with this prefix
#'   (after stripping non-digits except leading +) are flagged.
#' @param min_digits Integer. Minimum number of digits (default 8)
#' @param max_digits Integer. Maximum number of digits (default 15)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_phone_format <- function(data, id_col, phone_col,
                               country_code = NULL,
                               min_digits = 8, max_digits = 15, ...) {
  assert_columns(data, c(id_col, phone_col), context = "check_phone_format")

  ids <- as_id(data[[id_col]])
  n_total <- nrow(data)

  raw_phones <- as.character(data[[phone_col]])

  # Extract digits only for length check
  digits_only <- gsub("[^0-9]", "", raw_phones)
  digit_count <- nchar(digits_only)

  # Exclude NA and empty
  has_value <- !is.na(raw_phones) & trimws(raw_phones) != ""

  # Length violations
  too_few  <- has_value & digit_count < min_digits
  too_many <- has_value & digit_count > max_digits

  # Country code check (preserve leading + for prefix matching)
  bad_prefix <- rep(FALSE, n_total)
  if (!is.null(country_code)) {
    # Normalize: strip everything except digits and leading +
    normalized <- gsub("[^0-9+]", "", raw_phones)
    # Extract expected digit prefix from country_code
    code_digits <- gsub("[^0-9]", "", country_code)
    bad_prefix <- has_value &
      !startsWith(normalized, country_code) &
      !startsWith(digits_only, code_digits)
  }

  flagged_mask <- too_few | too_many | bad_prefix
  flagged_ids <- ids[flagged_mask]

  flag_reason <- vapply(which(flagged_mask), function(i) {
    reasons <- character(0)
    if (too_few[i])  reasons <- c(reasons, paste0(digit_count[i], " digits (min ", min_digits, ")"))
    if (too_many[i]) reasons <- c(reasons, paste0(digit_count[i], " digits (max ", max_digits, ")"))
    if (bad_prefix[i]) reasons <- c(reasons, paste0("missing prefix ", country_code))
    paste0("Phone '", raw_phones[i], "': ", paste(reasons, collapse = "; "))
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "O05_phone_format",
    check_category = "text_quality",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = as.character(flagged_ids),
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      n_too_few_digits  = sum(too_few),
      n_too_many_digits = sum(too_many),
      n_bad_prefix      = sum(bad_prefix),
      n_na_or_empty     = sum(!has_value)
    )
  )
}
