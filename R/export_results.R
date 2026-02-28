#' Export check results to an Excel workbook
#'
#' Creates a multi-sheet Excel file with check results, flagged records,
#' and summary statistics. Requires the openxlsx2 package.
#'
#' @param report A list of check_result objects (e.g., from running checks)
#' @param data Original data frame (for extracting flagged rows)
#' @param id_col Character. ID column name
#' @param path Character. Output file path (.xlsx)
#' @param ... Reserved for future use
#' @return Invisible path to the created file
#' @export
export_to_excel <- function(report, data, id_col, path, ...) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    cli::cli_abort("Install openxlsx2: {.code install.packages('openxlsx2')}")
  }

  # --- Prepare data ---
  summary_tbl <- bind_check_results(report)
  flagged_tbl <- build_flagged_table(report)
  categories  <- unique(summary_tbl$check_category)

  # --- Create workbook ---
  wb <- openxlsx2::wb_workbook()

  # Helper: header dims string e.g. "A1:F1"
  make_header_dims <- function(ncols) {
    paste0("A1:", openxlsx2::int2col(ncols), "1")
  }

  # ---- Sheet 1: Summary ----
  wb$add_worksheet("Summary")
  wb$add_data("Summary", summary_tbl)

  hd_sum <- make_header_dims(ncol(summary_tbl))
  wb$add_font("Summary", dims = hd_sum, bold = TRUE,
              color = openxlsx2::wb_color("FFFFFF"))
  wb$add_fill("Summary", dims = hd_sum,
              color = openxlsx2::wb_color("4472C4"))

  # ---- Sheet 2: Flagged Records ----
  wb$add_worksheet("Flagged Records")
  if (nrow(flagged_tbl) > 0) {
    wb$add_data("Flagged Records", flagged_tbl)
    hd_flag <- make_header_dims(ncol(flagged_tbl))
    wb$add_font("Flagged Records", dims = hd_flag, bold = TRUE,
                color = openxlsx2::wb_color("FFFFFF"))
    wb$add_fill("Flagged Records", dims = hd_flag,
                color = openxlsx2::wb_color("4472C4"))
  } else {
    wb$add_data("Flagged Records", dplyr::tibble(message = "No flagged records"))
  }

  # ---- Per-category sheets ----
  ids <- as_id(data[[id_col]])

  for (cat in categories) {
    sheet_name <- format_sheet_name(cat)

    cat_checks <- Filter(
      function(cr) is_check_result(cr) && cr$check_category == cat,
      report
    )
    cat_flagged_ids <- unique(unlist(lapply(cat_checks, `[[`, "flagged_ids")))
    if (length(cat_flagged_ids) == 0L) next

    match_rows <- ids %in% cat_flagged_ids
    if (!any(match_rows)) next

    cat_data <- data[match_rows, , drop = FALSE]
    wb$add_worksheet(sheet_name)
    wb$add_data(sheet_name, cat_data)

    hd_cat <- make_header_dims(ncol(cat_data))
    wb$add_font(sheet_name, dims = hd_cat, bold = TRUE,
                color = openxlsx2::wb_color("FFFFFF"))
    wb$add_fill(sheet_name, dims = hd_cat,
                color = openxlsx2::wb_color("4472C4"))
  }

  # --- Write to disk ---
  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  cli::cli_alert_success("Excel report saved to {.file {path}}")

  invisible(path)
}

#' Export check results to CSV files
#'
#' Writes summary and flagged records as separate CSV files.
#'
#' @param report A list of check_result objects
#' @param output_dir Character. Directory for output files
#' @param prefix Character. File name prefix (default "hfc_report")
#' @param ... Reserved for future use
#' @return Invisible list of file paths created
#' @export
export_to_csv <- function(report, output_dir, prefix = "hfc_report", ...) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_info("Created output directory: {.file {output_dir}}")
  }

  summary_tbl <- bind_check_results(report)
  flagged_tbl <- build_flagged_table(report)

  summary_path <- file.path(output_dir, paste0(prefix, "_summary.csv"))
  flagged_path <- file.path(output_dir, paste0(prefix, "_flagged.csv"))

  utils::write.csv(summary_tbl, summary_path, row.names = FALSE)
  cli::cli_alert_success("Summary written to {.file {summary_path}}")

  utils::write.csv(flagged_tbl, flagged_path, row.names = FALSE)
  cli::cli_alert_success("Flagged records written to {.file {flagged_path}}")

  invisible(list(summary = summary_path, flagged = flagged_path))
}


# -- Internal helpers ----------------------------------------------------------

#' Build a long-format flagged records table from check results
#'
#' @param report A list of check_result objects
#' @return A tibble with columns: check_name, flagged_id, flag_reason, severity
#' @keywords internal
build_flagged_table <- function(report) {
  # Flatten if nested
  flatten_results <- function(x) {
    if (is_check_result(x)) return(list(x))
    if (is.list(x)) return(unlist(lapply(x, flatten_results), recursive = FALSE))
    list()
  }
  results <- flatten_results(report)

  rows <- lapply(results, function(cr) {
    if (cr$n_flagged == 0L) return(NULL)
    dplyr::tibble(
      check_name = rep(cr$check_name, cr$n_flagged),
      flagged_id = cr$flagged_ids,
      flag_reason = if (length(cr$flag_reason) > 0) cr$flag_reason else rep(NA_character_, cr$n_flagged),
      severity   = rep(cr$severity, cr$n_flagged)
    )
  })

  out <- dplyr::bind_rows(rows)
  # Ensure columns exist even when empty

  if (nrow(out) == 0L) {
    out <- dplyr::tibble(
      check_name  = character(),
      flagged_id  = character(),
      flag_reason = character(),
      severity    = character()
    )
  }
  out
}

#' Format a check category name into a valid Excel sheet name
#'
#' Capitalizes the first letter and truncates to 31 characters
#' (Excel sheet name limit).
#'
#' @param cat Character. Category name
#' @return Character. Formatted sheet name
#' @keywords internal
format_sheet_name <- function(cat) {
  # Capitalize first letter, replace underscores with spaces
  name <- gsub("_", " ", cat)
  name <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
  # Excel sheet names max 31 chars
  if (nchar(name) > 31) name <- substr(name, 1, 31)
  name
}
