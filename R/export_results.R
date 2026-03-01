#' Export check results to an Excel workbook (AfCEST professional data quality report format)
#'
#' Creates a multi-sheet Excel workbook in AfCEST professional
#' data quality report format with up to 18 sheets covering
#' README, summary, duplicate IDs, outliers, logic checks, constraints,
#' other-specify, missing data, skip patterns, survey tracking, GPS issues,
#' timing issues, fabrication indicators, enumerator dashboard, flagged
#' records, corrections template, back-check results, and field comments.
#'
#' @param report An `adc_report` object (from [run_all_checks()]) or a
#'   list of `check_result` objects.
#' @param data Optional original data frame. When provided, category sheets
#'   include full data rows for flagged records.
#' @param id_col Character. ID column name in `data`.
#' @param path Character. Output file path (`.xlsx`). Defaults to
#'   `~/Downloads/hfc_report.xlsx`.
#' @param enum_col Character. Optional enumerator column name. When provided,
#'   adds an "Enumerator Dashboard" sheet.
#' @param date_col Character. Optional date column name. When provided,
#'   includes submission dates in relevant sheets.
#' @param keep_cols Character vector of additional column names from `data` to
#'   include in the Flagged Records output (e.g., village name, phone number).
#'   These columns help field teams identify and locate respondents. Columns
#'   that do not exist in `data` are silently skipped with a warning.
#' @param sheets Character. Which sheets to include. `"all"` (default) includes
#'   all 18 sheets. A character vector (e.g., `c("summary", "corrections")`)
#'   includes only those sheets plus README. A negative character vector
#'   (e.g., `c("-fabrication", "-back_check")`) excludes those from all.
#' @param backcheck_data Optional data frame with back-check comparison data.
#' @param ... Reserved for future use.
#' @return Invisible path to the created file.
#' @export
export_to_excel <- function(report, data = NULL, id_col = NULL,
                            path = file.path(path.expand("~/Downloads"), "hfc_report.xlsx"),
                            enum_col = NULL, date_col = NULL,
                            keep_cols = NULL, sheets = "all",
                            backcheck_data = NULL, ...) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    cli::cli_abort("Install openxlsx2: {.code install.packages('openxlsx2')}")
  }

  # --- Extract check results --------------------------------------------------
  results <- .extract_results(report)
  summary_tbl <- do.call(bind_check_results, results)
  flagged_tbl <- tryCatch(
    .build_flagged_table_full(results, data, id_col,
                              enum_col, date_col,
                              keep_cols),
    error = function(e) {
      cli::cli_warn("Failed to build flagged table: {e$message}")
      # Return empty flagged table so export can continue with partial data
      dplyr::tibble(
        row_number = integer(), enumerator = character(),
        date = character(), id = character(),
        check_name = character(), check_category = character(),
        variable = character(), value = character(),
        flag_reason = character(), severity = character(),
        action = character()
      )
    }
  )

  # --- Build context for sheet builders ---------------------------------------
  ctx <- list(
    results        = results,
    summary_tbl    = summary_tbl,
    flagged_tbl    = flagged_tbl,
    data           = data,
    id_col         = id_col,
    enum_col       = enum_col,
    date_col       = date_col,
    keep_cols      = keep_cols,
    backcheck_data = backcheck_data
  )

  # --- Resolve which sheets to include ----------------------------------------
  sheet_keys <- .resolve_sheets(sheets)

  # --- Styles -----------------------------------------------------------------
  blue_bg   <- openxlsx2::wb_color("4472C4")
  white_txt <- openxlsx2::wb_color("FFFFFF")
  red_bg    <- openxlsx2::wb_color("FF0000")
  orange_bg <- openxlsx2::wb_color("FFA500")
  info_bg   <- openxlsx2::wb_color("4472C4")

  # --- Create workbook --------------------------------------------------------
  wb <- openxlsx2::wb_workbook()

  # --- Loop over resolved sheets and build each -------------------------------
  for (key in sheet_keys) {
    entry <- .sheet_registry[[key]]
    if (is.null(entry)) next

    sheet_result <- tryCatch({
      builder_fn <- get(entry$builder, envir = asNamespace("afcestDataCheck"))
      builder_fn(ctx)
    }, error = function(e) {
      cli::cli_warn("Sheet builder {.val {key}} failed: {e$message}")
      NULL
    })
    if (is.null(sheet_result)) next

    sheet_name <- sheet_result$title
    df <- sheet_result$df
    if (!is.data.frame(df)) next

    wb$add_worksheet(sheet_name)
    wb$add_data(sheet_name, df)

    # --- Special formatting per sheet type ------------------------------------
    if (key == "readme") {
      # README gets wider columns, no filter
      .style_header(wb, sheet_name, ncol(df))
      wb$set_col_widths(sheet_name, cols = 1, widths = 30)
      wb$set_col_widths(sheet_name, cols = 2, widths = 80)
      wb$freeze_pane(sheet_name, first_row = TRUE)
    } else if (key == "corrections") {
      # Corrections gets data validation dropdown
      .style_header(wb, sheet_name, ncol(df))
      .auto_widths(wb, sheet_name, df)
      action_col_idx <- which(names(df) == "action")
      if (length(action_col_idx) == 1L) {
        action_letter <- openxlsx2::int2col(action_col_idx)
        n_rows_corr <- max(nrow(df), 1L)
        max_row <- max(n_rows_corr + 1L, 100L)
        val_dims <- paste0(action_letter, "2:", action_letter, max_row)
        wb$add_data_validation(
          sheet_name,
          dims = val_dims,
          type = "list",
          value = '"replace,drop,okay,pending"'
        )
      }
      wb$freeze_pane(sheet_name, first_row = TRUE)
    } else {
      # Standard sheet: header, auto-widths, filter, freeze
      .style_header(wb, sheet_name, ncol(df))
      .auto_widths(wb, sheet_name, df)
      if (nrow(df) > 0) {
        wb$add_filter(sheet_name, rows = 1,
                      cols = seq_len(ncol(df)))
      }
      wb$freeze_pane(sheet_name, first_row = TRUE)
    }

    # --- Color-code severity columns (batch per severity level) ---------------
    if ("severity" %in% names(df) && nrow(df) > 0) {
      sev_col_idx <- which(names(df) == "severity")
      if (length(sev_col_idx) == 1L) {
        sev_letter <- openxlsx2::int2col(sev_col_idx)
        sev_colors <- list(
          error   = red_bg,
          warning = orange_bg,
          info    = info_bg
        )
        for (sev_level in names(sev_colors)) {
          rows_idx <- which(df$severity == sev_level)
          if (length(rows_idx) == 0L) next
          # Chunk into batches of 500 to avoid huge dims strings
          chunks <- split(rows_idx, ceiling(seq_along(rows_idx) / 500L))
          for (chunk in chunks) {
            cell_refs <- paste0(sev_letter, chunk + 1L)
            dims_str <- paste(cell_refs, collapse = ",")
            wb$add_fill(sheet_name, dims = dims_str,
                        color = sev_colors[[sev_level]])
            wb$add_font(sheet_name, dims = dims_str,
                        color = white_txt, bold = TRUE)
          }
        }
      }
    }

    # --- Enumerator dashboard: red highlight on high flag rates ----------------
    if (key == "enumerator_dashboard" && "flag_rate" %in% names(df) &&
        nrow(df) > 0) {
      fr_col_idx <- which(names(df) == "flag_rate")
      if (length(fr_col_idx) == 1L) {
        fr_letter <- openxlsx2::int2col(fr_col_idx)
        high_rows <- which(!is.na(df$flag_rate) & df$flag_rate > 0.20)
        if (length(high_rows) > 0) {
          cell_refs <- paste0(fr_letter, high_rows + 1L)
          dims_str <- paste(cell_refs, collapse = ",")
          wb$add_fill(sheet_name, dims = dims_str, color = red_bg)
          wb$add_font(sheet_name, dims = dims_str,
                      color = white_txt, bold = TRUE)
        }
      }
    }
  }

  # --- Write to disk ----------------------------------------------------------
  if (length(wb$worksheets) == 0L) {
    cli::cli_abort("No sheets were produced. Check your {.arg sheets} parameter and data.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  cli::cli_alert_success("Excel report saved to {.file {path}}")

  invisible(path)
}


#' Export check results to CSV files
#'
#' Writes multiple CSV files with AfCEST data quality report structure:
#' dashboard, flagged records, corrections template, and optional
#' enumerator stats.
#'
#' @param report An `adc_report` object or a list of `check_result` objects.
#' @param output_dir Character. Directory for output files.
#' @param prefix Character. File name prefix (default `"hfc_report"`).
#' @param data Optional original data frame.
#' @param id_col Character. ID column name.
#' @param enum_col Character. Optional enumerator column name.
#' @param date_col Character. Optional date column name.
#' @param keep_cols Character vector of additional column names from `data` to
#'   include in the Flagged Records output (e.g., village name, phone number).
#'   These columns help field teams identify and locate respondents. Columns
#'   that do not exist in `data` are silently skipped with a warning.
#' @param ... Reserved for future use.
#' @return Invisible list of file paths created.
#' @export
export_to_csv <- function(report, output_dir, prefix = "hfc_report",
                          data = NULL, id_col = NULL,
                          enum_col = NULL, date_col = NULL,
                          keep_cols = NULL, ...) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_info("Created output directory: {.file {output_dir}}")
  }

  results     <- .extract_results(report)
  summary_tbl <- do.call(bind_check_results, results)
  flagged_tbl <- tryCatch(
    .build_flagged_table_full(results, data, id_col,
                              enum_col, date_col,
                              keep_cols),
    error = function(e) {
      cli::cli_warn("Failed to build flagged table: {e$message}")
      dplyr::tibble(
        row_number = integer(), enumerator = character(),
        date = character(), id = character(),
        check_name = character(), check_category = character(),
        variable = character(), value = character(),
        flag_reason = character(), severity = character(),
        action = character()
      )
    }
  )

  paths <- list()

  # Dashboard CSV
  dash <- .build_dashboard(results, summary_tbl, flagged_tbl,
                           data, enum_col, date_col)
  paths$dashboard <- file.path(output_dir, paste0(prefix, "_dashboard.csv"))
  utils::write.csv(dash, paths$dashboard, row.names = FALSE)
  cli::cli_alert_success("Dashboard written to {.file {paths$dashboard}}")

  # Flagged records CSV
  paths$flagged_records <- file.path(output_dir,
                                     paste0(prefix, "_flagged_records.csv"))
  utils::write.csv(flagged_tbl, paths$flagged_records, row.names = FALSE)
  cli::cli_alert_success(
    "Flagged records written to {.file {paths$flagged_records}}"
  )

  # Corrections template CSV
  corrections_df <- .build_corrections_template(flagged_tbl)
  paths$corrections <- file.path(output_dir,
                                 paste0(prefix, "_corrections_template.csv"))
  utils::write.csv(corrections_df, paths$corrections, row.names = FALSE)
  cli::cli_alert_success(
    "Corrections template written to {.file {paths$corrections}}"
  )

  # Enumerator stats CSV (optional)
  if (!is.null(enum_col) && !is.null(data)) {
    enum_stats <- .build_enumerator_stats(data, flagged_tbl, enum_col)
    if (!is.null(enum_stats) && nrow(enum_stats) > 0) {
      paths$enumerator_stats <- file.path(
        output_dir, paste0(prefix, "_enumerator_stats.csv")
      )
      utils::write.csv(enum_stats, paths$enumerator_stats, row.names = FALSE)
      cli::cli_alert_success(
        "Enumerator stats written to {.file {paths$enumerator_stats}}"
      )
    }
  }

  invisible(paths)
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Extract a flat list of check_result objects from a report or raw list
#' @param report An adc_report or list of check_result objects
#' @return List of check_result objects
#' @keywords internal
.extract_results <- function(report) {
  if (inherits(report, "adc_report")) {
    return(report$results)
  }
  # Recursively flatten
  flatten_results <- function(x) {
    if (is_check_result(x)) return(list(x))
    if (is.list(x)) return(unlist(lapply(x, flatten_results), recursive = FALSE))
    list()
  }
  flatten_results(report)
}


#' Build the full flagged records table
#'
#' Columns: row_number, enumerator, date, id, gps_lat, gps_lon, duration,
#' any `keep_cols`, check_name, check_category, variable, value,
#' flag_reason, severity, action.
#'
#' @param results List of check_result objects.
#' @param data Optional original data frame.
#' @param id_col Character. ID column name in `data`.
#' @param enum_col Character. Optional enumerator column name.
#' @param date_col Character. Optional date column name.
#' @param keep_cols Character vector of additional column names from `data` to
#'   include in the output. Columns not found in `data` are skipped with a
#'   warning.
#' @return A tibble with context columns (row_number, enumerator, date, id,
#'   gps_lat, gps_lon, duration), any requested keep_cols, and check detail
#'   columns (check_name, check_category, variable, value, flag_reason,
#'   severity, action).
#' @keywords internal
.build_flagged_table_full <- function(results, data = NULL, id_col = NULL,
                                      enum_col = NULL, date_col = NULL,
                                      keep_cols = NULL) {
  rows <- lapply(results, function(cr) {
    if (!is_check_result(cr) || cr$n_flagged == 0L) return(NULL)

    n <- cr$n_flagged
    flag_reasons <- if (length(cr$flag_reason) == n) {
      cr$flag_reason
    } else if (length(cr$flag_reason) == 1L) {
      rep(cr$flag_reason, n)
    } else {
      rep(NA_character_, n)
    }

    # Extract variable name from check_result (summary_stat$variable first,
    # then fallback to parsing check_name)
    variable <- .get_check_variable(cr)

    out <- dplyr::tibble(
      id             = cr$flagged_ids,
      check_name     = rep(cr$check_name, n),
      check_category = rep(cr$check_category, n),
      variable       = rep(variable, n),
      flag_reason    = flag_reasons,
      severity       = rep(cr$severity, n)
    )

    # Add value and row_number columns: look up in original data
    if (!is.null(data) && !is.null(id_col) && id_col %in% names(data)) {
      ids_lookup <- as_id(data[[id_col]])
      match_idx <- match(as_id(cr$flagged_ids), ids_lookup)
      unmatched <- is.na(match_idx)
      if (any(unmatched)) {
        n_unmatched <- sum(unmatched)
        cli::cli_warn("{n_unmatched} flagged ID{?s} not found in data.")
      }
      out$row_number <- ifelse(is.na(match_idx), NA_integer_,
                               as.integer(match_idx))
      if (!is.null(variable) && variable %in% names(data)) {
        out$value <- ifelse(is.na(match_idx), NA_character_,
                            as.character(data[[variable]][match_idx]))
      } else {
        out$value <- rep(NA_character_, n)
      }
    } else {
      out$row_number <- rep(NA_integer_, n)
      out$value <- rep(NA_character_, n)
    }

    out
  })

  out <- dplyr::bind_rows(rows)

  if (nrow(out) == 0L) {
    empty_tbl <- dplyr::tibble(
      row_number     = integer(),
      enumerator     = character(),
      date           = character(),
      id             = character(),
      gps_lat        = numeric(),
      gps_lon        = numeric(),
      duration       = numeric()
    )
    # Add keep_cols as empty character columns
    if (!is.null(keep_cols) && !is.null(data)) {
      valid_keep <- intersect(keep_cols, names(data))
      for (kc in valid_keep) {
        empty_tbl[[kc]] <- character()
      }
    }
    empty_tbl$check_name     <- character()
    empty_tbl$check_category <- character()
    empty_tbl$variable       <- character()
    empty_tbl$value          <- character()
    empty_tbl$flag_reason    <- character()
    empty_tbl$severity       <- character()
    empty_tbl$action         <- character()
    return(empty_tbl)
  }

  # Consolidate match lookup
  if (!is.null(data) && !is.null(id_col) && id_col %in% names(data) && nrow(out) > 0L) {
    ids_lookup <- as_id(data[[id_col]])
    match_idx <- match(as_id(out$id), ids_lookup)
  } else {
    match_idx <- rep(NA_integer_, nrow(out))
  }

  # Add enumerator column
  if (!is.null(enum_col) && !is.null(data) &&
      enum_col %in% names(data)) {
    out$enumerator <- ifelse(is.na(match_idx), NA_character_,
                             as.character(data[[enum_col]][match_idx]))
  } else {
    out$enumerator <- rep(NA_character_, nrow(out))
  }

  # Add date column
  if (!is.null(date_col) && !is.null(data) &&
      date_col %in% names(data)) {
    out$date <- ifelse(is.na(match_idx), NA_character_,
                       as.character(data[[date_col]][match_idx]))
  } else {
    out$date <- rep(NA_character_, nrow(out))
  }

  # Add GPS coordinates if available
  lat_col <- NA_character_
  lon_col <- NA_character_
  if (!is.null(data)) {
    # Auto-detect GPS columns
    lat_candidates <- c("gps_lat", "gps_latitude", "latitude", "lat",
                        "_geopoint_latitude")
    lon_candidates <- c("gps_lon", "gps_longitude", "longitude", "lon",
                        "_geopoint_longitude")
    found_lat <- intersect(lat_candidates, names(data))
    found_lon <- intersect(lon_candidates, names(data))
    if (length(found_lat) > 0) lat_col <- found_lat[1]
    if (length(found_lon) > 0) lon_col <- found_lon[1]
  }

  if (!is.na(lat_col) && !is.na(lon_col) && nrow(out) > 0L) {
    out$gps_lat <- ifelse(is.na(match_idx), NA_real_,
                          data[[lat_col]][match_idx])
    out$gps_lon <- ifelse(is.na(match_idx), NA_real_,
                          data[[lon_col]][match_idx])
  }

  # Add duration if available
  dur_candidates <- c("duration", "duration_minutes", "interview_duration")
  dur_col <- if (!is.null(data)) intersect(dur_candidates, names(data))[1] else NA_character_
  if (!is.na(dur_col) && nrow(out) > 0L) {
    out$duration <- ifelse(is.na(match_idx), NA_real_,
                           data[[dur_col]][match_idx])
  }

  # Empty action column for user to fill
  out$action <- rep(NA_character_, nrow(out))

  # Add keep_cols: extra columns from data for field team identification
  keep_col_names <- character()
  if (!is.null(keep_cols) && !is.null(data) && !is.null(id_col) &&
      id_col %in% names(data) && nrow(out) > 0L) {
    # Warn about missing columns, then use only valid ones
    missing_kc <- setdiff(keep_cols, names(data))
    if (length(missing_kc) > 0L) {
      cli::cli_warn(
        "keep_cols not found in data (skipped): {.val {missing_kc}}"
      )
    }
    reserved_cols <- c("row_number", "enumerator", "date", "id", "gps_lat",
                       "gps_lon", "duration", "check_name", "check_category",
                       "variable", "value", "flag_reason", "severity", "action")
    valid_keep <- setdiff(intersect(keep_cols, names(data)), reserved_cols)
    if (length(valid_keep) > 0L) {
      for (kc in valid_keep) {
        out[[kc]] <- ifelse(is.na(match_idx), NA_character_,
                            as.character(data[[kc]][match_idx]))
      }
      keep_col_names <- valid_keep
    }
  }

  # Reorder columns: row_number and context first, keep_cols, then check details
  base_cols <- c("row_number", "enumerator", "date", "id")
  context_cols <- intersect(c("gps_lat", "gps_lon", "duration"), names(out))
  check_cols <- c("check_name", "check_category", "variable", "value",
                  "flag_reason", "severity", "action")
  col_order <- intersect(
    c(base_cols, context_cols, keep_col_names, check_cols),
    names(out)
  )
  out <- out[, col_order, drop = FALSE]

  out
}


#' Build the Dashboard summary sheet
#'
#' Two-column vertical layout with section headers.
#'
#' @keywords internal
.build_dashboard <- function(results, summary_tbl, flagged_tbl,
                             data = NULL, enum_col = NULL, date_col = NULL) {
  rows <- list()
  add_section <- function(title) {
    rows[[length(rows) + 1L]] <<- c(title, NA_character_)
  }
  add_row <- function(desc, val) {
    rows[[length(rows) + 1L]] <<- c(as.character(desc), as.character(val))
  }

  # --- Submissions ---
  add_section("SUBMISSIONS")
  n_obs <- if (!is.null(data)) nrow(data) else NA
  add_row("Total observations", n_obs)

  if (!is.null(date_col) && !is.null(data) && date_col %in% names(data)) {
    dates <- as.character(data[[date_col]])
    dates <- dates[!is.na(dates) & dates != ""]
    if (length(dates) > 0) {
      parsed_dates <- tryCatch(as.Date(dates), error = function(e) NULL)
      if (!is.null(parsed_dates)) {
        parsed_dates <- parsed_dates[!is.na(parsed_dates)]
        if (length(parsed_dates) > 0) {
          add_row("Date range", paste(min(parsed_dates), "to", max(parsed_dates)))
        } else {
          add_row("Date range", "N/A")
        }
      } else {
        add_row("Date range", paste(min(dates), "to", max(dates)))
      }
    } else {
      add_row("Date range", "N/A")
    }
  } else {
    add_row("Date range", "N/A")
  }

  if (!is.null(enum_col) && !is.null(data) && enum_col %in% names(data)) {
    n_enum <- length(unique(data[[enum_col]][!is.na(data[[enum_col]])]))
    add_row("Number of enumerators", n_enum)
  } else {
    add_row("Number of enumerators", "N/A")
  }

  # --- Data Quality ---
  add_section("DATA QUALITY")
  n_checks <- nrow(summary_tbl)
  n_with_flags <- sum(summary_tbl$n_flagged > 0L)
  n_total_flags <- if (nrow(flagged_tbl) > 0) nrow(flagged_tbl) else 0L
  n_errors <- sum(flagged_tbl$severity == "error", na.rm = TRUE)
  n_warnings <- sum(flagged_tbl$severity == "warning", na.rm = TRUE)

  add_row("Checks run", n_checks)
  add_row("Checks with flags", n_with_flags)
  add_row("Total flags", n_total_flags)
  add_row("Errors", n_errors)

  add_row("Warnings", n_warnings)

  # --- By Category ---
  add_section("BY CATEGORY")
  if (nrow(flagged_tbl) > 0 && "check_category" %in% names(flagged_tbl)) {
    cat_counts <- table(flagged_tbl$check_category)
    for (cat_name in sort(names(cat_counts))) {
      add_row(format_sheet_name(cat_name), as.integer(cat_counts[[cat_name]]))
    }
  } else {
    add_row("(no flags)", 0)
  }

  # --- Enumerator Summary ---
  add_section("ENUMERATOR SUMMARY")
  if (!is.null(enum_col) && !is.null(data) && enum_col %in% names(data)) {
    enums <- unique(data[[enum_col]][!is.na(data[[enum_col]])])
    n_enum <- length(enums)
    avg_surveys <- if (n_enum > 0) round(nrow(data) / n_enum, 1) else 0
    add_row("Number of enumerators", n_enum)
    add_row("Avg surveys per enumerator", avg_surveys)
  } else {
    add_row("Number of enumerators", "N/A")
    add_row("Avg surveys per enumerator", "N/A")
  }

  mat <- do.call(rbind, rows)
  df <- data.frame(Description = mat[, 1], Value = mat[, 2],
                   stringsAsFactors = FALSE)
  df
}


#' Build the Corrections template sheet
#'
#' Filters to only record-level, actionable flags that a field team can act on.
#' Excludes fabrication checks (flag variables, not records), aggregate
#' completeness checks (C01, C02 flag variables/enumerators), and other
#' non-actionable flags.
#'
#' @keywords internal
.build_corrections_template <- function(flagged_tbl) {
  template <- dplyr::tibble(
    id        = character(),
    variable  = character(),
    old_value = character(),
    new_value = character(),
    action    = character(),
    reason    = character()
  )

  if (nrow(flagged_tbl) > 0) {
    # Filter to actionable, record-level flags only
    actionable <- flagged_tbl

    # Exclude fabrication checks (flag variables, not records)
    if ("check_category" %in% names(actionable)) {
      actionable <- actionable[is.na(actionable$check_category) |
                                 actionable$check_category != "fabrication", , drop = FALSE]
    }

    # Exclude aggregate completeness checks that flag variables/enumerators
    if ("check_name" %in% names(actionable)) {
      exclude_pattern <- "^C01_missing_by_variable$|^C02_|^C04_all_missing_variable$"
      actionable <- actionable[!grepl(exclude_pattern, actionable$check_name), , drop = FALSE]
    }

    if (nrow(actionable) > 0L) {
      example_rows <- dplyr::tibble(
        id        = actionable$id,
        variable  = if ("variable" %in% names(actionable)) actionable$variable else NA_character_,
        old_value = if ("value" %in% names(actionable)) actionable$value else NA_character_,
        new_value = rep(NA_character_, nrow(actionable)),
        action    = rep("pending", nrow(actionable)),
        reason    = if ("flag_reason" %in% names(actionable)) actionable$flag_reason else NA_character_
      )

      template <- dplyr::bind_rows(template, example_rows)
    }
  }

  template
}


#' Build Enumerator Stats sheet
#' @keywords internal
.build_enumerator_stats <- function(data, flagged_tbl, enum_col) {
  if (!enum_col %in% names(data)) return(NULL)

  # Surveys per enumerator
  enum_surveys <- as.data.frame(
    table(enumerator = data[[enum_col]]),
    stringsAsFactors = FALSE
  )
  names(enum_surveys) <- c("enumerator", "n_surveys")

  # Flags per enumerator
  if (nrow(flagged_tbl) > 0 && "enumerator" %in% names(flagged_tbl)) {
    enum_flags <- as.data.frame(
      table(enumerator = flagged_tbl$enumerator),
      stringsAsFactors = FALSE
    )
    names(enum_flags) <- c("enumerator", "n_flags")

    # Errors and warnings per enumerator
    errors_df <- flagged_tbl[flagged_tbl$severity == "error", , drop = FALSE]
    warns_df  <- flagged_tbl[flagged_tbl$severity == "warning", , drop = FALSE]

    enum_errors <- if (nrow(errors_df) > 0) {
      df <- as.data.frame(
        table(enumerator = errors_df$enumerator),
        stringsAsFactors = FALSE
      )
      names(df) <- c("enumerator", "n_errors")
      df
    } else {
      data.frame(enumerator = character(), n_errors = integer(),
                 stringsAsFactors = FALSE)
    }

    enum_warns <- if (nrow(warns_df) > 0) {
      df <- as.data.frame(
        table(enumerator = warns_df$enumerator),
        stringsAsFactors = FALSE
      )
      names(df) <- c("enumerator", "n_warnings")
      df
    } else {
      data.frame(enumerator = character(), n_warnings = integer(),
                 stringsAsFactors = FALSE)
    }
  } else {
    enum_flags  <- data.frame(enumerator = character(), n_flags = integer(),
                              stringsAsFactors = FALSE)
    enum_errors <- data.frame(enumerator = character(), n_errors = integer(),
                              stringsAsFactors = FALSE)
    enum_warns  <- data.frame(enumerator = character(), n_warnings = integer(),
                              stringsAsFactors = FALSE)
  }

  # Merge all
  out <- merge(enum_surveys, enum_flags, by = "enumerator", all.x = TRUE)
  out <- merge(out, enum_errors, by = "enumerator", all.x = TRUE)
  out <- merge(out, enum_warns, by = "enumerator", all.x = TRUE)

  # Fill NAs with 0
  out$n_flags[is.na(out$n_flags)]       <- 0L
  out$n_errors[is.na(out$n_errors)]     <- 0L
  out$n_warnings[is.na(out$n_warnings)] <- 0L

  # Compute flag rate
  out$flag_rate <- round(
    ifelse(out$n_surveys > 0L, out$n_flags / out$n_surveys, NA_real_),
    4
  )

  # Sort by flag_rate descending
  out <- out[order(-out$flag_rate), ]

  # Final column order
  out <- out[, c("enumerator", "n_surveys", "n_flags", "flag_rate",
                 "n_errors", "n_warnings"), drop = FALSE]
  rownames(out) <- NULL

  out
}


#' Extract a variable name from a check_result object
#'
#' Reads `summary_stat$variable` first (where outlier, range, negative,
#' fabrication checks store it), then falls back to parsing the check_name
#' string for checks that encode the variable in the name.
#'
#' @param cr A check_result object.
#' @return Character scalar: the variable name, or `NA_character_`.
#' @keywords internal
.get_check_variable <- function(cr) {
  if (is.list(cr$summary_stat) && !is.null(cr$summary_stat$variable)) {
    return(as.character(cr$summary_stat$variable))
  }
  parts <- strsplit(cr$check_name, "_", fixed = TRUE)[[1]]
  if (length(parts) >= 4L) return(paste(parts[4:length(parts)], collapse = "_"))
  NA_character_
}


#' Style header row with blue background and white bold text
#' @keywords internal
.style_header <- function(wb, sheet, ncols) {
  if (ncols == 0L) return(invisible(wb))
  dims <- paste0("A1:", openxlsx2::int2col(ncols), "1")
  wb$add_font(sheet, dims = dims, bold = TRUE,
              color = openxlsx2::wb_color("FFFFFF"))
  wb$add_fill(sheet, dims = dims,
              color = openxlsx2::wb_color("4472C4"))
  invisible(wb)
}


#' Set reasonable column widths
#' @keywords internal
.auto_widths <- function(wb, sheet, df) {
  ncols <- ncol(df)
  # Sample rows for width estimation
  sample_df <- if (nrow(df) > 200L) df[seq_len(200L), , drop = FALSE] else df
  widths <- vapply(seq_len(ncols), function(i) {
    header_len <- nchar(names(df)[i])
    max_data <- if (nrow(sample_df) > 0) {
      vals <- nchar(as.character(sample_df[[i]]))
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) max(vals) else 0L
    } else {
      0L
    }
    min(max(header_len, max_data, 8L) + 2L, 50L)
  }, numeric(1))
  wb$set_col_widths(sheet, cols = seq_len(ncols), widths = widths)
  invisible(wb)
}


#' Build a long-format flagged records table from check results
#'
#' @param report A list of check_result objects
#' @return A tibble with columns: check_name, flagged_id, flag_reason,
#'   severity. This is the simplified version without context columns;
#'   see [.build_flagged_table_full()] for the full version with
#'   row_number, enumerator, date, GPS, duration, and keep_cols.
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
    if (!is_check_result(cr) || cr$n_flagged == 0L) return(NULL)
    dplyr::tibble(
      check_name = rep(cr$check_name, cr$n_flagged),
      flagged_id = cr$flagged_ids,
      flag_reason = if (length(cr$flag_reason) > 0) cr$flag_reason else rep(NA_character_, cr$n_flagged),
      severity   = rep(cr$severity, cr$n_flagged)
    )
  })

  out <- dplyr::bind_rows(rows)

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
  # Remove Excel-forbidden characters
  name <- gsub('[\\\\/*?:\\[\\]]', '_', name, perl = TRUE)
  name <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
  # Excel sheet names max 31 chars
  if (nchar(name) > 31) name <- substr(name, 1, 31)
  name
}


# =============================================================================
# Sheet Registry & Resolution
# =============================================================================

#' Registry of all available sheets
#' @keywords internal
.sheet_registry <- list(
  readme               = list(name = "Guide",                builder = ".build_sheet_readme"),
  summary              = list(name = "Check Summary",        builder = ".build_sheet_summary"),
  duplicate_ids        = list(name = "Duplicate IDs",        builder = ".build_sheet_duplicates"),
  outliers             = list(name = "Outliers",             builder = ".build_sheet_outliers"),
  logic                = list(name = "Logic Errors",         builder = ".build_sheet_logic"),
  constraints          = list(name = "Constraint Violations",builder = ".build_sheet_constraints"),
  other_specify        = list(name = "Other Specify",        builder = ".build_sheet_other_specify"),
  all_missing          = list(name = "Missing Data",         builder = ".build_sheet_all_missing"),
  skip_patterns        = list(name = "Skip Violations",      builder = ".build_sheet_skip_patterns"),
  survey_tracking      = list(name = "Progress Tracker",     builder = ".build_sheet_survey_tracking"),
  gps_issues           = list(name = "GPS Issues",           builder = ".build_sheet_gps"),
  timing_issues        = list(name = "Timing Issues",        builder = ".build_sheet_timing"),
  fabrication          = list(name = "Fabrication Flags",     builder = ".build_sheet_fabrication"),
  enumerator_dashboard = list(name = "Enumerator Performance", builder = ".build_sheet_enumerator"),
  flagged_records      = list(name = "All Flags",            builder = ".build_sheet_flagged"),
  corrections          = list(name = "Corrections Log",      builder = ".build_sheet_corrections"),
  back_check           = list(name = "Back-Check Results",   builder = ".build_sheet_backcheck"),
  field_comments       = list(name = "Field Comments",       builder = ".build_sheet_field_comments")
)


#' Resolve which sheets to include based on the `sheets` parameter
#'
#' @param sheets Character. `"all"`, a positive vector of keys, or a negative
#'   vector (prefixed with `-`) to exclude.
#' @return Character vector of sheet registry keys to include.
#' @keywords internal
.resolve_sheets <- function(sheets) {
  all_keys <- names(.sheet_registry)

  if (anyNA(sheets)) {
    cli::cli_abort("{.arg sheets} must not contain NA values.")
  }

  if (length(sheets) == 0L) {
    cli::cli_abort("{.arg sheets} must not be empty.")
  }

  if (length(sheets) == 1L && sheets == "all") {
    return(all_keys)
  }

  # Check for mixed positive/negative — not allowed
  has_neg <- grepl("^-", sheets)
  if (any(has_neg) && !all(has_neg)) {
    cli::cli_abort(
      "{.arg sheets} must be all positive or all negative (prefixed with {.val -}), not mixed."
    )
  }

  # Negative selection: exclude specified keys
  if (all(has_neg)) {
    exclude <- sub("^-", "", sheets)
    invalid <- setdiff(exclude, all_keys)
    if (length(invalid) > 0L) {
      cli::cli_warn("Unknown sheet keys (ignored): {.val {invalid}}")
    }
    return(setdiff(all_keys, exclude))
  }

  # Positive selection: include only specified keys (+ always README)
  invalid <- setdiff(sheets, all_keys)
  if (length(invalid) > 0L) {
    cli::cli_warn("Unknown sheet keys (ignored): {.val {invalid}}")
  }
  selected <- intersect(sheets, all_keys)
  # Always include README
  selected <- unique(c("readme", selected))
  # Maintain registry order
  all_keys[all_keys %in% selected]
}


# =============================================================================
# Sheet Builder Functions
# =============================================================================

#' Build Sheet: Guide (README)
#' @keywords internal
.build_sheet_readme <- function(ctx) {
  df <- dplyr::tibble(
    Section = c(
      "Report Overview",
      "Sheet: Check Summary",
      "Sheet: Duplicate IDs",
      "Sheet: Outliers",
      "Sheet: Logic Errors",
      "Sheet: Constraint Violations",
      "Sheet: Other Specify",
      "Sheet: Missing Data",
      "Sheet: Skip Violations",
      "Sheet: Progress Tracker",
      "Sheet: GPS Issues",
      "Sheet: Timing Issues",
      "Sheet: Fabrication Flags",
      "Sheet: Enumerator Performance",
      "Sheet: All Flags",
      "Sheet: Corrections Log",
      "Sheet: Back-Check Results",
      "Sheet: Field Comments",
      "Severity: error",
      "Severity: warning",
      "Severity: info",
      "Action Column",
      "Corrections Sheet",
      "Generated By"
    ),
    Description = c(
      "This workbook contains high-frequency data quality check results. Each sheet focuses on a specific category of issues found in the survey data.",
      "One row per check executed, showing pass/fail status and the number of flagged observations.",
      "Records with duplicate household or respondent IDs that require deduplication.",
      "Observations with values outside expected ranges (IQR or SD-based outlier detection).",
      "Records failing logical consistency checks between related variables.",
      "Values violating hard range constraints, negative value checks, or other data constraints.",
      "Free-text responses in 'other (specify)' fields that may need recoding into existing categories.",
      "Variables with high rates of missing values or records where all key variables are missing.",
      "Records where skip pattern logic was violated (answered questions that should have been skipped, or vice versa).",
      "Survey progress tracking: daily submission counts, completion rates, and target monitoring.",
      "Records with GPS coordinate issues: missing coordinates, low accuracy, or coordinates outside expected areas.",
      "Records with interview duration issues: too short, too long, or unusual timing patterns.",
      "Indicators of potential data fabrication: low variance within enumerator, identical response patterns, suspicious timing.",
      "Per-enumerator performance metrics: survey counts, flag rates, errors, and productivity indicators.",
      "Master list of all flagged records across all checks, sorted by severity.",
      "Template for recording corrections. Use the Action column dropdown (replace/drop/okay/pending) to track resolution.",
      "Back-check comparison results: re-interview coverage and response match rates.",
      "Free-text field comments and paradata notes from enumerators.",
      "Must fix before data can be used. These are critical data quality issues.",
      "Should review. These are potential issues that may or may not require correction.",
      "For your information. These are minor observations that do not require action.",
      "Use the Action column in All Flags to mark how each flag was resolved: replace (correct the value), drop (remove the record), okay (flag reviewed, no action needed), pending (not yet reviewed).",
      "Use the Corrections Log sheet to document all data changes. Fill in: id, variable, old_value, new_value, action, and reason.",
      "Generated by the afcestDataCheck R package."
    )
  )
  list(df = df, title = "Guide")
}


#' Human-readable descriptions for common check codes
#' @keywords internal
.check_descriptions <- c(

  # Identification
  A01_duplicate_id              = "Records sharing the same household ID",
  A02_duplicate_id_enumerator   = "Same ID submitted by the same enumerator",
  A03_duplicate_id_date         = "Same ID submitted on the same date",


  # Completeness
  C01_missing_by_variable       = "Variables with missing values above threshold",
  C02_enumerator_missing_rate   = "Enumerators with high rates of missing data",
  C03_missing_key_variable      = "Key identification variables with missing values",
  C04_all_missing_variable      = "Variables that are 100% missing across all records",

  # Outliers
  D01_outlier_iqr               = "Extreme values detected (IQR method)",
  D02_outlier_sd                = "Extreme values detected (standard deviation method)",

  # Constraints
  D03_hard_range                = "Values outside hard minimum/maximum range",
  D04_negative_value            = "Negative values in fields that should be non-negative",

  # GPS
  E01_gps_missing               = "Records with missing GPS coordinates",
  E02_gps_low_accuracy          = "GPS readings with low accuracy (high error radius)",
  E03_gps_outside_boundary      = "GPS coordinates outside expected geographic area",
  E04_gps_duplicate_location    = "Multiple records at the exact same GPS location",
  E05_gps_distance_outlier      = "GPS location unusually far from other records",
  E06_gps_at_centroid           = "GPS coordinates at administrative area centroid",
  E07_gps_null_island           = "GPS coordinates at 0,0 (default/missing location)",

  # Timing
  F01_short_duration            = "Interview completed too quickly (suspiciously short)",
  F02_long_duration             = "Interview took unusually long",
  F03_off_hours                 = "Interview conducted outside normal working hours",
  F04_start_end_mismatch        = "Interview start/end timestamps are inconsistent",

  # Logic
  G01_logic_check               = "Logical inconsistency between related variables",

  # Skip patterns
  H01_skip_pattern              = "Skip logic violated (answered when should skip, or vice versa)",

  # Other specify
  K01_other_specify             = "Free-text 'other' responses that may need recoding",

  # Fabrication
  M01_benford_first_digit       = "First-digit distribution deviates from expected pattern",
  M02_benford_second_digit      = "Second-digit distribution deviates from expected pattern",
  M03_digit_preference          = "Terminal digit distribution shows systematic preference"
)


#' Build Sheet: Check Summary
#' @keywords internal
.build_sheet_summary <- function(ctx) {
  stbl <- ctx$summary_tbl
  if (nrow(stbl) == 0L) {
    df <- dplyr::tibble(
      check_name = character(), description = character(),
      check_category = character(),
      n_flagged = integer(), n_total = integer(),
      pct_flagged = numeric(), severity = character(),
      status = character()
    )
    return(list(df = df, title = "Check Summary"))
  }

  # Look up description: try exact match, then match by prefix (for variable-

  # specific checks like D01_outlier_iqr_income -> D01_outlier_iqr)
  descriptions <- vapply(stbl$check_name, function(cn) {
    if (cn %in% names(.check_descriptions)) return(.check_descriptions[[cn]])
    # Try matching by prefix (strip trailing _varname)
    parts <- strsplit(cn, "_", fixed = TRUE)[[1]]
    for (n_parts in min(length(parts), 4):2) {
      prefix <- paste(parts[seq_len(n_parts)], collapse = "_")
      if (prefix %in% names(.check_descriptions)) return(.check_descriptions[[prefix]])
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)

  df <- dplyr::tibble(
    check_name     = stbl$check_name,
    description    = descriptions,
    check_category = if ("check_category" %in% names(stbl)) stbl$check_category else NA_character_,
    n_flagged      = stbl$n_flagged,
    n_total        = if ("n_total" %in% names(stbl)) stbl$n_total else NA_integer_,
    pct_flagged    = if ("pct_flagged" %in% names(stbl)) stbl$pct_flagged else NA_real_,
    severity       = stbl$severity,
    status         = ifelse(stbl$n_flagged == 0L, "PASS", "FAIL")
  )
  list(df = df, title = "Check Summary")
}


#' Build Sheet: Duplicate IDs
#' @keywords internal
.build_sheet_duplicates <- function(ctx) {
  ft <- ctx$flagged_tbl
  if (nrow(ft) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Duplicate IDs"))
  }

  mask <- (!is.na(ft$check_category) & ft$check_category == "identification") |
          grepl("duplicate", ft$check_name, ignore.case = TRUE)
  subset <- ft[mask, , drop = FALSE]
  if (nrow(subset) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Duplicate IDs"))
  }

  # Build output with key identifying columns from data (not all 31+ columns)
  if (!is.null(ctx$data) && !is.null(ctx$id_col) &&
      ctx$id_col %in% names(ctx$data)) {
    dup_ids <- unique(subset$id)
    ids_lookup <- as_id(ctx$data[[ctx$id_col]])
    match_rows <- ids_lookup %in% as_id(dup_ids)
    if (any(match_rows)) {
      # Select only key identifying columns
      key_cols <- unique(c(ctx$id_col, ctx$enum_col, ctx$date_col,
                           "name_head", "phone", "village", "district"))
      key_cols <- intersect(key_cols[!is.na(key_cols)], names(ctx$data))
      dup_data <- ctx$data[match_rows, key_cols, drop = FALSE]
      # Add flag info
      flag_info <- unique(subset[, c("id", "check_name", "flag_reason", "severity"),
                                  drop = FALSE])
      dup_data$`.merge_id` <- as_id(dup_data[[ctx$id_col]])
      flag_info$`.merge_id` <- as_id(flag_info$id)
      df <- merge(dup_data, flag_info[, c(".merge_id", "check_name",
                                           "flag_reason", "severity")],
                  by = ".merge_id", all.x = TRUE,
                  suffixes = c("", ".flag"))
      df$`.merge_id` <- NULL
      return(list(df = dplyr::as_tibble(df), title = "Duplicate IDs"))
    }
  }

  # Fallback: just the flagged table subset
  cols <- intersect(c("id", "enumerator", "date", "check_name",
                       "flag_reason", "severity"), names(subset))
  df <- subset[, cols, drop = FALSE]
  list(df = dplyr::as_tibble(df), title = "Duplicate IDs")
}


#' Build Sheet: Outliers
#' @keywords internal
.build_sheet_outliers <- function(ctx) {
  ft <- ctx$flagged_tbl
  if (nrow(ft) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Outliers"))
  }

  mask <- !is.na(ft$check_category) & ft$check_category == "outliers"
  subset <- ft[mask, , drop = FALSE]
  if (nrow(subset) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Outliers"))
  }

  cols <- intersect(c("id", "enumerator", "variable", "value",
                       "flag_reason", "severity"), names(subset))
  df <- subset[, cols, drop = FALSE]
  list(df = dplyr::as_tibble(df), title = "Outliers")
}


#' Build Sheet: Logic Errors
#' @keywords internal
.build_sheet_logic <- function(ctx) {
  ft <- ctx$flagged_tbl
  if (nrow(ft) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Logic Errors"))
  }

  mask <- !is.na(ft$check_category) & ft$check_category == "logic"
  subset <- ft[mask, , drop = FALSE]
  if (nrow(subset) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Logic Errors"))
  }

  cols <- intersect(c("id", "enumerator", "variable", "value",
                       "check_name", "flag_reason", "severity"), names(subset))
  df <- subset[, cols, drop = FALSE]
  list(df = dplyr::as_tibble(df), title = "Logic Errors")
}


#' Build Sheet: Constraint Violations
#' @keywords internal
.build_sheet_constraints <- function(ctx) {
  ft <- ctx$flagged_tbl
  if (nrow(ft) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Constraint Violations"))
  }

  mask <- grepl("hard_range|negative|constraint", ft$check_name, ignore.case = TRUE)
  subset <- ft[mask, , drop = FALSE]
  if (nrow(subset) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Constraint Violations"))
  }

  cols <- intersect(c("id", "enumerator", "variable", "value",
                       "check_name", "flag_reason", "severity"), names(subset))
  df <- subset[, cols, drop = FALSE]
  if ("check_name" %in% names(df)) {
    names(df)[names(df) == "check_name"] <- "constraint_type"
  }
  list(df = dplyr::as_tibble(df), title = "Constraint Violations")
}


#' Build Sheet: Other Specify
#' @keywords internal
.build_sheet_other_specify <- function(ctx) {
  ft <- ctx$flagged_tbl
  if (nrow(ft) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Other Specify"))
  }

  mask <- grepl("other_specify", ft$check_name, ignore.case = TRUE)
  subset <- ft[mask, , drop = FALSE]
  if (nrow(subset) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Other Specify"))
  }

  cols <- intersect(c("id", "enumerator", "variable", "value",
                       "flag_reason", "severity"), names(subset))
  df <- subset[, cols, drop = FALSE]
  list(df = dplyr::as_tibble(df), title = "Other Specify")
}


#' Build Sheet: Missing Data
#'
#' Builds a clean table of per-variable missing data statistics from:
#' - C01_missing_by_variable: has `summary_stat$miss_stats` (tibble with
#'   variable, n_missing, n_total, miss_rate) and `flagged_ids` = variable names
#' - C04_all_missing_variable: `flagged_ids` = variable names (100% missing)
#' - Any other completeness check with per-variable stats
#'
#' @keywords internal
.build_sheet_all_missing <- function(ctx) {
  results <- ctx$results
  if (length(results) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Missing Data"))
  }

  rows <- list()

  for (cr in results) {
    if (!is_check_result(cr)) next
    if (!isTRUE(cr$check_category == "completeness") &&
        !grepl("missing|all_missing", cr$check_name, ignore.case = TRUE)) next

    # C01: extract miss_stats tibble (one row per flagged variable)
    if (is.list(cr$summary_stat) &&
        is.data.frame(cr$summary_stat$miss_stats)) {
      ms <- cr$summary_stat$miss_stats
      # Filter to flagged variables only (those in flagged_ids)
      if (cr$n_flagged > 0L && length(cr$flagged_ids) > 0L) {
        ms <- ms[ms$variable %in% cr$flagged_ids, , drop = FALSE]
      }
      if (nrow(ms) > 0L) {
        rows[[length(rows) + 1L]] <- dplyr::tibble(
          variable    = as.character(ms$variable),
          n_missing   = as.integer(if ("n_missing" %in% names(ms)) ms$n_missing else NA_integer_),
          n_total     = as.integer(if ("n_total" %in% names(ms)) ms$n_total else NA_integer_),
          pct_missing = as.numeric(if ("miss_rate" %in% names(ms)) round(ms$miss_rate * 100, 1)
                                   else NA_real_),
          severity    = rep(cr$severity, nrow(ms)),
          check_name  = rep(cr$check_name, nrow(ms))
        )
      }
      next
    }

    # C04 and others: flagged_ids ARE variable names
    if (cr$n_flagged > 0L && length(cr$flagged_ids) > 0L) {
      n <- cr$n_flagged
      flag_reasons <- if (length(cr$flag_reason) == n) {
        cr$flag_reason
      } else if (length(cr$flag_reason) == 1L) {
        rep(cr$flag_reason, n)
      } else {
        rep(NA_character_, n)
      }
      rows[[length(rows) + 1L]] <- dplyr::tibble(
        variable    = cr$flagged_ids,
        n_missing   = rep(NA_integer_, n),
        n_total     = rep(NA_integer_, n),
        pct_missing = rep(NA_real_, n),
        severity    = rep(cr$severity, n),
        check_name  = rep(cr$check_name, n)
      )
    }
  }

  if (length(rows) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Missing Data"))
  }

  df <- dplyr::bind_rows(rows)
  # Sort by pct_missing descending (NAs last)
  df <- df[order(-df$pct_missing, na.last = TRUE), ]
  list(df = df, title = "Missing Data")
}


#' Build Sheet: Skip Violations
#' @keywords internal
.build_sheet_skip_patterns <- function(ctx) {
  ft <- ctx$flagged_tbl
  if (nrow(ft) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Skip Violations"))
  }

  mask <- grepl("skip_pattern", ft$check_name, ignore.case = TRUE)
  subset <- ft[mask, , drop = FALSE]
  if (nrow(subset) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Skip Violations"))
  }

  cols <- intersect(c("id", "enumerator", "variable", "value",
                       "flag_reason", "severity"), names(subset))
  df <- subset[, cols, drop = FALSE]
  list(df = dplyr::as_tibble(df), title = "Skip Violations")
}


#' Build Sheet: Progress Tracker
#' @keywords internal
.build_sheet_survey_tracking <- function(ctx) {
  results <- ctx$results
  if (length(results) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Progress Tracker"))
  }

  rows <- list()
  for (cr in results) {
    if (!is_check_result(cr)) next
    if (!grepl("survey_tracking", cr$check_name, ignore.case = TRUE)) next
    # Extract summary_stat data if available
    if (is.list(cr$summary_stat) && length(cr$summary_stat) > 0) {
      stat_df <- tryCatch(
        dplyr::as_tibble(cr$summary_stat),
        error = function(e) {
          dplyr::tibble(
            check_name = cr$check_name,
            n_flagged  = cr$n_flagged,
            severity   = cr$severity
          )
        }
      )
      rows[[length(rows) + 1L]] <- stat_df
    } else {
      rows[[length(rows) + 1L]] <- dplyr::tibble(
        check_name = cr$check_name,
        n_flagged  = cr$n_flagged,
        severity   = cr$severity
      )
    }
  }

  if (length(rows) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Progress Tracker"))
  }
  df <- dplyr::bind_rows(rows)
  list(df = df, title = "Progress Tracker")
}


#' Build Sheet: GPS Issues
#' @keywords internal
.build_sheet_gps <- function(ctx) {
  ft <- ctx$flagged_tbl
  if (nrow(ft) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "GPS Issues"))
  }

  mask <- !is.na(ft$check_category) & ft$check_category == "gps"
  subset <- ft[mask, , drop = FALSE]
  if (nrow(subset) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "GPS Issues"))
  }

  base_cols <- intersect(c("id", "enumerator"), names(subset))
  # Rename check_name to check_type
  df <- subset[, base_cols, drop = FALSE]
  df$check_type <- subset$check_name

  # Add GPS columns if available
  if ("gps_lat" %in% names(subset)) df$gps_lat <- subset$gps_lat
  if ("gps_lon" %in% names(subset)) df$gps_lon <- subset$gps_lon

  # Add gps_accuracy from data if available
  if (!is.null(ctx$data) && !is.null(ctx$id_col)) {
    acc_candidates <- c("gps_accuracy", "accuracy", "_geopoint_accuracy")
    acc_col <- intersect(acc_candidates, names(ctx$data))
    if (length(acc_col) > 0) {
      ids_lookup <- as_id(ctx$data[[ctx$id_col]])
      match_idx <- match(as_id(subset$id), ids_lookup)
      df$gps_accuracy <- ifelse(is.na(match_idx), NA_real_,
                                 ctx$data[[acc_col[1]]][match_idx])
    }
  }

  df$flag_reason <- subset$flag_reason
  df$severity <- subset$severity
  list(df = dplyr::as_tibble(df), title = "GPS Issues")
}


#' Build Sheet: Timing Issues
#' @keywords internal
.build_sheet_timing <- function(ctx) {
  ft <- ctx$flagged_tbl
  if (nrow(ft) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Timing Issues"))
  }

  mask <- !is.na(ft$check_category) & ft$check_category == "timing"
  subset <- ft[mask, , drop = FALSE]
  if (nrow(subset) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Timing Issues"))
  }

  base_cols <- intersect(c("id", "enumerator"), names(subset))
  df <- subset[, base_cols, drop = FALSE]
  df$check_type <- subset$check_name

  # Add date if available
  if ("date" %in% names(subset)) df$date <- subset$date

  # Add duration if available
  if ("duration" %in% names(subset)) df$duration <- subset$duration

  df$flag_reason <- subset$flag_reason
  df$severity <- subset$severity
  list(df = dplyr::as_tibble(df), title = "Timing Issues")
}


#' Build Sheet: Fabrication Flags
#'
#' Builds one row per fabrication check result directly from the results list.
#' Fabrication checks (M01 Benford 1st digit, M02 Benford 2nd digit,
#' M03 Digit preference) flag variables not records, so the flagged table
#' approach doesn't work well. Instead we read summary_stat directly.
#'
#' @keywords internal
.build_sheet_fabrication <- function(ctx) {
  results <- ctx$results

  rows <- list()
  for (cr in results) {
    if (!is_check_result(cr)) next
    if (!isTRUE(cr$check_category == "fabrication")) next

    variable <- if (is.list(cr$summary_stat) && !is.null(cr$summary_stat$variable)) {
      as.character(cr$summary_stat$variable)
    } else {
      NA_character_
    }

    # Determine check type label from check_name
    check_type <- if (grepl("benford.*first|M01", cr$check_name, ignore.case = TRUE)) {
      "Benford 1st digit"
    } else if (grepl("benford.*second|M02", cr$check_name, ignore.case = TRUE)) {
      "Benford 2nd digit"
    } else if (grepl("digit_pref|terminal|M03", cr$check_name, ignore.case = TRUE)) {
      "Digit preference"
    } else {
      cr$check_name
    }

    # Extract test statistics
    ss <- cr$summary_stat
    chi_sq <- if (is.list(ss) && !is.null(ss$chi_sq_stat)) round(as.numeric(ss$chi_sq_stat), 3) else NA_real_
    p_val  <- if (is.list(ss) && !is.null(ss$p_value)) round(as.numeric(ss$p_value), 4) else NA_real_

    result_label <- if (cr$n_flagged > 0L) "FAIL" else "PASS"
    flag_reason <- if (length(cr$flag_reason) >= 1L) cr$flag_reason[1] else NA_character_

    rows[[length(rows) + 1L]] <- dplyr::tibble(
      variable       = variable,
      check_type     = check_type,
      test_statistic = chi_sq,
      p_value        = p_val,
      result         = result_label,
      flag_reason    = flag_reason,
      severity       = cr$severity
    )
  }

  if (length(rows) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Fabrication Flags"))
  }

  df <- dplyr::bind_rows(rows)
  list(df = df, title = "Fabrication Flags")
}


#' Build Sheet: Enumerator Performance
#' @keywords internal
.build_sheet_enumerator <- function(ctx) {
  if (is.null(ctx$enum_col) || is.null(ctx$data)) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Enumerator Performance"))
  }

  enum_stats <- .build_enumerator_stats(ctx$data, ctx$flagged_tbl, ctx$enum_col)
  if (is.null(enum_stats) || nrow(enum_stats) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Enumerator Performance"))
  }

  # Enhance with duration and date stats if available
  data <- ctx$data
  enum_col <- ctx$enum_col
  date_col <- ctx$date_col

  # Duration stats

  dur_candidates <- c("duration", "duration_minutes", "interview_duration")
  dur_col <- intersect(dur_candidates, names(data))
  if (length(dur_col) > 0) {
    dur_col <- dur_col[1]
    dur_vals <- as.numeric(data[[dur_col]])
    enum_vals <- data[[enum_col]]
    avg_dur <- tapply(dur_vals, enum_vals, function(x) { x <- x[!is.na(x)]; if (length(x) == 0L) NA_real_ else mean(x) })
    min_dur <- tapply(dur_vals, enum_vals, function(x) { x <- x[!is.na(x)]; if (length(x) == 0L) NA_real_ else min(x) })
    max_dur <- tapply(dur_vals, enum_vals, function(x) { x <- x[!is.na(x)]; if (length(x) == 0L) NA_real_ else max(x) })
    dur_df <- data.frame(
      enumerator   = names(avg_dur),
      avg_duration = as.numeric(avg_dur),
      min_duration = as.numeric(min_dur),
      max_duration = as.numeric(max_dur),
      stringsAsFactors = FALSE
    )
    enum_stats <- merge(enum_stats, dur_df, by = "enumerator", all.x = TRUE)
  }

  # Date stats
  if (!is.null(date_col) && date_col %in% names(data)) {
    date_vals <- as.character(data[[date_col]])
    enum_vals <- data[[enum_col]]
    first_d <- tapply(date_vals, enum_vals, function(x) { x <- x[!is.na(x) & x != ""]; if (length(x) == 0L) NA_character_ else min(x) })
    last_d  <- tapply(date_vals, enum_vals, function(x) { x <- x[!is.na(x) & x != ""]; if (length(x) == 0L) NA_character_ else max(x) })
    n_d     <- tapply(date_vals, enum_vals, function(x) { x <- x[!is.na(x) & x != ""]; length(unique(x)) })
    date_df <- data.frame(
      enumerator = names(first_d),
      first_date = as.character(first_d),
      last_date  = as.character(last_d),
      n_days     = as.integer(n_d),
      stringsAsFactors = FALSE
    )
    enum_stats <- merge(enum_stats, date_df, by = "enumerator", all.x = TRUE)
  }

  list(df = dplyr::as_tibble(enum_stats), title = "Enumerator Performance")
}


#' Build Sheet: All Flags
#' @keywords internal
.build_sheet_flagged <- function(ctx) {
  ft <- ctx$flagged_tbl
  if (nrow(ft) == 0L) {
    df <- dplyr::tibble(message = "No flagged records found.")
    return(list(df = df, title = "All Flags"))
  }

  # Sort: error > warning > info, then by check_name
  sev_order <- c(error = 1L, warning = 2L, info = 3L)
  ft$`.sev_order` <- sev_order[ft$severity]
  ft <- ft[order(ft$`.sev_order`, ft$check_name), ]
  ft$`.sev_order` <- NULL

  list(df = dplyr::as_tibble(ft), title = "All Flags")
}


#' Build Sheet: Corrections Log
#' @keywords internal
.build_sheet_corrections <- function(ctx) {
  df <- .build_corrections_template(ctx$flagged_tbl)
  list(df = df, title = "Corrections Log")
}


#' Build Sheet: Back-Check Results
#' @keywords internal
.build_sheet_backcheck <- function(ctx) {
  ft <- ctx$flagged_tbl
  results <- ctx$results

  # Filter for backcheck results
  bc_results <- Filter(function(cr) {
    is_check_result(cr) && isTRUE(cr$check_category == "backcheck")
  }, results)

  if (length(bc_results) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Back-Check Results"))
  }

  rows <- list()
  for (cr in bc_results) {
    row <- dplyr::tibble(
      check_name = cr$check_name,
      n_flagged  = cr$n_flagged,
      severity   = cr$severity
    )
    # Add summary_stat values
    if (is.list(cr$summary_stat) && length(cr$summary_stat) > 0) {
      for (stat_name in names(cr$summary_stat)) {
        val <- cr$summary_stat[[stat_name]]
        if (length(val) == 1L) {
          row[[stat_name]] <- val
        }
      }
    }
    rows[[length(rows) + 1L]] <- row
  }

  df <- dplyr::bind_rows(rows)

  # If backcheck_data is provided, add coverage info
  if (!is.null(ctx$backcheck_data) && nrow(ctx$backcheck_data) > 0) {
    bc_summary <- dplyr::tibble(
      check_name = "backcheck_coverage",
      n_flagged  = NA_integer_,
      severity   = "info",
      n_backchecked = nrow(ctx$backcheck_data)
    )
    df <- dplyr::bind_rows(df, bc_summary)
  }

  list(df = df, title = "Back-Check Results")
}


#' Build Sheet: Field Comments
#' @keywords internal
.build_sheet_field_comments <- function(ctx) {
  ft <- ctx$flagged_tbl
  results <- ctx$results
  rows <- list()

  # Filter flagged_tbl for field_comments or paradata comment checks
  if (nrow(ft) > 0) {
    mask <- grepl("field_comment", ft$check_name, ignore.case = TRUE) |
      (!is.na(ft$check_category) & ft$check_category == "paradata" &
       grepl("comment", ft$check_name, ignore.case = TRUE))
    subset <- ft[mask, , drop = FALSE]
    if (nrow(subset) > 0) {
      cols <- intersect(c("id", "enumerator", "variable", "value",
                           "flag_reason", "severity"), names(subset))
      rows[[length(rows) + 1L]] <- subset[, cols, drop = FALSE]
      rows[[length(rows)]]$source <- rep("flag", nrow(rows[[length(rows)]]))
    }
  }

  # Also scan data for a "comments" column with non-NA values
  if (!is.null(ctx$data)) {
    comment_cols <- intersect(c("comments", "comment", "field_comments",
                                 "enumerator_comments", "notes"),
                               names(ctx$data))
    if (length(comment_cols) > 0 && !is.null(ctx$id_col) &&
        ctx$id_col %in% names(ctx$data)) {
      for (cc in comment_cols) {
        vals <- ctx$data[[cc]]
        has_val <- !is.na(vals) & vals != ""
        if (any(has_val)) {
          comment_df <- dplyr::tibble(
            id       = as.character(ctx$data[[ctx$id_col]][has_val]),
            variable = cc,
            value    = as.character(vals[has_val])
          )
          if (!is.null(ctx$enum_col) && ctx$enum_col %in% names(ctx$data)) {
            comment_df$enumerator <- as.character(
              ctx$data[[ctx$enum_col]][has_val]
            )
          }
          comment_df$source <- rep("comment", nrow(comment_df))
          rows[[length(rows) + 1L]] <- comment_df
        }
      }
    }
  }

  if (length(rows) == 0L) {
    df <- dplyr::tibble(message = "No issues found in this category.")
    return(list(df = df, title = "Field Comments"))
  }
  df <- dplyr::bind_rows(rows)
  list(df = dplyr::as_tibble(df), title = "Field Comments")
}
