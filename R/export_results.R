#' Export check results to an Excel workbook (IPA HFC format)
#'
#' Creates a multi-sheet Excel workbook matching IPA's professional
#' high-frequency checks output format. Sheets include a Dashboard summary,
#' Flagged Records master list, per-category detail sheets, a Corrections
#' template, and optional Enumerator Stats.
#'
#' @param report An `adc_report` object (from [run_all_checks()]) or a
#'   list of `check_result` objects.
#' @param data Optional original data frame. When provided, category sheets
#'   include full data rows for flagged records.
#' @param id_col Character. ID column name in `data`.
#' @param path Character. Output file path (must end in `.xlsx`).
#' @param enum_col Character. Optional enumerator column name. When provided,
#'   adds an "Enumerator Stats" sheet.
#' @param date_col Character. Optional date column name. When provided,
#'   includes submission dates in the Flagged Records sheet.
#' @param ... Reserved for future use.
#' @return Invisible path to the created file.
#' @export
export_to_excel <- function(report, data = NULL, id_col = NULL, path,
                            enum_col = NULL, date_col = NULL, ...) {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    cli::cli_abort("Install openxlsx2: {.code install.packages('openxlsx2')}")
  }

 # --- Extract check results --------------------------------------------------
  results <- .extract_results(report)
  summary_tbl <- bind_check_results(results)
  flagged_tbl <- .build_flagged_table_full(results, data, id_col,
                                            enum_col, date_col)

  # --- Styles ----------------------------------------------------------------
  blue_bg   <- openxlsx2::wb_color("4472C4")
  white_txt <- openxlsx2::wb_color("FFFFFF")
  red_bg    <- openxlsx2::wb_color("FF0000")
  orange_bg <- openxlsx2::wb_color("FFA500")
  info_bg   <- openxlsx2::wb_color("4472C4")

  # --- Create workbook -------------------------------------------------------
  wb <- openxlsx2::wb_workbook()

  # === Sheet 1: Dashboard ====================================================
  dash <- .build_dashboard(results, summary_tbl, flagged_tbl,
                           data, enum_col, date_col)
  wb$add_worksheet("Dashboard")
  wb$add_data("Dashboard", dash, col_names = FALSE)
  wb$set_col_widths("Dashboard", cols = 1, widths = 40)
  wb$set_col_widths("Dashboard", cols = 2, widths = 25)

  # Style section headers (rows where Value column is empty or NA)
  for (i in seq_len(nrow(dash))) {
    dims_row <- paste0("A", i, ":B", i)
    if (is.na(dash[[2]][i]) || dash[[2]][i] == "") {
      # Section header row
      wb$add_font("Dashboard", dims = dims_row, bold = TRUE,
                  color = white_txt)
      wb$add_fill("Dashboard", dims = dims_row, color = blue_bg)
    }
  }
  wb$freeze_pane("Dashboard", first_row = TRUE)

  # === Sheet 2: Flagged Records ==============================================
  wb$add_worksheet("Flagged Records")
  if (nrow(flagged_tbl) > 0) {
    # Sort: error > warning > info, then by check_name
    sev_order <- c(error = 1L, warning = 2L, info = 3L)
    flagged_tbl$`.sev_order` <- sev_order[flagged_tbl$severity]
    flagged_tbl <- flagged_tbl[order(flagged_tbl$`.sev_order`,
                                     flagged_tbl$check_name), ]
    flagged_tbl$`.sev_order` <- NULL

    wb$add_data("Flagged Records", flagged_tbl)
    .style_header(wb, "Flagged Records", ncol(flagged_tbl))
    .auto_widths(wb, "Flagged Records", flagged_tbl)

    # Color-code severity column
    sev_col_idx <- which(names(flagged_tbl) == "severity")
    if (length(sev_col_idx) == 1L) {
      sev_letter <- openxlsx2::int2col(sev_col_idx)
      for (r in seq_len(nrow(flagged_tbl))) {
        cell_dims <- paste0(sev_letter, r + 1L)
        sev_val <- flagged_tbl$severity[r]
        fill_color <- switch(sev_val,
                             error   = red_bg,
                             warning = orange_bg,
                             info    = info_bg,
                             NULL)
        if (!is.null(fill_color)) {
          wb$add_fill("Flagged Records", dims = cell_dims,
                      color = fill_color)
          wb$add_font("Flagged Records", dims = cell_dims,
                      color = white_txt, bold = TRUE)
        }
      }
    }

    wb$add_filter("Flagged Records",
                  rows = 1,
                  cols = seq_len(ncol(flagged_tbl)))
    wb$freeze_pane("Flagged Records", first_row = TRUE)
  } else {
    no_flags <- dplyr::tibble(message = "No flagged records found.")
    wb$add_data("Flagged Records", no_flags)
  }

  # === Sheet 3+: Per-category detail sheets ==================================
  if (nrow(flagged_tbl) > 0 && !is.null(data) && !is.null(id_col)) {
    categories <- unique(flagged_tbl$check_category)
    categories <- categories[!is.na(categories)]
    ids <- as_id(data[[id_col]])

    for (cat in categories) {
      sheet_name <- format_sheet_name(cat)

      cat_flags <- flagged_tbl[flagged_tbl$check_category == cat, , drop = FALSE]
      cat_flagged_ids <- unique(cat_flags$id)
      if (length(cat_flagged_ids) == 0L) next

      match_rows <- ids %in% as_id(cat_flagged_ids)
      if (!any(match_rows)) next

      cat_data <- data[match_rows, , drop = FALSE]

      # Merge flag info into data rows
      # An ID can have multiple flags, so we merge flags onto data
      flag_info <- cat_flags[, c("id", "check_name", "flag_reason", "severity"),
                             drop = FALSE]
      flag_info <- flag_info[!duplicated(paste(flag_info$id,
                                                flag_info$check_name)), ]

      cat_data_with_id <- cat_data
      cat_data_with_id$`.merge_id` <- as_id(cat_data_with_id[[id_col]])
      flag_info$`.merge_id` <- as_id(flag_info$id)

      out <- merge(cat_data_with_id, flag_info[, c(".merge_id", "check_name",
                                                    "flag_reason", "severity")],
                   by = ".merge_id", all.x = FALSE)
      out$`.merge_id` <- NULL

      # Reorder: flag columns first, then data columns
      flag_cols <- c("check_name", "flag_reason", "severity")
      data_cols <- setdiff(names(out), flag_cols)
      out <- out[, c(flag_cols, data_cols), drop = FALSE]

      wb$add_worksheet(sheet_name)
      wb$add_data(sheet_name, out)
      .style_header(wb, sheet_name, ncol(out))
      .auto_widths(wb, sheet_name, out)
      wb$add_filter(sheet_name, rows = 1, cols = seq_len(ncol(out)))
      wb$freeze_pane(sheet_name, first_row = TRUE)
    }
  }

  # === Sheet: Corrections ====================================================
  wb$add_worksheet("Corrections")
  corrections_df <- .build_corrections_template(flagged_tbl)
  wb$add_data("Corrections", corrections_df)
  .style_header(wb, "Corrections", ncol(corrections_df))
  .auto_widths(wb, "Corrections", corrections_df)

  # Data validation dropdown for action column
  action_col_idx <- which(names(corrections_df) == "action")
  if (length(action_col_idx) == 1L) {
    action_letter <- openxlsx2::int2col(action_col_idx)
    n_rows_corr <- max(nrow(corrections_df), 1L)
    # Apply validation to rows 2 through at least 100 (for future entries)
    max_row <- max(n_rows_corr + 1L, 100L)
    val_dims <- paste0(action_letter, "2:", action_letter, max_row)
    wb$add_data_validation(
      "Corrections",
      dims = val_dims,
      type = "list",
      value = '"replace,drop,okay,pending"'
    )
  }
  wb$freeze_pane("Corrections", first_row = TRUE)

  # === Sheet: Enumerator Stats (optional) ====================================
  if (!is.null(enum_col) && nrow(flagged_tbl) > 0 && !is.null(data)) {
    enum_stats <- .build_enumerator_stats(data, flagged_tbl, enum_col)
    if (!is.null(enum_stats) && nrow(enum_stats) > 0) {
      wb$add_worksheet("Enumerator Stats")
      wb$add_data("Enumerator Stats", enum_stats)
      .style_header(wb, "Enumerator Stats", ncol(enum_stats))
      .auto_widths(wb, "Enumerator Stats", enum_stats)
      wb$add_filter("Enumerator Stats",
                    rows = 1,
                    cols = seq_len(ncol(enum_stats)))
      wb$freeze_pane("Enumerator Stats", first_row = TRUE)

      # Conditional formatting: red fill on flag_rate > 0.20
      fr_col_idx <- which(names(enum_stats) == "flag_rate")
      if (length(fr_col_idx) == 1L) {
        fr_letter <- openxlsx2::int2col(fr_col_idx)
        for (r in seq_len(nrow(enum_stats))) {
          if (!is.na(enum_stats$flag_rate[r]) &&
              enum_stats$flag_rate[r] > 0.20) {
            cell_dims <- paste0(fr_letter, r + 1L)
            wb$add_fill("Enumerator Stats", dims = cell_dims,
                        color = red_bg)
            wb$add_font("Enumerator Stats", dims = cell_dims,
                        color = white_txt, bold = TRUE)
          }
        }
      }
    }
  }

  # --- Write to disk ---------------------------------------------------------
  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  cli::cli_alert_success("Excel report saved to {.file {path}}")

  invisible(path)
}


#' Export check results to CSV files (IPA HFC format)
#'
#' Writes multiple CSV files matching the IPA HFC output structure:
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
#' @param ... Reserved for future use.
#' @return Invisible list of file paths created.
#' @export
export_to_csv <- function(report, output_dir, prefix = "hfc_report",
                          data = NULL, id_col = NULL,
                          enum_col = NULL, date_col = NULL, ...) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_alert_info("Created output directory: {.file {output_dir}}")
  }

  results     <- .extract_results(report)
  summary_tbl <- bind_check_results(results)
  flagged_tbl <- .build_flagged_table_full(results, data, id_col,
                                            enum_col, date_col)

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
  if (!is.null(enum_col) && !is.null(data) && nrow(flagged_tbl) > 0) {
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


#' Build the full flagged records table (IPA format)
#'
#' Columns: enumerator, date, id, check_name, check_category, variable,
#' value, flag_reason, severity, action.
#'
#' @keywords internal
.build_flagged_table_full <- function(results, data = NULL, id_col = NULL,
                                      enum_col = NULL, date_col = NULL) {
  rows <- lapply(results, function(cr) {
    if (!is_check_result(cr) || cr$n_flagged == 0L) return(NULL)

    n <- cr$n_flagged
    flag_reasons <- if (length(cr$flag_reason) > 0) {
      cr$flag_reason
    } else {
      rep(NA_character_, n)
    }

    # Extract variable name from check_name (heuristic: after last underscore
    # pattern like D01_outlier_iqr_VARNAME)
    variable <- .extract_variable_from_check(cr$check_name)

    out <- dplyr::tibble(
      id             = cr$flagged_ids,
      check_name     = rep(cr$check_name, n),
      check_category = rep(cr$check_category, n),
      variable       = rep(variable, n),
      flag_reason    = flag_reasons,
      severity       = rep(cr$severity, n)
    )

    # Add value column: attempt to look up the variable value in data
    if (!is.null(data) && !is.null(id_col) && !is.null(variable) &&
        variable %in% names(data) && id_col %in% names(data)) {
      ids_lookup <- as_id(data[[id_col]])
      match_idx <- match(as_id(cr$flagged_ids), ids_lookup)
      out$value <- ifelse(is.na(match_idx), NA_character_,
                          as.character(data[[variable]][match_idx]))
    } else {
      out$value <- rep(NA_character_, n)
    }

    out
  })

  out <- dplyr::bind_rows(rows)

  if (nrow(out) == 0L) {
    return(dplyr::tibble(
      enumerator     = character(),
      date           = character(),
      id             = character(),
      check_name     = character(),
      check_category = character(),
      variable       = character(),
      value          = character(),
      flag_reason    = character(),
      severity       = character(),
      action         = character()
    ))
  }

  # Add enumerator column
  if (!is.null(enum_col) && !is.null(data) && !is.null(id_col) &&
      enum_col %in% names(data)) {
    ids_lookup <- as_id(data[[id_col]])
    match_idx <- match(as_id(out$id), ids_lookup)
    out$enumerator <- ifelse(is.na(match_idx), NA_character_,
                             as.character(data[[enum_col]][match_idx]))
  } else {
    out$enumerator <- rep(NA_character_, nrow(out))
  }

  # Add date column
  if (!is.null(date_col) && !is.null(data) && !is.null(id_col) &&
      date_col %in% names(data)) {
    ids_lookup <- as_id(data[[id_col]])
    match_idx <- match(as_id(out$id), ids_lookup)
    out$date <- ifelse(is.na(match_idx), NA_character_,
                       as.character(data[[date_col]][match_idx]))
  } else {
    out$date <- rep(NA_character_, nrow(out))
  }

  # Empty action column for user to fill
  out$action <- rep(NA_character_, nrow(out))

  # Reorder columns to match IPA format
  col_order <- c("enumerator", "date", "id", "check_name", "check_category",
                 "variable", "value", "flag_reason", "severity", "action")
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
      add_row("Date range", paste(min(dates), "to", max(dates)))
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
    # Take first 3 flags as example rows
    n_examples <- min(3L, nrow(flagged_tbl))
    examples <- flagged_tbl[seq_len(n_examples), , drop = FALSE]

    example_rows <- dplyr::tibble(
      id        = examples$id,
      variable  = if ("variable" %in% names(examples)) examples$variable else NA_character_,
      old_value = if ("value" %in% names(examples)) examples$value else NA_character_,
      new_value = rep(NA_character_, n_examples),
      action    = rep("pending", n_examples),
      reason    = if ("flag_reason" %in% names(examples)) examples$flag_reason else NA_character_
    )

    template <- dplyr::bind_rows(template, example_rows)
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
  out$flag_rate <- round(out$n_flags / out$n_surveys, 4)

  # Sort by flag_rate descending
  out <- out[order(-out$flag_rate), ]

  # Final column order
  out <- out[, c("enumerator", "n_surveys", "n_flags", "flag_rate",
                 "n_errors", "n_warnings"), drop = FALSE]
  rownames(out) <- NULL

  out
}


#' Extract a variable name from a check_name string
#'
#' Heuristic: for checks like "D01_outlier_iqr_income", extracts "income".
#' For checks like "A01_duplicate_id", returns NA.
#'
#' @keywords internal
.extract_variable_from_check <- function(check_name) {
  # Pattern: prefix_method_VARNAME (3+ underscored parts after the code)
  parts <- strsplit(check_name, "_", fixed = TRUE)[[1]]
  if (length(parts) >= 4L) {
    # Variable is everything after the third part
    return(paste(parts[4:length(parts)], collapse = "_"))
  }
  NA_character_
}


#' Style header row with blue background and white bold text
#' @keywords internal
.style_header <- function(wb, sheet, ncols) {
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
  # Estimate width: max of header length and a reasonable default
  widths <- vapply(seq_len(ncols), function(i) {
    header_len <- nchar(names(df)[i])
    max_data <- if (nrow(df) > 0) {
      vals <- nchar(as.character(df[[i]]))
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
