#' Normalize survey data to standard column names
#'
#' Detects or accepts a survey platform and renames platform-specific columns
#' to a standard set: .id, .enum, .date, .start, .end, .duration, .latitude,
#' .longitude. Computes .duration from .start and .end if both exist.
#'
#' @param data Data frame of raw survey submissions
#' @param platform Character. One of "auto", "kobo", "surveycto", "odk",
#'   "survey_solutions", "cspro". Default "auto" detects from column names.
#' @param id_col Character or NULL. Explicit column name for primary ID
#' @param enum_col Character or NULL. Explicit column name for enumerator
#' @param date_col Character or NULL. Explicit column name for submission date
#' @param start_col Character or NULL. Explicit column name for start time
#' @param end_col Character or NULL. Explicit column name for end time
#' @param gps_col Character or NULL. Explicit column name for GPS data
#' @param ... Reserved for future use
#' @return A data frame with standard columns prepended, class "normalized_survey"
#' @importFrom rlang %||%
#' @export
normalize_survey_data <- function(data,
                                   platform = c("auto", "kobo", "surveycto",
                                                 "odk", "survey_solutions",
                                                 "cspro"),
                                   id_col = NULL,
                                   enum_col = NULL,
                                   date_col = NULL,
                                   start_col = NULL,
                                   end_col = NULL,
                                   gps_col = NULL,
                                   ...) {
  platform <- match.arg(platform)
  cols <- names(data)


  # --- Detect platform if auto ---
  if (platform == "auto") {
    platform <- detect_platform(data)
    if (platform == "unknown") {
      cli::cli_warn(c(
        "Could not auto-detect survey platform.",
        "i" = "Specify {.arg platform} explicitly or provide column mappings."
      ))
    }
  }

  # --- Get platform defaults ---
  defaults <- platform_defaults(platform, cols)

  # --- Resolve each mapping: user override > platform default ---
  id_col    <- id_col    %||% defaults$id
  enum_col  <- enum_col  %||% defaults$enum
  date_col  <- date_col  %||% defaults$date
  start_col <- start_col %||% defaults$start
  end_col   <- end_col   %||% defaults$end
  gps_col   <- gps_col   %||% defaults$gps

  # --- Build standard columns ---
  result <- data

  result$.id <- if (!is.null(id_col) && id_col %in% cols) {
    as_id(data[[id_col]])
  } else {
    NA_character_
  }

  result$.enum <- if (!is.null(enum_col) && enum_col %in% cols) {
    as.character(data[[enum_col]])
  } else {
    NA_character_
  }

  result$.date <- if (!is.null(date_col) && date_col %in% cols) {
    as.Date(data[[date_col]])
  } else {
    as.Date(NA)
  }

  result$.start <- if (!is.null(start_col) && start_col %in% cols) {
    as.POSIXct(data[[start_col]], tz = "UTC")
  } else {
    as.POSIXct(NA, tz = "UTC")
  }

  result$.end <- if (!is.null(end_col) && end_col %in% cols) {
    as.POSIXct(data[[end_col]], tz = "UTC")
  } else {
    as.POSIXct(NA, tz = "UTC")
  }

  # --- Parse GPS ---
  if (!is.null(gps_col) && gps_col %in% cols) {
    gps_parsed <- parse_gps_string(data[[gps_col]])
    result$.latitude  <- gps_parsed$latitude
    result$.longitude <- gps_parsed$longitude
  } else {
    # Try separate lat/lon columns from defaults
    lat_col <- defaults$latitude
    lon_col <- defaults$longitude
    result$.latitude <- if (!is.null(lat_col) && lat_col %in% cols) {
      as.numeric(data[[lat_col]])
    } else {
      NA_real_
    }
    result$.longitude <- if (!is.null(lon_col) && lon_col %in% cols) {
      as.numeric(data[[lon_col]])
    } else {
      NA_real_
    }
  }

  # --- Compute duration ---
  has_start <- !all(is.na(result$.start))
  has_end   <- !all(is.na(result$.end))
  if (has_start && has_end) {
    result$.duration <- as.numeric(difftime(result$.end, result$.start, units = "mins"))
  } else {
    result$.duration <- NA_real_
  }

  # --- Reorder: standard columns first ---
  std_cols <- c(".id", ".enum", ".date", ".start", ".end", ".duration",
                ".latitude", ".longitude")
  other_cols <- setdiff(names(result), std_cols)
  result <- result[, c(std_cols, other_cols), drop = FALSE]

  # --- Attach class and metadata ---
  attr(result, "platform") <- platform
  class(result) <- c("normalized_survey", class(result))
  result
}

#' Detect survey platform from column names
#'
#' Inspects column names to guess the originating platform.
#'
#' @param data Data frame
#' @return Character string: one of "kobo", "surveycto", "odk",
#'   "survey_solutions", "cspro", or "unknown"
#' @keywords internal
detect_platform <- function(data) {
  cols <- names(data)

  # KoBo: typically has _id, _submission_time, _submitted_by

  if ("_id" %in% cols && "_submission_time" %in% cols) {
    return("kobo")
  }

  # SurveyCTO: KEY, submissiondate
  if ("KEY" %in% cols && "submissiondate" %in% cols) {
    return("surveycto")
  }

  # ODK Central: __id, __system/submissionDate
  if (any(grepl("^__id$", cols)) && any(grepl("^__system", cols))) {
    return("odk")
  }

  # Survey Solutions: interview__id, interview__key
  if ("interview__id" %in% cols || "interview__key" %in% cols) {
    return("survey_solutions")
  }

  # CSPro: guid or case_id
  if ("guid" %in% cols || "case_id" %in% cols) {
    return("cspro")
  }

  "unknown"
}

#' Get platform-specific default column mappings
#'
#' @param platform Character. Platform name
#' @param cols Character vector. Available column names in the data
#' @return Named list of default column mappings
#' @keywords internal
platform_defaults <- function(platform, cols) {
  defaults <- switch(platform,
    kobo = list(
      id    = "_id",
      enum  = first_match(c("_submitted_by", "username"), cols),
      date  = "_submission_time",
      start = first_match(c("start"), cols),
      end   = first_match(c("end"), cols),
      gps   = first_match(grep("_gps$", cols, value = TRUE), cols),
      latitude  = first_match(c("_geolocation_latitude", "_gps_latitude"), cols),
      longitude = first_match(c("_geolocation_longitude", "_gps_longitude"), cols)
    ),
    surveycto = list(
      id    = "KEY",
      enum  = first_match(c("username", grep("^enumerator", cols, value = TRUE)), cols),
      date  = "submissiondate",
      start = first_match(c("starttime", "start"), cols),
      end   = first_match(c("endtime", "end"), cols),
      gps   = NULL,
      latitude  = first_match(grep("latitude", cols, value = TRUE, ignore.case = TRUE), cols),
      longitude = first_match(grep("longitude", cols, value = TRUE, ignore.case = TRUE), cols)
    ),
    odk = list(
      id    = first_match(c("__id", grep("^__id$", cols, value = TRUE)), cols),
      enum  = first_match(c("__system/submitterName"), cols),
      date  = first_match(c("__system/submissionDate"), cols),
      start = first_match(c("start"), cols),
      end   = first_match(c("end"), cols),
      gps   = NULL,
      latitude  = NULL,
      longitude = NULL
    ),
    survey_solutions = list(
      id    = first_match(c("interview__id"), cols),
      enum  = first_match(c("responsible"), cols),
      date  = first_match(c("date", grep("^date", cols, value = TRUE)), cols),
      start = NULL,
      end   = NULL,
      gps   = NULL,
      latitude  = NULL,
      longitude = NULL
    ),
    cspro = list(
      id    = first_match(c("guid", "case_id"), cols),
      enum  = first_match(c("operator", "supervisor"), cols),
      date  = NULL,
      start = NULL,
      end   = NULL,
      gps   = NULL,
      latitude  = NULL,
      longitude = NULL
    ),
    # unknown / fallback
    list(
      id = NULL, enum = NULL, date = NULL, start = NULL, end = NULL,
      gps = NULL, latitude = NULL, longitude = NULL
    )
  )
  defaults
}

#' Find the first matching column name
#'
#' @param candidates Character vector of candidate column names
#' @param cols Character vector of available column names
#' @return The first candidate found in cols, or NULL if none match
#' @keywords internal
first_match <- function(candidates, cols) {
  hit <- intersect(candidates, cols)
  if (length(hit) > 0) hit[1] else NULL
}

#' Parse GPS string into structured components
#'
#' Parses the "lat lon alt accuracy" space-separated format commonly used
#' by KoBo Toolbox and ODK into a tibble with named columns.
#'
#' @param gps_string Character vector. GPS strings in "lat lon alt accuracy" format
#' @return A tibble with columns: latitude, longitude, altitude, accuracy
#' @export
parse_gps_string <- function(gps_string) {
  n <- length(gps_string)

  latitude  <- rep(NA_real_, n)
  longitude <- rep(NA_real_, n)
  altitude  <- rep(NA_real_, n)
  accuracy  <- rep(NA_real_, n)

  valid <- !is.na(gps_string) & nzchar(trimws(gps_string))

  if (any(valid)) {
    parts <- strsplit(trimws(gps_string[valid]), "\\s+")

    for (i in seq_along(parts)) {
      p <- parts[[i]]
      idx <- which(valid)[i]
      if (length(p) >= 1) latitude[idx]  <- suppressWarnings(as.numeric(p[1]))
      if (length(p) >= 2) longitude[idx] <- suppressWarnings(as.numeric(p[2]))
      if (length(p) >= 3) altitude[idx]  <- suppressWarnings(as.numeric(p[3]))
      if (length(p) >= 4) accuracy[idx]  <- suppressWarnings(as.numeric(p[4]))
    }
  }

  dplyr::tibble(
    latitude  = latitude,
    longitude = longitude,
    altitude  = altitude,
    accuracy  = accuracy
  )
}
