# ---- Internal helper: Haversine distance in km ----

#' Compute Haversine distance between two points
#' @param lat1 Latitude of point 1 (degrees)
#' @param lon1 Longitude of point 1 (degrees)
#' @param lat2 Latitude of point 2 (degrees)
#' @param lon2 Longitude of point 2 (degrees)
#' @return Distance in kilometres
#' @keywords internal
haversine_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  dlat <- (lat2 - lat1) * pi / 180
  dlon <- (lon2 - lon1) * pi / 180
  a <- sin(dlat / 2)^2 +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
  2 * R * asin(sqrt(a))
}


# ---- E.03: GPS accuracy ----

#' Check GPS accuracy
#'
#' E.03: Flag GPS readings with accuracy greater than a threshold.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param accuracy_col Character. Name of the GPS accuracy column (metres)
#' @param max_accuracy Numeric. Maximum acceptable accuracy in metres (default 50)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_gps_accuracy <- function(data, id_col, accuracy_col,
                                max_accuracy = 50, ...) {
  assert_columns(data, c(id_col, accuracy_col), context = "check_gps_accuracy")
  assert_numeric(data, accuracy_col, context = "check_gps_accuracy")

  ids <- as_id(data[[id_col]])
  vals <- data[[accuracy_col]]
  n_total <- sum(!is.na(vals))

  flagged_mask <- !is.na(vals) & vals > max_accuracy
  flagged_ids <- ids[flagged_mask]
  flagged_vals <- vals[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0("GPS accuracy = ", round(flagged_vals[i], 1),
           "m exceeds threshold of ", max_accuracy, "m")
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "E03_gps_accuracy",
    check_category = "gps",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      accuracy_col = accuracy_col,
      max_accuracy = max_accuracy,
      median_accuracy = stats::median(vals, na.rm = TRUE),
      max_observed = if (n_total > 0) max(vals, na.rm = TRUE) else NA_real_
    )
  )
}


# ---- E.04: GPS duplicates ----

#' Check GPS duplicate coordinates
#'
#' E.04: Flag GPS coordinates that are identical or within a minimum distance
#' of each other across different survey IDs.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param lat_col Character. Name of the latitude column
#' @param lon_col Character. Name of the longitude column
#' @param min_distance Numeric. Minimum distance in degrees below which points
#'   are considered duplicates (default 0.0001, approx 11m)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_gps_duplicates <- function(data, id_col, lat_col, lon_col,
                                  min_distance = 0.0001, ...) {
  assert_columns(data, c(id_col, lat_col, lon_col),
                 context = "check_gps_duplicates")

  ids <- as_id(data[[id_col]])
  lats <- data[[lat_col]]
  lons <- data[[lon_col]]

  # Only consider rows with non-NA GPS

  valid <- !is.na(lats) & !is.na(lons)
  n_total <- sum(valid)

  if (n_total < 2L) {
    return(new_check_result(
      check_name     = "E04_gps_duplicates",
      check_category = "gps",
      n_flagged      = 0L,
      n_total        = as.integer(n_total),
      flagged_ids    = character(),
      flag_reason    = character(),
      severity       = "warning"
    ))
  }

  v_ids  <- ids[valid]
  v_lats <- lats[valid]
  v_lons <- lons[valid]

  # Simple pairwise check for near-duplicates

  flagged_set <- character()
  reasons <- character()

  for (i in seq_len(length(v_ids) - 1L)) {
    for (j in (i + 1L):length(v_ids)) {
      if (abs(v_lats[i] - v_lats[j]) < min_distance &&
          abs(v_lons[i] - v_lons[j]) < min_distance) {
        if (!(v_ids[i] %in% flagged_set)) {
          flagged_set <- c(flagged_set, v_ids[i])
          reasons <- c(reasons,
                       paste0("Near-duplicate GPS with ID ", v_ids[j]))
        }
        if (!(v_ids[j] %in% flagged_set)) {
          flagged_set <- c(flagged_set, v_ids[j])
          reasons <- c(reasons,
                       paste0("Near-duplicate GPS with ID ", v_ids[i]))
        }
      }
    }
  }

  new_check_result(
    check_name     = "E04_gps_duplicates",
    check_category = "gps",
    n_flagged      = length(flagged_set),
    n_total        = as.integer(n_total),
    flagged_ids    = flagged_set,
    flag_reason    = reasons,
    severity       = "warning",
    summary_stat   = list(
      min_distance = min_distance,
      n_valid_gps  = n_total
    )
  )
}


# ---- E.07: Null island / default GPS ----

#' Check GPS null island and default values
#'
#' E.07: Flag GPS coordinates at (0, 0), or with lat/lon equal to known
#' default or placeholder values (0, 999, -999, 9999).
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param lat_col Character. Name of the latitude column
#' @param lon_col Character. Name of the longitude column
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_gps_null_island <- function(data, id_col, lat_col, lon_col, ...) {
  assert_columns(data, c(id_col, lat_col, lon_col),
                 context = "check_gps_null_island")

  ids <- as_id(data[[id_col]])
  lats <- data[[lat_col]]
  lons <- data[[lon_col]]
  n_total <- sum(!is.na(lats) | !is.na(lons))

  defaults <- c(0, 999, -999, 9999)

  lat_default <- !is.na(lats) & lats %in% defaults
  lon_default <- !is.na(lons) & lons %in% defaults
  flagged_mask <- lat_default | lon_default

  flagged_ids <- ids[flagged_mask]
  flagged_lats <- lats[flagged_mask]
  flagged_lons <- lons[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0("GPS appears to be default/null island: lat=",
           flagged_lats[i], ", lon=", flagged_lons[i])
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "E07_gps_null_island",
    check_category = "gps",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      n_lat_default = sum(lat_default),
      n_lon_default = sum(lon_default)
    )
  )
}


# ---- E.01: GPS boundary ----

#' Check GPS boundary
#'
#' E.01: Flag GPS points that fall outside a specified boundary. Accepts
#' either a bounding box (named list) or an sf polygon. Falls back to
#' valid geographic range \[-90, 90\] for lat and \[-180, 180\] for lon.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param lat_col Character. Name of the latitude column
#' @param lon_col Character. Name of the longitude column
#' @param boundary_bbox Named list with xmin, xmax, ymin, ymax (lon/lat)
#' @param boundary_sf An sf polygon object for point-in-polygon checks
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_gps_boundary <- function(data, id_col, lat_col, lon_col,
                                boundary_bbox = NULL, boundary_sf = NULL, ...) {
  assert_columns(data, c(id_col, lat_col, lon_col),
                 context = "check_gps_boundary")

  ids <- as_id(data[[id_col]])
  lats <- data[[lat_col]]
  lons <- data[[lon_col]]
  valid <- !is.na(lats) & !is.na(lons)
  n_total <- sum(valid)

  if (!is.null(boundary_sf)) {
    # Point-in-polygon using sf
    if (!requireNamespace("sf", quietly = TRUE)) {
      cli::cli_abort(
        "Package {.pkg sf} is required for boundary_sf checks but is not installed."
      )
    }
    pts <- sf::st_as_sf(
      data[valid, , drop = FALSE],
      coords = c(lon_col, lat_col),
      crs = 4326
    )
    within <- sf::st_within(pts, boundary_sf, sparse = FALSE)
    inside <- apply(within, 1, any)
    flagged_mask <- rep(FALSE, nrow(data))
    flagged_mask[valid] <- !inside
  } else if (!is.null(boundary_bbox)) {
    # Bounding box check
    flagged_mask <- valid &
      (lons < boundary_bbox$xmin | lons > boundary_bbox$xmax |
       lats < boundary_bbox$ymin | lats > boundary_bbox$ymax)
  } else {
    # Global validity check
    flagged_mask <- valid &
      (lats < -90 | lats > 90 | lons < -180 | lons > 180)
  }

  flagged_ids <- ids[flagged_mask]
  flagged_lats <- lats[flagged_mask]
  flagged_lons <- lons[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0("GPS outside boundary: lat=", round(flagged_lats[i], 5),
           ", lon=", round(flagged_lons[i], 5))
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "E01_gps_boundary",
    check_category = "gps",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      method = if (!is.null(boundary_sf)) "sf_polygon"
               else if (!is.null(boundary_bbox)) "bounding_box"
               else "global_range",
      boundary_bbox = boundary_bbox
    )
  )
}


# ---- E.02: GPS centroid distance ----

#' Check GPS distance from cluster centroid
#'
#' E.02: Flag GPS points that are more than a specified distance from their
#' cluster centroid. Uses the Haversine formula.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param lat_col Character. Name of the latitude column
#' @param lon_col Character. Name of the longitude column
#' @param cluster_col Character. Name of the cluster/EA column
#' @param centroid_data Optional data frame with pre-computed centroids.
#'   Must contain columns matching cluster_col, lat_col, and lon_col.
#' @param max_distance_km Numeric. Maximum distance in km from centroid (default 10)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_gps_centroid_distance <- function(data, id_col, lat_col, lon_col,
                                         cluster_col,
                                         centroid_data = NULL,
                                         max_distance_km = 10, ...) {
  assert_columns(data, c(id_col, lat_col, lon_col, cluster_col),
                 context = "check_gps_centroid_distance")

  ids <- as_id(data[[id_col]])
  lats <- data[[lat_col]]
  lons <- data[[lon_col]]
  clusters <- data[[cluster_col]]
  valid <- !is.na(lats) & !is.na(lons) & !is.na(clusters)
  n_total <- sum(valid)

  if (n_total == 0L) {
    return(new_check_result(
      check_name     = "E02_gps_centroid_distance",
      check_category = "gps",
      n_flagged      = 0L,
      n_total        = 0L,
      flagged_ids    = character(),
      flag_reason    = character(),
      severity       = "warning"
    ))
  }

  # Build centroid lookup
  if (!is.null(centroid_data)) {
    assert_columns(centroid_data, c(cluster_col, lat_col, lon_col),
                   context = "check_gps_centroid_distance (centroid_data)")
    centroid_lat <- stats::setNames(centroid_data[[lat_col]],
                                    as.character(centroid_data[[cluster_col]]))
    centroid_lon <- stats::setNames(centroid_data[[lon_col]],
                                    as.character(centroid_data[[cluster_col]]))
  } else {
    # Compute centroids from the data
    valid_df <- data.frame(
      cluster = as.character(clusters[valid]),
      lat     = lats[valid],
      lon     = lons[valid],
      stringsAsFactors = FALSE
    )
    agg_lat <- stats::aggregate(lat ~ cluster, data = valid_df, FUN = mean)
    agg_lon <- stats::aggregate(lon ~ cluster, data = valid_df, FUN = mean)
    centroid_lat <- stats::setNames(agg_lat$lat, agg_lat$cluster)
    centroid_lon <- stats::setNames(agg_lon$lon, agg_lon$cluster)
  }

  # Compute distance for each valid row
  flagged_ids_out <- character()
  flag_reason_out <- character()

  for (idx in which(valid)) {
    cl <- as.character(clusters[idx])
    if (!(cl %in% names(centroid_lat))) next
    dist_km <- haversine_km(lats[idx], lons[idx],
                            centroid_lat[[cl]], centroid_lon[[cl]])
    if (dist_km > max_distance_km) {
      flagged_ids_out <- c(flagged_ids_out, ids[idx])
      flag_reason_out <- c(flag_reason_out,
                           paste0(round(dist_km, 2),
                                  "km from cluster ", cl, " centroid",
                                  " (max ", max_distance_km, "km)"))
    }
  }

  new_check_result(
    check_name     = "E02_gps_centroid_distance",
    check_category = "gps",
    n_flagged      = length(flagged_ids_out),
    n_total        = as.integer(n_total),
    flagged_ids    = flagged_ids_out,
    flag_reason    = flag_reason_out,
    severity       = "warning",
    summary_stat   = list(
      max_distance_km = max_distance_km,
      n_clusters      = length(centroid_lat)
    )
  )
}


# ---- E.05: GPS swap detection ----

#' Check for swapped latitude and longitude
#'
#' E.05: Detect cases where latitude and longitude appear to be swapped.
#' If an expected bounding box is provided, checks whether swapping the
#' coordinates would place the point inside the expected area. Without
#' a bounding box, flags rows where abs(lat) > 90 (impossible value that
#' may be a longitude).
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param lat_col Character. Name of the latitude column
#' @param lon_col Character. Name of the longitude column
#' @param expected_bbox Optional named list with lat_min, lat_max, lon_min,
#'   lon_max defining the expected study area
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_gps_swap <- function(data, id_col, lat_col, lon_col,
                            expected_bbox = NULL, ...) {
  assert_columns(data, c(id_col, lat_col, lon_col),
                 context = "check_gps_swap")

  ids <- as_id(data[[id_col]])
  lats <- data[[lat_col]]
  lons <- data[[lon_col]]
  valid <- !is.na(lats) & !is.na(lons)
  n_total <- sum(valid)

  flagged_mask <- rep(FALSE, nrow(data))

  if (!is.null(expected_bbox)) {
    for (idx in which(valid)) {
      lat <- lats[idx]
      lon <- lons[idx]
      in_bbox <- lat >= expected_bbox$lat_min & lat <= expected_bbox$lat_max &
                 lon >= expected_bbox$lon_min & lon <= expected_bbox$lon_max
      if (!in_bbox) {
        # Check if swapped coordinates would be inside
        swapped_in <- lon >= expected_bbox$lat_min & lon <= expected_bbox$lat_max &
                      lat >= expected_bbox$lon_min & lat <= expected_bbox$lon_max
        if (swapped_in) {
          flagged_mask[idx] <- TRUE
        }
      }
    }
  } else {
    # Without bbox: flag impossible latitudes (abs > 90)
    flagged_mask <- valid & (abs(lats) > 90)
  }

  flagged_ids <- ids[flagged_mask]
  flagged_lats <- lats[flagged_mask]
  flagged_lons <- lons[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    paste0("Possible lat/lon swap: lat=", round(flagged_lats[i], 5),
           ", lon=", round(flagged_lons[i], 5))
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "E05_gps_swap",
    check_category = "gps",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "error",
    summary_stat   = list(
      expected_bbox = expected_bbox,
      method = if (!is.null(expected_bbox)) "bbox_swap_test" else "impossible_latitude"
    )
  )
}


# ---- E.08: GPS clustering / fabrication ----

#' Check excessive GPS clustering
#'
#' E.08: Flag cases where too many surveys share the exact same GPS
#' coordinates (rounded to 4 decimal places, approximately 11m), which
#' may indicate data fabrication.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param lat_col Character. Name of the latitude column
#' @param lon_col Character. Name of the longitude column
#' @param enum_col Character or NULL. Enumerator ID column. If provided,
#'   only flags clusters where all surveys are from the same enumerator.
#' @param max_same_point Integer. Maximum number of surveys allowed at the
#'   same GPS point before flagging (default 3)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_gps_clustering <- function(data, id_col, lat_col, lon_col,
                                  enum_col = NULL, max_same_point = 3, ...) {
  required <- c(id_col, lat_col, lon_col)
  if (!is.null(enum_col)) required <- c(required, enum_col)
  assert_columns(data, required, context = "check_gps_clustering")

  ids <- as_id(data[[id_col]])
  lats <- data[[lat_col]]
  lons <- data[[lon_col]]
  valid <- !is.na(lats) & !is.na(lons)
  n_total <- sum(valid)

  if (n_total == 0L) {
    return(new_check_result(
      check_name     = "E08_gps_clustering",
      check_category = "gps",
      n_flagged      = 0L,
      n_total        = 0L,
      flagged_ids    = character(),
      flag_reason    = character(),
      severity       = "warning"
    ))
  }

  # Round GPS to 4 decimal places (~11m precision)
  gps_key <- paste0(round(lats[valid], 4), "_", round(lons[valid], 4))
  v_ids <- ids[valid]
  v_enum <- if (!is.null(enum_col)) as_id(data[[enum_col]][valid]) else NULL

  # Group by rounded GPS key
  groups <- split(seq_along(gps_key), gps_key)

  flagged_ids_out <- character()
  flag_reason_out <- character()

  for (key in names(groups)) {
    grp_idx <- groups[[key]]
    if (length(grp_idx) <= max_same_point) next

    # If enum_col provided, only flag when all from same enumerator
    if (!is.null(v_enum)) {
      enums_in_group <- unique(v_enum[grp_idx])
      if (length(enums_in_group) > 1L) next
    }

    grp_ids <- v_ids[grp_idx]
    reason <- paste0(length(grp_idx), " surveys at same GPS point (",
                     key, "), max allowed = ", max_same_point)
    flagged_ids_out <- c(flagged_ids_out, grp_ids)
    flag_reason_out <- c(flag_reason_out, rep(reason, length(grp_ids)))
  }

  new_check_result(
    check_name     = "E08_gps_clustering",
    check_category = "gps",
    n_flagged      = length(flagged_ids_out),
    n_total        = as.integer(n_total),
    flagged_ids    = flagged_ids_out,
    flag_reason    = flag_reason_out,
    severity       = "warning",
    summary_stat   = list(
      max_same_point = max_same_point,
      n_gps_groups   = length(groups),
      enum_filter    = !is.null(enum_col)
    )
  )
}


# ---- E.10: GPS altitude ----

#' Check GPS altitude plausibility
#'
#' E.10: Flag altitude values outside a plausible range.
#'
#' @param data Data frame of survey submissions
#' @param id_col Character. Name of the primary key column
#' @param altitude_col Character. Name of the GPS altitude column (metres)
#' @param min_alt Numeric. Minimum plausible altitude in metres (default -500)
#' @param max_alt Numeric. Maximum plausible altitude in metres (default 6000)
#' @param ... Reserved for future use
#' @return A check_result object
#' @export
check_gps_altitude <- function(data, id_col, altitude_col,
                                min_alt = -500, max_alt = 6000, ...) {
  assert_columns(data, c(id_col, altitude_col), context = "check_gps_altitude")
  assert_numeric(data, altitude_col, context = "check_gps_altitude")

  ids <- as_id(data[[id_col]])
  vals <- data[[altitude_col]]
  n_total <- sum(!is.na(vals))

  below <- !is.na(vals) & vals < min_alt
  above <- !is.na(vals) & vals > max_alt
  flagged_mask <- below | above

  flagged_ids <- ids[flagged_mask]
  flagged_vals <- vals[flagged_mask]
  flagged_below <- below[flagged_mask]

  flag_reason <- vapply(seq_along(flagged_ids), function(i) {
    v <- flagged_vals[i]
    if (flagged_below[i]) {
      paste0("Altitude = ", v, "m below minimum ", min_alt, "m")
    } else {
      paste0("Altitude = ", v, "m above maximum ", max_alt, "m")
    }
  }, character(1), USE.NAMES = FALSE)

  new_check_result(
    check_name     = "E10_gps_altitude",
    check_category = "gps",
    n_flagged      = length(flagged_ids),
    n_total        = as.integer(n_total),
    flagged_ids    = flagged_ids,
    flag_reason    = flag_reason,
    severity       = "warning",
    summary_stat   = list(
      altitude_col = altitude_col,
      min_alt      = min_alt,
      max_alt      = max_alt,
      actual_min   = if (n_total > 0) min(vals, na.rm = TRUE) else NA_real_,
      actual_max   = if (n_total > 0) max(vals, na.rm = TRUE) else NA_real_
    )
  )
}
