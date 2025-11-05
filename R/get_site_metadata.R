#' @title Get Metadata for a Single Interrogation Site
#'
#' @description
#' Retrieves PTAGIS metadata for a single interrogation site identified by its
#' PTAGIS site code (e.g., \code{"LGR"}). Returns a tibble containing site-level
#' information as provided by the PTAGIS API.
#'
#' @param site_code Character string giving the PTAGIS site code to query. Must be
#'   a single, non-empty value (e.g., \code{"LGR"}).
#'
#' @return A tibble containing interrogation site metadata, typically one row.
#'
#' @author Ryan Kinzer
#'
#' @export
#'
get_site_metadata <- function(site_code) {

  # validate site_code
  if (missing(site_code) || !is.character(site_code) || length(site_code) != 1L ||
      is.na(site_code) || !nzchar(site_code)) {
    stop("`site_code` must be a single, non-missing string (e.g., 'LGR').", call. = FALSE)
  }

  site_code <- toupper(trimws(site_code))

  # endpoint
  # confirm path in PTAGIS Swagger; many APIs use `interrogation/sites/{code}`
  path <- paste0("interrogation/sites/", site_code)

  message("Downloading site metadata for ", site_code, " from PTAGIS...")
  out <- ptagis_GET(path)

  tbl <- as_tibble_safely(out)

  if (nrow(tbl) == 0L) {
    warning("No metadata returned for site '", site_code, "'.", call. = FALSE)
    return(tbl)
  }

  # clean names
  nm <- names(tbl)
  nm <- gsub("\\.", "_", nm)
  nm <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", nm)
  names(tbl) <- tolower(nm)

  tbl
}
