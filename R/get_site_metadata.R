#' Get interrogation site metadata
#'
#' Fetch metadata for a single PTAGIS interrogation site.
#'
#' @param site_code PTAGIS site code as a single string (e.g., "LGR").
#' @return A tibble (usually one row) with site metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' get_site_metadata("LGR")
#' }
get_site_metadata <- function(site_code) {
  # ---- validate ----
  if (missing(site_code) || !is.character(site_code) || length(site_code) != 1L ||
      is.na(site_code) || !nzchar(site_code)) {
    stop("`site_code` must be a single, non-missing string (e.g., 'LGR').", call. = FALSE)
  }
  site_code <- toupper(trimws(site_code))

  # ---- endpoint ----
  # Confirm path in PTAGIS Swagger; many APIs use `interrogation/sites/{code}`
  path <- paste0("sites/interrogation/", site_code)

  message("Downloading site metadata for ", site_code, " ...")
  out <- ptagis_GET(path)

  tbl <- as_tibble_safely(out)

  if (nrow(tbl) == 0L) {
    warning("No metadata returned for site '", site_code, "'.", call. = FALSE)
    return(tbl)
  }

  # ---- clean names ----
  nm <- names(tbl)
  nm <- gsub("\\.", "_", nm)
  nm <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", nm)
  names(tbl) <- tolower(nm)

  tbl
}
