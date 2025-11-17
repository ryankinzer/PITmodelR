#' @title Retrieve File Info for Project Code and Year
#'
#' @description
#' Retrieve file information submitted by a
#' mark-recapture-recovery (MRR) project code within a given year.
#'
#' @param year A four digit year as an integer.
#'
#' @inheritParams get_site_observations
#' @inheritParams get_project_years
#'
#' @return A tibble containing metadata for submitted files.
#'
#' @author Ryan Kinzer
#'
#' @examples
#' \dontrun{ get_mrr_files(code = "LGR", year = 2024) }
#'
#' @export

get_mrr_files <- function(code,
                          year = NULL,
                          page = 1,
                          page_size = 1000,
                          all_pages = TRUE) {

  # ---- validate project code ----
  if (missing(code) || length(code) != 1 || !is.character(code) || is.na(code)) {
    stop("`code` must be a single, non-missing character string.", call. = FALSE)
  }
  code <- toupper(trimws(code))
  if (nchar(code) != 3) {
    stop("`code` must be exactly 3 characters (e.g., 'LGR').", call. = FALSE)
  }

  # ---- validate year ----
  if (!is.null(year)) {
    if (!is.numeric(year) || length(year) != 1L || is.na(year) ||
        nchar(as.character(as.integer(year))) != 4) {
      stop("`year` must be a single four-digit integer (e.g., 2024) or NULL.", call. = FALSE)
    }
    year <- as.integer(year)
  }

  # ---- validate pagination controls ----
  if (!is.logical(all_pages) || length(all_pages) != 1L || is.na(all_pages)) {
    stop("`all_pages` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.numeric(page) || length(page) != 1L || is.na(page) || page < 1) {
    stop("`page` must be a single positive integer.", call. = FALSE)
  }
  if (!is.numeric(page_size) || length(page_size) != 1L || is.na(page_size) || page_size < 1) {
    stop("`page_size` must be a single positive integer.", call. = FALSE)
  }

  # ---- message ----
  message("Downloading tag file information for project ", code,
          " and year ", year, " from PTAGIS...")

  # ---- construct URL and query parameters ----
  url <- paste0("files/mrr/sites/", code, "/year/", year)
  params <- list(
    pageSize   = page_size,
    pageNumber = page
  )

  # ---- fetch content ----
  content <- ptagis_GET(url, params)

  # ---- coerce to tibble ----
  tmp <- as_tibble_safely(content$model)

  return(tmp)
}
