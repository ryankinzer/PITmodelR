#' @title Get Observations for a Single Interrogation Site
#'
#' @description
#' Retrieves PIT-tag observation events for a specified PTAGIS interrogation site.
#' Optionally filters by year and can automatically fetch all available pages
#' of data from the API.
#'
#' @param year Optional four-digit integer year (e.g., \code{2024}) used as a filter
#'   when supported by the API.
#' @param page,page_size Integers controlling pagination when \code{all_pages = FALSE}.
#' @param all_pages Logical; if \code{TRUE} (default), the function automatically
#'   retrieves and combines all pages of results.
#'
#' @inheritParams get_site_metadata
#' @inheritParams get_tag_history
#'
#' @return A tibble containing PIT-tag observation events for the requested site
#'   (and year, if specified). May be zero rows if no data are available.
#'
#' @author Ryan Kinzer
#'
#' @export

get_site_observations <- function(api_key = NULL,
                                  site_code,
                                  year = NULL,
                                  page = 1,
                                  page_size = 1000,
                                  all_pages = TRUE,
                                  fields = NULL) {
  # ---- resolve / validate api_key ----
  if (is.null(api_key) || !nzchar(api_key)) {
    api_key <- Sys.getenv("PTAGIS_API_KEY", unset = NA_character_)
  }
  if (!nzchar(api_key %||% "")) {
    stop("Missing PTAGIS API key. Pass `api_key` or set PTAGIS_API_KEY env var.", call. = FALSE)
  }
  if (!is.character(api_key) || length(api_key) != 1L || is.na(api_key)) {
    stop("`api_key` must be a single, non-missing string.", call. = FALSE)
  }

  # ---- validate site_code ----
  if (missing(site_code) || !is.character(site_code) || length(site_code) != 1L ||
      is.na(site_code) || !nzchar(site_code)) {
    stop("`site_code` must be a single, non-missing string (e.g., 'LGR').", call. = FALSE)
  }
  site_code <- toupper(trimws(site_code))

  # ---- validate year (optional) ----
  if (!is.null(year)) {
    if (!is.numeric(year) || length(year) != 1L || is.na(year) || nchar(as.character(as.integer(year))) != 4) {
      stop("`year` must be a single four-digit integer (e.g., 2024) or NULL.", call. = FALSE)
    }
    year <- as.integer(year)
  }

  # ---- validate paging controls ----
  if (!is.logical(all_pages) || length(all_pages) != 1L || is.na(all_pages)) {
    stop("`all_pages` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.numeric(page) || length(page) != 1L || is.na(page) || page < 1) {
    stop("`page` must be a single positive integer.", call. = FALSE)
  }
  if (!is.numeric(page_size) || length(page_size) != 1L || is.na(page_size) || page_size < 1) {
    stop("`page_size` must be a single positive integer.", call. = FALSE)
  }
  if (!is.null(fields) && (!is.character(fields) || anyNA(fields))) {
    stop("`fields` must be a character vector (or NULL).", call. = FALSE)
  }

  # ---- endpoint & base query ----
  # Confirm the exact path/params in PTAGIS Swagger.
  #path <- paste0("interrogation/sites/", site_code, "/observations")
  path <- paste0("data/events/observation/site/", site_code, "/year/", year)

  base_query <- list(
    apiKey   = api_key,   # PTAGIS commonly allows apiKey as query
    page     = NULL,      # filled in below
    pageSize = page_size
  )
  if (!is.null(year)) {
    # If the API expects a different param name (e.g., `observationYear`, `year`), adjust here.
    base_query$year <- year
  }

  message("Downloading observations for site ", site_code,
          if (!is.null(year)) paste0(" in ", year) else "",
          " from PTAGIS...")

  fetch_page <- function(p) {
    q <- base_query
    q$page <- p
    out <- ptagis_GET(path, query = q)
    as_tibble_safely(out)
  }

  if (isTRUE(all_pages)) {
    # Page until empty/short final page
    p <- as.integer(page)
    acc <- list()
    repeat {
      tbl <- fetch_page(p)
      if (nrow(tbl) == 0L) break
      acc[[length(acc) + 1L]] <- tbl
      if (nrow(tbl) < page_size) break
      p <- p + 1L
    }
    res <- if (length(acc)) dplyr::bind_rows(acc) else tibble::tibble()
  } else {
    res <- fetch_page(as.integer(page))
  }

  # ---- clean names (snake_case) ----
  if (ncol(res) > 0) {
    nm <- names(res)
    nm <- gsub("\\.", "_", nm)
    nm <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", nm)
    names(res) <- tolower(nm)
  }

  # ---- optional column selection ----
  if (!is.null(fields) && ncol(res) > 0) {
    keep <- intersect(tolower(fields), names(res))
    if (!length(keep)) {
      warning("None of the requested `fields` were found; returning all columns.", call. = FALSE)
    } else {
      res <- res[, keep, drop = FALSE]
    }
  }

  if (nrow(res) == 0L) {
    warning("No observations returned for site '", site_code,
            if (!is.null(year)) paste0("' in ", year) else "'",
            ".", call. = FALSE)
  }

  res
}
