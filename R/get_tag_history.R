#' @title Get Observation Events for a Single PIT Tag
#'
#' @description
#' Retrieves the full PTAGIS observation (event) history for a single PIT tag.
#' The request uses the PTAGIS Events API endpoint and returns a cleaned
#' tibble with snake_case column names. Optional field selection can be used
#' to reduce the returned payload.
#'
#' @param api_key Optional PTAGIS API key. If omitted, the function attempts to use
#'   the \code{PTAGIS_API_KEY} environment variable. Must be a single non-empty string.
#' @param tag_code Character string containing a single PIT tag code (e.g.,
#'   \code{"384.1B79726A98"}). Case-insensitive; leading/trailing whitespace is trimmed.
#' @param fields Optional character vector of column names to retain after name
#'   cleaning. If provided, only matching columns are returned.
#'
#' @return A tibble of observation events for the requested PIT tag. May be zero rows
#'   if no events are available.
#'
#' @author Ryan Kinzer
#'
#' @examples
#' \dontrun{
#' # recommended: store your key once per session
#' Sys.setenv(PTAGIS_API_KEY = "YOUR-KEY-HERE")
#' get_tag_history(tag_code = "384.1B79726A98")
#'
#' # or pass the key manually
#' get_tag_history(api_key = "YOUR-KEY-HERE", tag_code = "384.1B79726A98",
#'                 fields = c("event_time", "site_code", "antenna"))
#' }
#'
#' @export

get_tag_history <- function(api_key = NULL, tag_code, fields = NULL) {
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

  # ---- validate tag_code ----
  if (missing(tag_code) || !is.character(tag_code) || length(tag_code) != 1L || is.na(tag_code) || !nzchar(tag_code)) {
    stop("`tag_code` must be a single, non-missing string.", call. = FALSE)
  }
  tag_code <- toupper(trimws(tag_code))

  # Best-effort sanity check:  (hex with optional 384. prefix e.g., 384.1B..., or plain hex)
  # Don't be overly strict; allow dots and hex characters.
  if (!grepl("^[0-9A-F.]+$", tag_code)) {
    warning("`tag_code` contains characters outside 0-9, A-F, or '.'; verify the format.", call. = FALSE)
  }

  # ---- request ----
  # PTAGIS example: /data/events?apiKey=...&tagCode=...
  path  <- "data/events"
  query <- list(apiKey = api_key, tagCode = tag_code)

  message("Downloading observation events for tag ", tag_code, "...")
  out <- ptagis_GET(path, query = query)

  # ---- coerce to tibble ----
  tbl <- as_tibble_safely(out)

  # ---- normalize names to snake_case ----
  if (ncol(tbl) > 0) {
    nm <- names(tbl)
    nm <- gsub("\\.", "_", nm)
    nm <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", nm)
    names(tbl) <- tolower(nm)
  }

  # ---- optional column selection ----
  if (!is.null(fields)) {
    if (!is.character(fields) || anyNA(fields)) {
      stop("`fields` must be a character vector of column names (or NULL).", call. = FALSE)
    }
    keep <- intersect(tolower(fields), names(tbl))
    if (!length(keep)) {
      warning("None of the requested `fields` found; returning all columns.", call. = FALSE)
    } else {
      tbl <- tbl[, keep, drop = FALSE]
    }
  }

  # ---- final ----
  if (nrow(tbl) == 0L) {
    warning("No events returned for tag '", tag_code, "'.", call. = FALSE)
  }
  tbl
}
