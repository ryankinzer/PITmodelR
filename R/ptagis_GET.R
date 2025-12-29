#' @title HTTP GET with retries for PTAGIS API
#'
#' @description
#' Performs a GET request to the PTAGIS API with retries and user-agent handling.
#' Automatically stops on common client errors (400, 401, 403, 404) and attempts
#' to parse JSON, falling back to raw text if parsing fails.
#'
#' @param path Character; endpoint path (e.g., "data/events").
#' @param query Named list of query parameters (default empty list).
#' @param base_url Base URL for PTAGIS API (default: "https://api.ptagis.org").
#' @param ... Additional arguments passed to \code{httr::RETRY}.
#'
#' @return Parsed JSON content if successful, otherwise raw text content.
#'
#' @author Ryan Kinzer
#'
#' @keywords internal
ptagis_GET <- function(path, query = list(), base_url = "https://api.ptagis.org", ...) {

  stopifnot(is.character(path), length(path) == 1L)
  url <- paste0(rtrim_slash(base_url), "/", ltrim_slash(path))

  # build user-agent and retry
  ua <- httr::user_agent("PITmodelR (R package); contact maintainer for issues")
  resp <- httr::RETRY(
    "GET", url, ua, query = query,
    times = 3, pause_base = 1,
    terminate_on = c(400, 401, 403, 404)
  )
  httr::stop_for_status(resp)

  # safe multi-encoding function
  decode_with_fallback <- function(raw_bytes) {
    txt <- rawToChar(raw_bytes)

    # try UTF-8
    try_utf8 <- iconv(txt, from = "", to = "UTF-8")
    if (!any(is.na(try_utf8))) return(try_utf8)

    # if UTF-8 fails, try Latin-1 / Windows-1252
    try_latin1 <- iconv(txt, from = "latin1", to = "UTF-8")
    if (!any(is.na(try_latin1))) return(try_latin1)

    # last resort: keep the original with substitution chars
    iconv(txt, from = "ASCII", to = "UTF-8", sub = "?")
  }

  # try to parse JSON, but fall back to text if needed
  out <- try(httr::content(resp, as = "parsed", type = "application/json"), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out)) {
    # use decode_with_fallback for safer encoding
    raw_txt <- httr::content(resp, as = "raw")
    out     <- decode_with_fallback(raw_txt)
    #out <- httr::content(resp, as = "text", encoding = "UTF-8")
  }
  out
}


#' @keywords internal
ltrim_slash <- function(x) sub("^/+", "", x)

#' @keywords internal
rtrim_slash <- function(x) sub("/+$", "", x)
