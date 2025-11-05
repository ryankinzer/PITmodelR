#' Internal: HTTP GET with retries for PTAGIS API
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

  # try to parse JSON, but fall back to text if needed
  out <- try(httr::content(resp, as = "parsed", type = "application/json"), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out)) {
    out <- httr::content(resp, as = "text", encoding = "UTF-8")
  }
  out
}

#' @keywords internal
ltrim_slash <- function(x) sub("^/+", "", x)

#' @keywords internal
rtrim_slash <- function(x) sub("/+$", "", x)
