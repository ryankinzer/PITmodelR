#' Internal: HTTP GET with retries for PTAGIS
#'
#' @param path A character string of the endpoint path (e.g., "data/events")
#' @param query A named list of the possible query parameters
#' @param base_url The base API URL for PTAGIS API ("api.ptagis.org")
#' @return Parsed http request application/json or text
#' @keywords internal
ptagis_GET <- function(path, query = list(), base_url = "https://api.ptagis.org", ...) {
  stopifnot(is.character(path), length(path) == 1L)
  url <- paste0(rtrim_slash(base_url), "/", ltrim_slash(path))

  # Build user-agent and retry
  ua <- httr::user_agent("PITmodelR (R package); https://github.com/ryankinzer/PITmodelR/")
  resp <- httr::RETRY(
    "GET", url, ua, query = query,
    times = 3, pause_base = 1,
    terminate_on = c(400, 401, 403, 404)
  )
  httr::stop_for_status(resp)

  # Try to parse JSON, but fall back to text if needed
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
