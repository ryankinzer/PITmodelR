#' Build detection histories from observation events
#'
#' Converts raw observation events into long/wide detection histories over a specified route.
#'
#' @param observations data.frame/tibble of PTAGIS observations.
#' @param route_sites character vector of site codes defining the downstream route order (e.g., c("LGR","LGS","LMN","MCN")).
#' @param tag_col name of the column containing PIT tag codes.
#' @param time_col name of the column containing event timestamps (POSIXct or parseable).
#' @param site_col name of the column containing site codes.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{long}: long table of (tag, site, time), ordered.
#'   \item \code{wide}: one row per tag with 0/1 detection indicators per route site and encounter history string.
#' }
#' @export
build_detection_histories <- function(observations,
                                      route_sites,
                                      tag_col = "tag_code",
                                      time_col = "event_time",
                                      site_col = "site_code") {
  stopifnot(is.data.frame(observations))
  if (length(route_sites) < 2) stop("`route_sites` must have at least 2 sites.", call. = FALSE)
  if (!all(c(tag_col, time_col, site_col) %in% names(observations))) {
    stop("`observations` is missing one of: ", paste(c(tag_col, time_col, site_col), collapse = ", "), call. = FALSE)
  }
  # Coerce site codes and time
  obs <- observations
  obs[[site_col]] <- toupper(trimws(as.character(obs[[site_col]])))
  obs <- obs[obs[[site_col]] %in% toupper(route_sites), , drop = FALSE]
  if (!inherits(obs[[time_col]], "POSIXt")) {
    # best-effort parse
    suppressWarnings(obs[[time_col]] <- as.POSIXct(obs[[time_col]], tz = "UTC"))
  }
  # Keep only needed columns
  long <- obs[, c(tag_col, site_col, time_col), drop = FALSE]
  names(long) <- c("tag", "site", "time")
  # Order
  o <- order(long$tag, long$time, long$site)
  long <- long[o, , drop = FALSE]

  # Wide indicators per tag x site (ever detected = 1)
  tags <- unique(long$tag)
  rs <- toupper(route_sites)
  wide <- data.frame(tag = tags, stringsAsFactors = FALSE)
  for (s in rs) {
    wide[[s]] <- as.integer(wide$tag %in% unique(long$tag[long$site == s]))
  }
  # Encounter history string in route order
  wide$encounter_history <- apply(wide[, rs, drop = FALSE], 1L, paste0, collapse = "")

  # Attach route order for downstream functions
  attr(wide, "route_sites") <- rs
  attr(long, "route_sites") <- rs

  list(long = long, wide = wide)
}
