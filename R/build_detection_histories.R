#' @title Build Detection Histories from Observation Events
#'
#' @description
#' Converts raw PTAGIS observation events into long and wide detection histories
#' along a specified route. The long table records each detection event,
#' while the wide table summarizes whether each tag was detected at each site and
#' includes an encounter history string.
#'
#' @param observations A data.frame or tibble of PTAGIS observation events. Must include
#'   columns for tag codes, site codes, and event timestamps.
#' @param route_sites Character vector of site codes defining the downstream route order.
#'   Example: \code{c("LGR","LGS","LMN","MCN")}.
#' @param tag_col Character; name of the column in \code{observations} containing PIT tag codes.
#'   Default is \code{"tag_code"}.
#' @param time_col Character; name of the column in \code{observations} containing event timestamps
#'   (POSIXct or parseable). Default is \code{"event_time"}.
#' @param site_col Character; name of the column in \code{observations} containing site codes.
#'   Default is \code{"site_code"}.
#'
#' @return A list with two elements:
#' \itemize{
#'   \item \code{long} – a long-format table of detections with columns \code{tag}, \code{site}, and \code{time},
#'     ordered by tag, time, and site.
#'   \item \code{wide} – a wide-format table with one row per tag, columns for each site indicating
#'     presence/absence (0/1), and an \code{encounter_history} string summarizing detections along
#'     the route in order.
#' }
#' Both elements also carry a \code{"route_sites"} attribute with the ordered route used.
#'
#' @author Ryan Kinzer
#'
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

  # coerce site codes and time
  obs <- observations
  obs[[site_col]] <- toupper(trimws(as.character(obs[[site_col]])))
  obs <- obs[obs[[site_col]] %in% toupper(route_sites), , drop = FALSE]
  if (!inherits(obs[[time_col]], "POSIXt")) {
    # best-effort parse
    suppressWarnings(obs[[time_col]] <- as.POSIXct(obs[[time_col]], tz = "UTC"))
  }

  # keep only needed columns
  long <- obs[, c(tag_col, site_col, time_col), drop = FALSE]
  names(long) <- c("tag", "site", "time")

  # order
  o <- order(long$tag, long$time, long$site)
  long <- long[o, , drop = FALSE]

  # wide indicators per tag x site (ever detected = 1)
  tags <- unique(long$tag)
  rs <- toupper(route_sites)
  wide <- data.frame(tag = tags, stringsAsFactors = FALSE)
  for (s in rs) {
    wide[[s]] <- as.integer(wide$tag %in% unique(long$tag[long$site == s]))
  }
  # encounter history string in route order
  wide$encounter_history <- apply(wide[, rs, drop = FALSE], 1L, paste0, collapse = "")

  # attach route order for downstream functions
  attr(wide, "route_sites") <- rs
  attr(long, "route_sites") <- rs

  list(long = long, wide = wide)

}
