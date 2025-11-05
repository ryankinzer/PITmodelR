#' @title Fit Migration Timing Metrics (Placeholder)
#'
#' @description
#' Computes daily counts and requested quantiles of passage based on event timestamps.
#' Currently supports simple daily aggregation; more advanced methods (e.g., GAM smoothing)
#' are accepted as arguments but return the same structure in this placeholder.
#'
#' @param observations Data frame or tibble of observation events containing a timestamp column.
#' @param time_col Character; name of the column containing event timestamps (POSIXct or parseable). Default: "event_time".
#' @param by Character; aggregation unit for counts. Currently only "day" is supported. Default: "day".
#' @param quantiles Numeric vector of quantiles to compute, in the range [0, 1]. Default: c(0.1, 0.5, 0.9).
#' @param method Character; method to compute timing metrics. Options: "quantile" (default) or "gam" (placeholder, returns same structure).
#'
#' @return A list with components:
#' \describe{
#'   \item{\code{daily}}{Data frame with columns \code{date} (Date of the aggregation unit),
#'     \code{n} (Number of events on that date),
#'     \code{cum_n} (Cumulative number of events through that date),
#'     \code{prop} (Cumulative proportion of total events through that date).}
#'   \item{\code{quantiles}}{Data frame of requested quantiles with columns \code{q} (Requested quantile),
#'     \code{date} (Date corresponding to the quantile).}
#'   \item{\code{method}}{Character indicating which method was used ("quantile" or "gam").}
#' }
#'
#' @author Ryan Kinzer
#'
#' @export

fit_timing <- function(observations,
                       time_col = "event_time",
                       by = "day",
                       quantiles = c(0.1, 0.5, 0.9),
                       method = c("quantile", "gam")) {
  stopifnot(is.data.frame(observations))
  method <- match.arg(method)
  if (!time_col %in% names(observations)) {
    stop("`observations` is missing column: ", time_col, call. = FALSE)
  }
  dat <- observations
  # Parse/ensure POSIXct
  if (!inherits(dat[[time_col]], "POSIXt")) {
    suppressWarnings(dat[[time_col]] <- as.POSIXct(dat[[time_col]], tz = "UTC"))
  }
  if (any(is.na(dat[[time_col]]))) {
    dat <- dat[!is.na(dat[[time_col]]), , drop = FALSE]
  }
  if (nrow(dat) == 0) {
    return(list(daily = data.frame(date = as.Date(character()), n = integer(), cum_n = integer(), prop = numeric()),
                quantiles = data.frame(q = numeric(), date = as.Date(character()))))
  }

  if (identical(by, "day")) {
    dates <- as.Date(dat[[time_col]])
    tab <- as.data.frame(table(dates), stringsAsFactors = FALSE)
    names(tab) <- c("date", "n")
    tab$date <- as.Date(tab$date)
  } else {
    stop("Only by = 'day' is supported in the placeholder.", call. = FALSE)
  }

  tab <- tab[order(tab$date), , drop = FALSE]
  tab$cum_n <- cumsum(tab$n)
  tab$prop <- tab$cum_n / max(tab$cum_n)

  # Quantile dates by linear interpolation on cumulative proportion
  qs <- sort(unique(pmax(0, pmin(1, quantiles))))
  q_dates <- rep(NA, length(qs))
  for (i in seq_along(qs)) {
    q <- qs[i]
    # Find first date where prop >= q
    idx <- which(tab$prop >= q)[1]
    if (!is.na(idx)) {
      q_dates[i] <- tab$date[idx]
    } else {
      q_dates[i] <- NA
    }
  }
  qdf <- data.frame(q = qs, date = as.Date(q_dates, origin = "1970-01-01"))

  list(daily = tab, quantiles = qdf, method = method)
}
