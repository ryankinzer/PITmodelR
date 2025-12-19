#' Parse mixed-format datetime strings into POSIXct (UTC)
#'
#' @description
#' Robust datetime parser - always returns POSIXct (UTC), NA when not parseable.
#'
#'
#' @keywords internal
parse_datetime_mixed <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(as.POSIXct(character(), tz = "UTC"))
  }

  x <- as.character(x)
  x[!nzchar(x)] <- NA_character_

  # Normalize ISO offsets "-07:00" → "-0700"
  x_iso <- sub("([+-]\\d{2}):?(\\d{2})$", "\\1\\2", x, perl = TRUE)

  fmts <- c(
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%dT%H:%M:%S%z",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M:%S",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M"
  )

  out <- rep(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"), length(x_iso))

  todo <- is.na(out) & !is.na(x_iso)
  for (f in fmts) {
    if (!any(todo)) break
    tmp <- suppressWarnings(as.POSIXct(x_iso[todo], format = f, tz = "UTC"))
    hit <- !is.na(tmp)
    if (any(hit)) out[which(todo)[hit]] <- tmp[hit]
    todo <- is.na(out) & !is.na(x_iso)
  }

  out
}
