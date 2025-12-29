#' @title Retrieve Years for a PTAGIS Project
#'
#' @description
#' Retrieves the years for which data were submitted for a given
#' PTAGIS project code.
#'
#' @param code A single PTAGIS project code (3 characters), e.g. "LGR".
#'
#' @return An integer vector of available years (sorted, unique).
#'
#' @author Ryan Kinzer
#'
#' @examples
#' \dontrun{ get_project_years("LGR") }
#'
#' @export
get_project_years <- function(code) {

  # ---- validate code ----
  if (missing(code) || length(code) != 1 || !is.character(code) || is.na(code)) {
    stop("`code` must be a single, non-missing character string.", call. = FALSE)
  }
  code <- toupper(trimws(code))
  if (nchar(code) != 3) {
    stop("`code` must be exactly 3 characters (e.g., 'LGR').", call. = FALSE)
  }

  # ---- message ----
  message("Downloading available years for project ", code, " from PTAGIS...")

  # ---- fetch content ----
  content <- ptagis_GET(paste0("files/mrr/projects/", code))

  # ---- parse years ----
  years <- NULL
  if (is.numeric(content)) {
    years <- as.integer(content)
  } else if (is.list(content)) {
    years <- vapply(content, function(x) x, integer(1))
  }

  # ---- clean years ----
  years <- unique(stats::na.omit(years))

  if (!length(years)) {
    warning("No years returned for project '", code, "'.", call. = FALSE)
    return(integer())
  }

  # ---- sort ----
  sort(years)
}
