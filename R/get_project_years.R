#' @title Download Years for a PTAGIS Project
#'
#' @description Download the years for which data are available for a PTAGIS
#' project code.
#'
#' @author Ryan Kinzer
#'
#' @param code A single PTAGIS project code (3 characters), e.g. "LGR".
#'
#' @export
#' @return An integer vector of available years (sorted, unique).
#' @examples
#' \dontrun{ get_project_years("LGR") }
#'
get_project_years <- function(code) {

  if (missing(code) || length(code) != 1 || !is.character(code) || is.na(code))
    stop("`code` must be a single, non-missing character string.", call. = FALSE)
  code <- toupper(trimws(code))
  if (nchar(code) != 3)
    stop("`code` must be exactly 3 characters (e.g., 'LGR').", call. = FALSE)

  message("Downloading available years for project ", code, " from PTAGIS...")
  content <- ptagis_GET(paste0("files/mrr/projects/", code))

  years <- NULL
  if (is.numeric(content)) {
    years <- as.integer(content)
  } else if (is.list(content)) {
    years <- vapply(content, function(x) x, integer(1))
  }

  years <- unique(stats::na.omit(years))

  if (!length(years)) {
    warning("No years returned for project '", code, "'.", call. = FALSE)
    return(integer())
  }

  sort(years)
}
