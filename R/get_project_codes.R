#' @title Download PTAGIS Project Codes
#'
#' @description Download a character vector of project codes available in PTAGIS. Project
#' codes are used to identify the mark-recapture-recovery (MRR) project under which the
#' data was submitted. More information on available project codes can be found
#' [here](https://www.ptagis.org/Resources/ValidationCodes?domainFilter=MrrProject).
#'
#' @author Ryan Kinzer
#'
#' @export
#' @return A character vector of available project codes (uppercase, sorted, unique).
#' @examples
#' \dontrun{ get_project_codes() }

get_project_codes <- function() {

  message("Downloading available project codes from PTAGIS...")
  content <- ptagis_GET("files/mrr/projects")

  # content may be a character vector or a list of objects with code fields
  codes <- NULL
  if (is.character(content)) {
    codes <- content
  } else if (is.list(content)) {
    codes <- vapply(content, function(x) x, character(1))
  }

  codes <- unique(stats::na.omit(codes))

  if (!length(codes)) {
    warning("No project codes returned by PTAGIS.", call. = FALSE)
    return(character())
  }

  sort(unique(toupper(trimws(codes))))
}
