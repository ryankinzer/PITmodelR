#' @title Retrieve PTAGIS Project Codes
#'
#' @description
#' Retrieves a character vector of all mark-recapture-recovery (MRR)
#' project codes currently available in the [PTAGIS](https://www.ptagis.org) database.
#' Project codes identify the MRR project under which PIT-tag data were submitted.
#'
#' For a full list of available project codes with names, years, and status, see
#' the [PTAGIS Validation Codes page](https://www.ptagis.org/Resources/ValidationCodes?domainFilter=MrrProject).
#'
#' @return A character vector of available project codes (uppercase, sorted, unique).
#'
#' @author Ryan Kinzer
#'
#' @examples
#' \dontrun{ get_project_codes() }
#'
#' @export

get_project_codes <- function() {

  # ---- message to user ----
  message("Downloading available project codes from PTAGIS...")

  # ---- fetch content from PTAGIS ----
  content <- ptagis_GET("files/mrr/projects")

  # ---- parse codes ----
  codes <- NULL
  if (is.character(content)) {
    codes <- content
  } else if (is.list(content)) {
    codes <- vapply(content, function(x) x, character(1))
  }

  # ---- clean codes ----
  codes <- unique(stats::na.omit(codes))

  if (!length(codes)) {
    warning("No project codes returned by PTAGIS.", call. = FALSE)
    return(character())
  }

  # ---- normalize codes ----
  sort(unique(toupper(trimws(codes))))
}
