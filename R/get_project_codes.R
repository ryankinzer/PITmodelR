#' Download available PTAGIS project codes
#'
#' @return A character vector of available project codes (uppercase, sorted, unique).
#' @export
#' @examples
#' \dontrun{ get_project_codes() }
get_project_codes <- function() {
  message("Downloading available project codes...")
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
