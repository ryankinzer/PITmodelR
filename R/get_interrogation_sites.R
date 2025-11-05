#' @title List Interrogation Sites
#'
#' @description
#' Retrieves metadata for PTAGIS interrogation sites. The function can return either
#' all known sites or only those currently active/reporting, depending on the value
#' of \code{active_only}.
#'
#' @param active_only Logical; if \code{TRUE} (default), return only active or currently
#'   reporting interrogation sites. If \code{FALSE}, return all interrogation sites.
#'
#' @return A tibble containing interrogation site metadata.
#'
#' @author Ryan Kinzer
#'
#' @export

get_interrogation_sites <- function(active_only = TRUE) {

  # TODO: confirm actual path in Swagger UI
  path <- if (isTRUE(active_only)) {
    # e.g., "interrogation/sites/active" (placeholder)
    "sites/interrogation/active"
  } else {
    # e.g., "interrogation/sites" (placeholder)
    "sites/interrogation"
  }

  out <- ptagis_GET(path)
  tbl <- as_tibble_safely(out)

  if (nrow(tbl) == 0L) {
    warning("No interrogation sites returned.", call. = FALSE)
    return(tbl)
  } else {
    return(tbl)
  }
}
