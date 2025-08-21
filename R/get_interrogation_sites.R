#' List interrogation sites (metadata)
#' @param active_only logical; return only active/currently reporting sites.
#' @return A tibble of site metadata.
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
