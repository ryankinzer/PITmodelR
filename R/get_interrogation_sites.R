#' @title List Interrogation Sites
#'
#' @description
#' Retrieves metadata for interrogation sites from PTAGIS. The function can return either
#' all registered sites (past and present) or only those active and contributing data.
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

  # --- set URL to API endpoint ---
  path <- if (isTRUE(active_only)) {
    "sites/interrogation/active"
  } else {
    "sites/interrogation"
  }

  # --- retrieve ---
  out <- ptagis_GET(path)
  tbl <- as_tibble_safely(out)

  # --- convert column names to snake_case ---
  if (ncol(tbl) > 0) {
    nm <- names(tbl)
    nm <- gsub("\\.", "_", nm)
    nm <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", nm)
    names(tbl) <- tolower(nm)
  }

  # --- warning message if no rows returned ---
  if (nrow(tbl) == 0L) {
    warning("No interrogation sites returned.", call. = FALSE)
    return(tbl)
  } else {
    return(tbl)
  }
}
