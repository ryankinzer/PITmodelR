#' Internal: Coerce PTAGIS API content into a tibble
#'
#' Ensures JSON parsed responses are always a tibble:
#' - data.frame      -> tibble
#' - list of records -> rows bound into a tibble
#' - named list      -> 1-row tibble
#' - atomic vector   -> tibble with a single `value` column
#'
#' @param x Object from httr::content(..., as = "parsed").
#' @return tibble
#' @keywords internal
as_tibble_safely <- function(x) {
  out <- NULL

  if (is.null(x)) {
    out <- tibble::tibble()

  } else if (inherits(x, "data.frame")) {
    out <- tibble::as_tibble(x)

  } else if (is.list(x) && length(x) > 0 && all(vapply(x, is.list, logical(1)))) {
    # List of records (each element is itself a list)
    records <- lapply(x, tibble::as_tibble)
    # Use vctrs to row-bind without dplyr
    out <- if (length(records)) {
      suppressWarnings(do.call(vctrs::vec_rbind, records))
    } else {
      tibble::tibble()
    }

  } else if (is.list(x)) {
    # Named list (single record) or mixed list
    out <- tibble::as_tibble(x)

  } else {
    # Atomic vector
    out <- tibble::tibble(value = x)
  }

  out
}
