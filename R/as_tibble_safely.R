#' Internal: Coerce PTAGIS API content into a tibble
#'
#' @description
#' Converts parsed PTAGIS API responses into a tibble, handling multiple possible
#' formats consistently:
#' \itemize{
#'   \item{data.frame}{Converted directly to a tibble.}
#'   \item{list of records}{Each element (a list) becomes a row in the tibble.}
#'   \item{named list}{Converted to a single-row tibble.}
#'   \item{atomic vector}{Converted to a tibble with a single column named \code{value}.}
#' }
#'
#' @param x Object returned from \code{httr::content(..., as = "parsed")}.
#'
#' @return A tibble representing the content.
#'
#' @keywords internal

as_tibble_safely <- function(x) {
  out <- NULL

  if (is.null(x)) {
    out <- tibble::tibble()

  } else if (inherits(x, "data.frame")) {
    out <- tibble::as_tibble(x)

  } else if (is.list(x) && length(x) > 0 && all(vapply(x, is.list, logical(1)))) {
    # list of records (each element is itself a list)
    records <- lapply(x, tibble::as_tibble)
    # use vctrs to row-bind without dplyr
    out <- if (length(records)) {
      suppressWarnings(do.call(vctrs::vec_rbind, records))
    } else {
      tibble::tibble()
    }

  } else if (is.list(x)) {
    # named list (single record) or mixed list
    out <- tibble::as_tibble(x)

  } else {
    # atomic vector
    out <- tibble::tibble(value = x)
  }

  out
}
