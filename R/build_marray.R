#' @title Create an M-array of releases, detections, and removals.
#'
#' @description
#' Builds a matrix of marks released, observed, and never seen.
#' @param ch_data Data frame or tibble with at least the following columns:
#'   \describe{
#'     \item{\code{tag_code}}{Unique identifier for each tagged individual.}
#'     \item{\code{ch}}{Encounter history string (e.g., "11001"). The value
#'     1 = released/detected, 0 = not observed, and a 2 = detected but removed and not available for observation at the next site.}
#'   }
#' @param locs_def Either a character vector defining the ordered occasions
#'   (e.g., \code{c("SECTRP","ZEN",...)}), or a named list mapping one or more site codes
#'   to occasions
#'   (e.g., \code{list(LGR = c("GRJ","GRS"), Down = c("LMN","MCN","BON"))}).
#'
#' @return a data.frame
#' @author Ryan N. Kinzer
#'
#' @export
build_marray <- function(ch_data,
                         locs_def = NULL) {

  ch_vec <- ch_data$ch
  n_occ <- nchar(ch_vec[1])

  if (is.null(locs_def)) {
    locs_def <- paste0("site_", seq_len(n_occ))
  }

  n_observed <- integer(n_occ)
  n_released <- integer(n_occ)

  m_array <- matrix(
    0L,
    nrow = n_occ - 1,
    ncol = n_occ,
    dimnames = list(
      release = paste0("R_", seq_len(n_occ - 1)),
      recapture = c(paste0("m_", 2:n_occ), "never")
    )
  )

  for (h in ch_vec) {

    x <- as.integer(strsplit(h, "")[[1]])

    n_observed <- n_observed + as.integer(x %in% c(1L, 2L))
    n_released <- n_released + as.integer(x == 1L)

    rel <- which(x == 1L)
    rel <- rel[rel < n_occ]

    for (i in rel) {

      later <- which(seq_along(x) > i & x %in% c(1L, 2L))

      if (length(later) > 0) {
        j <- later[1]

        m_array[paste0("R_", i), paste0("m_", j)] <-
          m_array[paste0("R_", i), paste0("m_", j)] + 1L

      } else {

        m_array[paste0("R_", i), "never"] <-
          m_array[paste0("R_", i), "never"] + 1L
      }
    }
  }

  out <- data.frame(
    release = paste0("R_", seq_len(n_occ - 1)),
    site = locs_def[seq_len(n_occ - 1)],
    observed = n_observed[seq_len(n_occ - 1)],
    released = n_released[seq_len(n_occ - 1)],
    as.data.frame(m_array, check.names = FALSE),
    row.names = NULL
  )

  out
}
