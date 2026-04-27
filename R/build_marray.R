#' @title Create a Burnham-style M-array
#'
#' @description
#' Builds an M-array of releases, downstream first detections, removals, and
#' never-seen-again counts from encounter histories.
#'
#' @details
#' Encounter history coding:
#' \itemize{
#'   \item \code{1}: detected and released
#'   \item \code{2}: detected but removed/censored after detection
#'   \item \code{0}: not observed
#' }
#'
#' Fish coded \code{2} are counted as observed at that occasion but are not counted
#' as released from that occasion.
#'
#' @param ch_data Data frame with at least \code{tag_code} and \code{ch}.
#' @param locs_def Optional character vector of occasion labels.
#'
#' @return Data frame with release occasion, site, observed, released, M-array
#'   first-detection counts, and never-seen-again counts.
#'
#' @author Ryan N. Kinzer
#'
#' @export
build_marray <- function(ch_data,
                         locs_def = NULL) {

  stopifnot(is.data.frame(ch_data), all(c("tag_code", "ch") %in% names(ch_data)))

  ch_vec <- as.character(ch_data$ch)
  n_occ <- unique(nchar(ch_vec))

  if (length(n_occ) != 1L) {
    stop("All encounter histories must have the same length.", call. = FALSE)
  }

  if (is.null(locs_def)) {
    locs_def <- paste0("site_", seq_len(n_occ))
  }

  if (length(locs_def) != n_occ) {
    stop("locs_def must have the same length as the encounter histories.", call. = FALSE)
  }

  ch_mat <- do.call(rbind, strsplit(ch_vec, ""))
  storage.mode(ch_mat) <- "integer"

  observed <- colSums(ch_mat %in% c(1L, 2L))
  released <- colSums(ch_mat == 1L)

  m_array <- matrix(
    0L,
    nrow = n_occ - 1,
    ncol = n_occ,
    dimnames = list(
      release = paste0("R_", seq_len(n_occ - 1)),
      recapture = c(paste0("m_", 2:n_occ), "never")
    )
  )

  for (r in seq_len(nrow(ch_mat))) {

    x <- ch_mat[r, ]
    release_occs <- which(x == 1L)
    release_occs <- release_occs[release_occs < n_occ]

    for (i in release_occs) {

      later <- which(seq_along(x) > i & x %in% c(1L, 2L))

      if (length(later)) {
        m_array[paste0("R_", i), paste0("m_", later[1])] <-
          m_array[paste0("R_", i), paste0("m_", later[1])] + 1L
      } else {
        m_array[paste0("R_", i), "never"] <-
          m_array[paste0("R_", i), "never"] + 1L
      }
    }
  }

  data.frame(
    release = paste0("R_", seq_len(n_occ - 1)),
    site = locs_def[seq_len(n_occ - 1)],
    observed = observed[seq_len(n_occ - 1)],
    released = released[seq_len(n_occ - 1)],
    as.data.frame(m_array, check.names = FALSE),
    row.names = NULL
  )
}
