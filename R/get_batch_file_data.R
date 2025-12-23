#' @title Download and combine PTAGIS MRR files
#'
#' @description
#' Convenience wrapper to download multiple PTAGIS mark–recapture–recovery (MRR)
#' files (JSON, XML, or legacy TXT/ASCII), parse them via \code{\link{get_file_data}},
#' and return combined \code{sessions} (one row per file) and \code{events} (all rows).
#'
#' If \code{pdvs = "attach"}, SPDV/PDV values are attached in wide format
#' (\code{spdv1}, \code{spdv2}, ... on \code{sessions}; \code{pdv1}, \code{pdv2}, ...
#' on \code{events}) and a per-file \code{pdv_map} is returned.
#'
#' If \code{map_pdvs = TRUE} (and \code{pdvs = "attach"}), SPDV/PDV code columns
#' are mapped to label-based columns using \code{pdv_map$label}, and the original
#' \code{spdv*}/\code{pdv*} code columns are removed. The \code{pdv_map} is preserved.
#'
#' @param filenames Character vector of MRR file names to download and process.
#'   Duplicate filenames are dropped (only the first occurrence is used).
#' @param pdvs Character; one of \code{"drop"} (default) or \code{"attach"}.
#'   If \code{"drop"}, PDV-related components are removed (faster/smaller output).
#'   If \code{"attach"}, PDV values are retained/attached and \code{pdv_map} is returned.
#' @param map_pdvs Logical; if \code{TRUE} and \code{pdvs = "attach"}, map \code{spdv*}/\code{pdv*}
#'   code columns to label-based columns using \code{pdv_map} and drop the code columns.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{sessions}: tibble with one row per file/session
#'   \item \code{events}: tibble with all event rows
#'   \item \code{pdv_map}: tibble mapping (only when \code{pdvs = "attach"})
#' }
#' If \code{map_pdvs = TRUE}, \code{sessions}/\code{events} include mapped label columns
#' and \code{spdv*}/\code{pdv*} code columns are removed.
#'
#' @author Mike Ackerman & Ryan Kinzer
#'
#' @export
get_batch_file_data <- function(filenames,
                                pdvs = c("drop", "attach"),
                                map_pdvs = FALSE) {

  pdvs <- match.arg(pdvs)

  if (!is.character(filenames) || !length(filenames)) {
    stop("`filenames` must be a non-empty character vector.", call. = FALSE)
  }

  # avoid double downloads i.e., confusing duplicates
  filenames <- unique(filenames)

  mrr_list <- download_mrr_files(
    filenames,
    drop_pdvs = (pdvs == "drop")
  )

  out <- collapse_mrr_list(mrr_list, pdvs = pdvs)

  if (isTRUE(map_pdvs) && pdvs == "attach") {
    out <- map_pdvs_to_cols(out)
  }

  out
}
