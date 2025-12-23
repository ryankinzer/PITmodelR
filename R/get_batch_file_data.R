#' @title Download and Combine MRR Files
#'
#' @description
#' Convenience wrapper to download multiple PTAGIS MRR files (JSON/XML/TXT),
#' enforce schemas via \code{get_file_data()}, and return combined `sessions`
#' and `events`. If \code{pdvs="attach"}, SPDV/PDV values are attached in wide
#' format (spdv1.., pdv1..) and a per-file \code{pdv_map} is returned. Optionally,
#' PDV/SPDV code columns can be mapped to label-based columns using \code{pdv_map}.
#'
#' @param filenames Character vector of MRR file names to download and process.
#' @param pdvs Character; one of \code{"drop"} (default) or \code{"attach"}.
#'   If \code{"drop"}, PDV/SPDV structures are not downloaded/kept upstream.
#'   If \code{"attach"}, PDV/SPDV values are retained/attached and \code{pdv_map}
#'   is returned.
#' @param map_pdvs Logical; if TRUE and \code{pdvs="attach"}, map spdv*/pdv* code
#'   columns to label-based columns using \code{pdv_map}, and drop code columns.
#'
#' @return A list with \code{sessions} and \code{events}. If \code{pdvs="attach"},
#'   also returns \code{pdv_map}. If \code{map_pdvs=TRUE}, sessions/events will
#'   include mapped label columns and code columns will be removed.
#'
#' @author Mike Ackerman & Ryan Kinzer
#'
#' @export

get_batch_file_data <- function(
    filenames,
    pdvs = c("drop", "attach"),
    map_pdvs = FALSE
) {

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
