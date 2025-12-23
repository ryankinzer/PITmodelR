#' @title Download and Combine MRR Files
#'
#' @description
#' Convenience wrapper to download multiple PTAGIS MRR files (JSON/XML/TXT),
#' enforce schemas via \code{get_file_data()}, and return combined `sessions`
#' and `events`. Optionally attaches SPDV/PDV values (wide: spdv1.., pdv1..)
#' and returns a simple per-file mapping tibble.
#'
#' @param filenames Character vector of MRR file names to download and process.
#' @param pdvs Character; one of \code{"drop"} (default) or \code{"attach"}.
#'   If \code{"drop"}, PDV/SPDV structures are not downloaded/kept upstream (faster, simpler).
#'   If \code{"attach"}, PDV/SPDV values are retained and attached in the output and
#'   a mapping tibble is returned.
#'
#' @return A list with `sessions`, `events`, and (if \code{pdvs="attach"}) `mapping`.
#'
#' @author Mike Ackerman & Ryan Kinzer
#'
#' @export

get_batch_file_data <- function(filenames,
                                pdvs = c("drop", "attach")) {

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

  collapse_mrr_list(mrr_list, pdvs = pdvs)

}
