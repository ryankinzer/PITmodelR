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
#' @return A list with:
#' \itemize{
#'   \item \code{sessions} – tibble of one row per file/session (optionally with spdv* cols)
#'   \item \code{events} – tibble of all event rows (optionally with pdv* cols)
#'   \item \code{mapping} – tibble mapping (file_name, level, pdv_column, label, definition)
#'     (empty if \code{pdvs="drop"} or if PDV structures are unavailable)
#' }
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

  mrr_list <- download_mrr_files(filenames, drop_pdvs = (pdvs == "drop"))

  collapse_mrr_list(mrr_list, pdvs = pdvs)

}
